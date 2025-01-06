# Cài đặt thư viện cần thiết
install.packages("dplyr")
install.packages("geosphere")
install.packages("readxl")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("ROCR")
# Gọi thư viện cần thiết
library(carData)
library(car)
library(tools)
library(dplyr)
library(geosphere)
library(readxl)
library(ggplot2)
library(ggpubr)
library(ROCR)
# 1: Làm sạch file dirty_data
# 1.1: Đọc dữ liệu từ file dirty_data
dirty_data = read.csv("./dirty_data.csv")
View(dirty_data)
# 1.2: Tạo trường dataframe chỉ chứa những biến cần thiết
cleaning_data = dirty_data[,c("nearest_warehouse","order_price","delivery_charges","customer_lat","customer_long","coupon_discount","order_total","season","is_expedited_delivery","distance_to_nearest_warehouse","is_happy_customer")]
# 1.3: Kiểm tra dữ liệu khuyết
apply(is.na(cleaning_data),2,which)
# 1.4: Làm sạch những cột có biến phân loại chưa đồng nhất
str(cleaning_data)
# 1.4.1: Cột nearest_warehouse
table(cleaning_data$nearest_warehouse, useNA = "always")
cleaning_data$nearest_warehouse = toTitleCase(cleaning_data$nearest_warehouse)
table(cleaning_data$nearest_warehouse, useNA = "always")
# 1.4.2: Cột season
table(cleaning_data$season, useNA = "always")
cleaning_data$season = toTitleCase(cleaning_data$season)
table(cleaning_data$season, useNA = "always")
# 1.5: Ghi ra tệp con sao lưu dữ liệu đã làm sạch
cleaned_data = cleaning_data
# * Kiểm tra xem tệp có bị NA hay không
sum(is.na(cleaned_data))
rm(cleaning_data)
write.csv(cleaned_data, file = "cleaned_data_from_dirty_data.csv", row.names = FALSE)
# 2: Làm sạch file missing_data
# 2.1: Đọc dữ liệu
missing_data = read.csv("./missing_data.csv")
# 2.2: Làm sạch dữ liệu
# 2.2.1: Cột season:
as.Date(missing_data$date)
missing_data = missing_data %>%
mutate(date_onset = as.Date(date, format = "%Y/%m/%d"))
fix_season = function(date) {
month = as.POSIXlt(date)$mon +1
if (month %in% c(1, 2, 3)) {
return("Spring")
} else if (month %in% c(4, 5, 6)) {
return("Summer")
} else if (month %in% c(7, 8, 9)) {
return("Autumn")
} else {
return("Winter")
}
}
missing_data$season = sapply(missing_data$date, fix_season)
# 2.2.2: Cột nearest_warehouse
warehouses = read.csv("./warehouses.csv")
find_nearest_warehouse = function(customer, warehouses) {
distances = geosphere::distVincentySphere(
cbind(customer["customer_long"], customer["customer_lat"]), cbind(warehouses$lon, warehouses$lat)
)
nearest_warehouse = warehouses[which.min(distances), ]
return(nearest_warehouse$names)
}
empty_rows = missing_data$nearest_warehouse == ""
missing_data$nearest_warehouse[empty_rows] = apply(missing_data[empty_rows, c("customer_long", "customer_lat")], 1, function(row) find_nearest_warehouse(row, warehouses))
# 2.2.3: Cột is_happy_customer
get_sentiment = function(review) {
positive_keywords <- c("good", "excellent", "love", "like", "amazing", "happy", "recommend","great", "nice")
return(any(grepl(paste(positive_keywords, collapse = "|"), tolower(review))))
}
missing_data$is_happy_customer = ifelse(missing_data$is_happy_customer == "", sapply(missing_data$latest_customer_review, get_sentiment), missing_data$is_happy_customer)
missing_data$is_happy_customer[missing_data$is_happy_customer == 'TRUE'] = "True"
missing_data$is_happy_customer[missing_data$is_happy_customer == 'FALSE'] = "False"
# 2.2.4: Cột order_price
get_price = function(total, discount, deli){
price= (total-deli)/(1-discount/100)
return(price)
}
missing_data$order_price[is.na(missing_data$order_price)] = get_price(missing_data$order_total[is.na(missing_data$order_price)],missing_data$coupon_discount[is.na(missing_data$order_price)],missing_data$delivery_charges[is.na(missing_data$order_price)])
# 2.2.5: Cột order_total
get_total = function(price, discount, deli){
total= price*(1-discount/100)+deli
return(total)
}
missing_data$order_total[is.na(missing_data$order_total)] = get_total(missing_data$order_price[is.na(missing_data$order_total)],missing_data$coupon_discount[is.na(missing_data$order_total)],missing_data$delivery_charges[is.na(missing_data$order_total)])
# 2.2.6: Cột distance_to_nearest_warehouse
get_distance = function(customer, warehouses){
distances = geosphere::distVincentySphere(
cbind(customer["customer_long"], customer["customer_lat"]), cbind(warehouses$lon, warehouses$lat)
)
distance = min(distances)
return(distance/1000)
}
missing_data$distance_to_nearest_warehouse[is.na(missing_data$distance_to_nearest_warehouse)] = apply(missing_data[is.na(missing_data$distance_to_nearest_warehouse), c("customer_long", "customer_lat")], 1, function(row2) get_distance(row2, warehouses))
# 2.2.7: Xử lý dữ liệu còn lại NA
na_count = colSums(is.na(missing_data))
na_count
missing_data = missing_data %>%
select(-date_onset)
missing_data = na.omit(missing_data)
na_count = colSums(is.na(missing_data))
na_count
# Ghi ra tệp để sao lưu
write.csv(missing_data, file = "cleaned_data_from_missing_data.csv", row.names = FALSE)
# Tạo tệp mới
new_data = missing_data[,c("nearest_warehouse","order_price","delivery_charges","customer_lat","customer_long","coupon_discount","order_total","season","is_expedited_delivery","distance_to_nearest_warehouse","is_happy_customer")]
# Chỉnh các biến về để thống kê
new_data$is_expedited_delivery[new_data$is_expedited_delivery == 'True'] = 1
new_data$is_expedited_delivery[new_data$is_expedited_delivery == 'False'] = 0
new_data$is_expedited_delivery = as.integer(new_data$is_expedited_delivery)
new_data$is_happy_customer[new_data$is_happy_customer == 'True'] = 1
new_data$is_happy_customer[new_data$is_happy_customer == 'False'] = 0
new_data$is_happy_customer = as.integer(new_data$is_happy_customer)
new_data$season = factor((new_data$season), levels = c("Spring","Summer","Autumn","Winter"))
levels(new_data$season)
new_data$nearest_warehouse = factor((new_data$nearest_warehouse), levels = c("Bakers","Nickolson","Thompson"))
levels(new_data$nearest_warehouse)
summary(new_data)
# Tổng doanh thu từng cửa hàng
tongstore = new_data %>%
group_by(nearest_warehouse) %>%
summarise(total_sales = sum(order_total, na.rm = TRUE))
print(tongstore)
# Tổng doanh thu theo mùa
tongmua = new_data %>%
group_by(season) %>%
summarise(total_sales = sum(order_total, na.rm = TRUE))
print(tongmua)
# Biểu đồ cột cho biến giao hàng nhanh hay không
giao_hang_nhanh = table(new_data$is_expedited_delivery) / nrow(new_data) * 100
barplot(giao_hang_nhanh,
col = c("skyblue", "orange"),
main = "Biểu đồ cột cho biến giao hàng nhanh",
xlab = "Giao hàng nhanh (0 = Không, 1 = Có)",
ylab = "Phần trăm (%)",
beside = TRUE,
names.arg = c("Không","Có"),
ylim = c(0, 100))
# Biểu đồ cột cho biến khách hàng hài lòng hay không
khach_hang_hai_long = table(new_data$is_happy_customer) / nrow(new_data) * 100
barplot(khach_hang_hai_long,
col = c("lightgreen", "salmon"),
main = "Biểu đồ cột cho biến khách hàng hài lòng",
xlab = "Khách hàng hài lòng (0 = Không, 1 = Có)",
ylab = "Phần trăm (%)",
beside = TRUE,
names.arg = c("Không", "Có"),
ylim = c(0, 100))
# Biểu đồ tần suất cho biến chi phí giao hàng
hist(new_data$delivery_charges,
col = "skyblue",
main = "Biểu đồ tần suất cho biến chi phí giao hàng",
xlab = "Chi phí",
ylab = "Tần suất",
breaks = 20)
# Biểu đồ tần suất cho biến giá trị tổng
hist(new_data$order_total,
col = "pink",
main = "Biểu đồ tần suất cho giá trị tổng",
xlab = "Giá trị tổng",
ylab = "Tần suất",
)
# Kiểm tra giá trị ngoại lai cho giá trị đơn hàng và giá trị tổng
boxplot(new_data$order_price,main ="Giá trị đơn hàng")
boxplot(new_data$order_total,main ="Giá trị tổng")
## Xây dựng hàm xử lý giá trị ngoại lai
xulingoailai = function(data) {
Q1 = quantile(data, 0.25, na.rm = TRUE)
Q3 = quantile(data, 0.75, na.rm = TRUE)
IQR = Q3 - Q1
lower_bound = Q1 - 1.5 * IQR
upper_bound = Q3 + 1.5 * IQR
data[data < lower_bound | data > upper_bound] = NA
return(data)
}
## Áp dụng
new_data$order_total = xulingoailai(new_data$order_total)
new_data$order_price = xulingoailai(new_data$order_price)
boxplot(new_data$order_price,main ="Giá trị đơn hàng")
boxplot(new_data$order_total,main ="Giá trị tổng")
# Kiểm tra giá trị ngoại lai cho biến Giá trị tổng theo biến Mùa
boxplot(order_total ~ season,
data = new_data,
col = c("pink", "orange", "lightyellow", "cyan"),
main = "Giá trị tổng theo mùa",
xlab = "Mùa",
ylab = "Giá trị tổng")
## Xử lý giá trị ngoại lai
Spring_data = subset(new_data,new_data$season =="Spring")
Spring_data$order_total = xulingoailai(Spring_data$order_total)
Summer_data = subset(new_data,new_data$season =="Summer")
Summer_data$order_total = xulingoailai(Summer_data$order_total)
Autumn_data = subset(new_data,new_data$season =="Autumn")
Autumn_data$order_total = xulingoailai(Autumn_data$order_total)
Winter_data = subset(new_data,new_data$season =="Winter")
Winter_data$order_total = xulingoailai(Winter_data$order_total)
new_data_2 = rbind(Spring_data,Summer_data,Autumn_data,Winter_data)
apply(is.na(new_data_2),2,sum)
apply(is.na(new_data_2),2,mean)
new_data_2 = na.omit(new_data_2)
boxplot(order_total ~ season,
data = new_data_2,
col = c("pink", "orange", "lightyellow", "cyan"),
main = "Giá trị tổng theo mùa",
xlab = "Mùa",
ylab = "Giá trị tổng")
# Tạo Subset cho kho Bakers, Nickolson và Thompson
Bakers_data = subset(new_data_2, new_data_2$nearest_warehouse == "Bakers")
Nickolson_data = subset(new_data_2, new_data_2$nearest_warehouse == "Nickolson")
Thompson_data = subset(new_data_2, new_data_2$nearest_warehouse == "Thompson")
# Tạo qq_plot cho kho Bakers, Nickolson và Thompson
qq_plot_Bakers = ggqqplot(Bakers_data$order_total,
main = "Đồ thị phân phối xác suất của nhà kho Bakers")
qq_plot_Nickolson = ggqqplot(Nickolson_data$order_total,
main = "Đồ thị phân phối xác suất của nhà kho Nickolson")
qq_plot_Thompson = ggqqplot(Thompson_data$order_total,
main = "Đồ thị phân phối xác suất của nhà kho Thompson")
# In QQPlot
print(qq_plot_Bakers)
print(qq_plot_Nickolson)
print(qq_plot_Thompson)
# Thực hiện kiểm tra Shapiro-Wilk
shapiro.test(Bakers_data$order_total)
shapiro.test(Nickolson_data$order_total)
shapiro.test(Thompson_data$order_total)
# Sử dụng kiểm định Levene để kiểm định cho biến giá trị tổng theo kho hàng gần nhất
leveneTest(order_total~nearest_warehouse, data = new_data_2)
# Sử dụng phương sai 1 yếu tố
one.way = aov(order_total~nearest_warehouse, data = new_data_2)
summary(one.way)
# Sử dụng kiểm định Levene để kiểm định cho biến giá trị giá trị tổng theo mùa
leveneTest(order_total~season, data = new_data_2)
# Phương sai 1 yếu tố
one.way_2 = aov(order_total~season, data = new_data_2)
summary(one.way_2)
# Biển đổi new_data_2
new_data_2$nearest_warehouse = factor((new_data_2$nearest_warehouse), levels = c("Bakers","Nickolson","Thompson"))
levels(new_data_2$nearest_warehouse)
new_data_2$season = factor((new_data_2$season), levels = c("Spring", "Summer", "Autumn","Winter"))
levels(new_data_2$season)
summary(new_data_2)
# Tạo tệp new_data_3 để phân tích
new_data_3 = new_data_2[,c("nearest_warehouse","delivery_charges","customer_lat","customer_long","coupon_discount", "season", "order_total", "distance_to_nearest_warehouse", "is_expedited_delivery", "is_happy_customer")]
summary(new_data_3)
new_data_3$season = as.integer(new_data_3$season)
new_data_3$nearest_warehouse = as.integer(new_data_3$nearest_warehouse)
summary(new_data_3)
# Tạo mô hình hồi quy logistic với new_data_3
mohinh1 = glm(is_happy_customer~., family = "binomial", data = new_data_3)
summary(mohinh1)
# Tạo tệp new_data_4 sau khi loại bỏ các biến không cần thiết
new_data_4 = new_data_3[,c("delivery_charges", "season", "distance_to_nearest_warehouse", "is_expedited_delivery", "is_happy_customer")]
mohinh2 = glm(is_happy_customer~., family = "binomial", data = new_data_4)
summary(mohinh2)
# So sánh 2 mô hình bằng ANOVA
anova(mohinh1, mohinh2, test = "LRT")
# Dự báo và vẽ đường cong ROC
dubaoROC = predict(mohinh2, type = "response", newdata = new_data_4)
ROCRpred = prediction(dubaoROC, new_data_4$is_happy_customer)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize=TRUE, print.cutoffs.at= seq(0,1,by=0.1), text.adj=c(-0.2,1.7))