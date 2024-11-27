# Tổng doanh thu từng cửa hàng
tongstore = new_data %>%
group_by(nearest_warehouse) %>%
summarise(total_sales = sum(order_total, na.rm = TRUE))
print(tongstore)
# Tổng doanh thu theo mùa

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