# Cài đặt thư viện cần thiết
install.packages("dplyr")
install.packages("geosphere")
# Gọi thư viện cần thiết
library(dplyr)
library(tools)
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