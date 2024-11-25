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