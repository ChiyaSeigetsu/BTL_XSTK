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

