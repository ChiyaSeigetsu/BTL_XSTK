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