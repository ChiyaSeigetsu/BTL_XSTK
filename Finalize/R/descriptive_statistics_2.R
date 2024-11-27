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