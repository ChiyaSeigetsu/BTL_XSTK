# Dự báo và vẽ đường cong ROC
dubaoROC = predict(mohinh2, type = "response", newdata = new_data_4)
ROCRpred = prediction(dubaoROC, new_data_4$is_happy_customer)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize=TRUE, print.cutoffs.at= seq(0,1,by=0.1), text.adj=c(-0.2,1.7))