url <- "https://raw.githubusercontent.com/Abrars2000/DataScience_Project/main/WA_Fn-UseC_-Telco-Customer-Churn.csv"
data <- read.csv(url)
head(data)

data$Churn <- as.factor(data$Churn)
levels(data$Churn)
data$MonthlyCharges <- as.numeric(data$MonthlyCharges)
data$tenure <- as.numeric(data$tenure)

tapply(data$MonthlyCharges, data$Churn, mean, na.rm = TRUE)


churn_means <- tapply(data$MonthlyCharges, data$Churn, mean, na.rm = TRUE)
barplot(churn_means,
        main = "Average Monthly Charges by Churn",
        xlab = "Churn",
        ylab = "Mean Monthly Charges",
        col = c("steelblue", "tomato"),
        names.arg = c("No", "Yes"))


churn_groups <- split(data$tenure, data$Churn)

variability_results <- data.frame(
  Churn = c("No", "Yes"),
  SD = c(sd(churn_groups$No, na.rm = TRUE), sd(churn_groups$Yes, na.rm = TRUE)),
  Variance = c(var(churn_groups$No, na.rm = TRUE), var(churn_groups$Yes, na.rm = TRUE)),
  IQR = c(IQR(churn_groups$No, na.rm = TRUE), IQR(churn_groups$Yes, na.rm = TRUE)),
  Range = c(diff(range(churn_groups$No, na.rm = TRUE)), diff(range(churn_groups$Yes, na.rm = TRUE)))
)

print(variability_results)



boxplot(tenure ~ Churn, data = data,
        main = "Variability of Tenure by Churn",
        xlab = "Churn",
        ylab = "Tenure (Months)",
        col = c("lightblue", "lightsalmon"),
        names = c("No", "Yes"))







