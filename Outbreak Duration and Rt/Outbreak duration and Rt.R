library(readxl)
library(writexl)

## Outbreak Duration
# High
data1 <- read_excel("D:/Kuliah/ResearchPractice/Model/Outbreak Duration and Rt/Fatukopa/allreport_high.xlsx") 
data1

duration_high <- data1$duration
duration_high

mean(duration_high)

#hist(duration_high)
#boxplot(duration_high)

Rabid_high <- data1$rabid_dogs
Rabid_high

mean(Rabid_high)

#hist(Rabid_high)
#boxplot(Rabid_high)


# Middle
data2 <-read_excel("D:/Kuliah/ResearchPractice/Model/Outbreak Duration and Rt/Fatukopa/allreport_middle.xlsx")

duration_Mid <- data2$duration
duration_Mid

mean(duration_Mid)

#hist(duration_Mid)
#boxplot(duration_Mid)

Rabid_Mid <- data2$rabid_dogs
Rabid_Mid

mean(Rabid_Mid)

#hist(Rabid_Mid)
#boxplot(Rabid_Mid)


# Low
data3 <-read_excel("D:/Kuliah/ResearchPractice/Model/Outbreak Duration and Rt/Fatukopa/allreport_low.xlsx")

duration_low <- data3$duration
duration_low

mean(duration_low)

#hist(duration_low)
#boxplot(duration_low)

Rabid_low <- data3$rabid_dogs
Rabid_low

mean(Rabid_low)

#hist(Rabid_low)
#boxplot(Rabid_low)


# Add 1
data4 <-read_excel("D:/Kuliah/ResearchPractice/Model/Outbreak Duration and Rt/Fatukopa/allreport_add1.xlsx")

duration_add1 <- data4$duration
duration_add1

mean(duration_add1)

#hist(duration_low)
#boxplot(duration_low)

Rabid_add1 <- data4$rabid_dogs
Rabid_add1

mean(Rabid_add1)

#hist(Rabid_low)
#boxplot(Rabid_low)


# Add 2
data5 <-read_excel("D:/Kuliah/ResearchPractice/Model/Outbreak Duration and Rt/Fatukopa/allreport_add2.xlsx")

duration_add2 <- data5$duration
duration_add2

mean(duration_add2)

#hist(duration_low)
#boxplot(duration_low)

Rabid_add2 <- data5$rabid_dogs
Rabid_add2

mean(Rabid_add2)

#hist(Rabid_low)
#boxplot(Rabid_low)


## Print and combine
combined_dns <- cbind(duration_add1,duration_add2,duration_high,duration_low,duration_Mid,Rabid_add1,Rabid_add2,Rabid_high,Rabid_low,Rabid_Mid)  
combined_dns <- as.data.frame(combined_dns)
write_xlsx(combined_dns, "Without_intervention.xlsx")

## Compare

boxplot(duration_high, duration_Mid, duration_low, duration_add1, duration_add2, main = "Duration", ylab = "Days", names = c("high", "middle", "low", "add1", "add2"))


boxplot(Rabid_high, Rabid_Mid, Rabid_low, Rabid_add1, Rabid_add2, main = "Rabid Dogs", ylab = "Dogs", names = c("high", "middle", "low" , "add1", "add2"))
