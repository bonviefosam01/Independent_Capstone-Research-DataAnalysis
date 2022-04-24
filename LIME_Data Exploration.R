#library(readr)
data <- read.csv("LIME Data Collection Clean One.csv")
#View(data)
print(data)
print(length(data$Group))
print(length(data$Score_1))

result_score1 = aov(data$Score_1~data$Group, data)
summary(result_score1)
TukeyHSD(result_score1)

result_score2 = aov(data$Score_2~data$Group, data)
summary(result_score2)
TukeyHSD(result_score2)

result_score3 = aov(data$Score_3~data$Group, data)
summary(result_score3)
TukeyHSD(result_score3)




library(reshape)
library(dplyr)
library(ggplot2)
library(tidyverse)

melted_data = melt(data = data)
C_melted_data <- filter(melted_data, melted_data$variable == "HR1"| melted_data$variable == "HR2" | melted_data$variable == "HR3" | melted_data$variable == "HR4" | melted_data$variable == "HR5")
print(C_melted_data)
tail(melted_data)
Blocking = aov(C_melted_data$value ~ C_melted_data$variable + C_melted_data$Group, data = C_melted_data)
summary(Blocking)

TukeyHSD(Blocking)


aov(C_melted_data$value ~ C_melted_data$variable + C_melted_data$Group, data = C_melted_data)



plot <- ggplot(data, aes(x = data$Group, y = data$Score_1, fill = data$Group)) + geom_boxplot(alpha=0.7) + ggtitle("First Iteration Scores by Motivation Type")
plot

plot3 <- ggplot(data, aes(x = data$Group, y = data$Score_3, fill = data$Group)) + geom_boxplot(alpha=0.7) + ggtitle("First Iteration Scores 3 by Motivation Type")
plot3


## heart rate plot
data_w_avgHR = data %>%
  mutate(avg_HR = (data$HR1 + data$HR2 + data$HR3 + data$HR4 + data$HR5)/5)


plot_hr <- ggplot(data_w_avgHR, aes(x = data_w_avgHR$Group, y = data_w_avgHR$avg_HR, fill = data_w_avgHR$Group)) + geom_boxplot(alpha= 0.7)
plot_hr


result_HR = aov(data_w_avgHR$avg_HR~data_w_avgHR$Group, data)
summary(result_HR)
TukeyHSD(result_HR)

result_HR = aov(data_w_avgHR$HR3~data_w_avgHR$Group, data)
summary(result_HR)
TukeyHSD(result_HR)




plot(x = data$HR1, y = data$score1)


LinReg = lm(data$score1 ~ data$PS_ability.level + data$PS_shot_experience, data$PS_importance)
print(data$FS_focus_2)

scatter.smooth()


