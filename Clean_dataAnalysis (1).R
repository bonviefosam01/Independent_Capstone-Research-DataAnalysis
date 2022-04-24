library(ggplot2)
library(dplyr)

data <- read.csv('SHAPE_ACFT_dataClean.csv')

#============================Description Stats=================================
view(data)
head(data)
mean(data$self_ideal)
mean(data$self_perception)
mean(data$typical_female)
mean(data$typical_male)
mean(data$ideal_female)
mean(data$ideal_male)

sum(data$race == "Black or African American")
sum(data$race == "Caucasian")
sum(data$race == "Asian (e.g. Asian Indian, Chinese, Filipino, Japanese, Korean, Vietnamese)")
sum(data$race == "Biracial")
sum(data$race == "Middle Eastern")

print(mean(data$ideal_female) - mean(data$typical_female)) #larger disparity in female ideal-typical -> people think typical females are slightly larger than the ideal
print(mean(data$ideal_male) - mean(data$typical_male)) ## very small difference


#===============================Filtering/Cleaning The data================================

data[data$gender == "Male", 'gender'] <- "male"
data[data$gender == "Female", 'gender'] <- "female"

data <- data %>%
  mutate(percep_ideal_diff = data$self_perception - data$self_ideal) %>%
  mutate(fem_typ_ideal_diff = data$typical_female - data$ideal_female) %>%
  mutate(male_typ_ideal_diff = data$typical_male - data$ideal_male) %>%
  mutate(BMI = (data$Weight/data$Height/data$Height)*10000) %>%
  filter(gender != "")

ACFT_analysis <- data %>%
  filter(ACFT_score != "")


female_data <- data %>%
  filter(gender == "female")

male_data <- data %>%
  filter(gender == "male")

w_self_diff[w_self_diff$gender == "Male", 'gender'] <- "male"
w_self_diff[w_self_diff$gender == "Female", 'gender'] <- "female"

w_self_diff <- data %>%
  mutate(percep_ideal_diff = data$self_perception - data$self_ideal) %>%
  filter(gender != "")

w_self_diff <- w_self_diff %>%
  mutate(fem_typ_ideal_diff = w_self_diff$typical_female - w_self_diff$ideal_female) %>%
  mutate(male_typ_ideal_diff = w_self_diff$typical_male - w_self_diff$ideal_male)

#need tofilter the race columns, participants inputted them a little differently so there are redundant columns


#=================================ASSUMPTIONS VERIFICATION====================================================

hist(data$percep_ideal_diff, xlab = "Self Perception Discrepancy", main = "Histogram of Self Perception Discrepancy", col = "light yellow")
qqnorm(Diff_AOV_gender$residuals)
qqline(Diff_AOV_gender$residuals)
shapiro.test(data$percep_ideal_diff)
bartlett.test(data$percep_ideal_diff ~ data$gender, data)


BMI_score_lm <- lm(ACFT_analysis$percep_ideal_diff ~ ACFT_analysis$ACFT_score)
plot(ACFT_analysis$percep_ideal_diff ~ ACFT_analysis$ACFT_score, xlab = "ACFT Overall Score", ylab = "Self Perception Discrepancy", main = "ACFT Overall Score vs Self Perception Discrepancy",
     pch = 19, frame = FALSE)
lines(lowess(ACFT_analysis$percep_ideal_diff, ACFT_analysis$ACFT_score), col = "blue")

BMI_score_resid = resid(BMI_score_lm)
length()
plot(ACFT_analysis$ACFT_score, BMI_score_resid,  xlab = "ACFT Overall Score", ylab = "Self Perception Discrepancy Residuals", main = "Residual Plot", pch = 19, frame = FALSE)
summary(BMI_score_lm)



#======================================Plotting/Data Visualization================================================

#-----------Research Aim One----------------

#displays self perception versus the ideal - positive correlation (all genders)
ideal_v_perception <- ggplot(data, aes(x = data$self_perception, y = data$self_ideal)) + geom_point() #this makes it look like people are generally satisfied with how they look
print(ideal_v_perception)

#displays women self perception versus the ideal - positive correlation
female_ideal_v_perception <- ggplot(female_data, aes(x = female_data$self_perception, y = female_data$self_ideal)) + geom_point() #this makes it look like people are generally satisfied with how they look
print(female_ideal_v_perception)

#displays women self perception versus the ideal - positive correlation
male_ideal_v_perception <- ggplot(male_data, aes(x = male_data$self_perception, y = male_data$self_ideal)) + geom_point() #this makes it look like people are generally satisfied with how they look
print(male_ideal_v_perception)

#displays ALL participants discrepency value - negative means desire to be larger , negative is desire to be smaller
self_error <- ggplot(data, aes(x = data$percep_ideal_diff, fill = data$gender)) + geom_histogram(bins = 40, alpha = 0.5, position = "dodge") + xlab("Self Perception Discrepancy") +
      ylab("Frequency") + ggtitle("Self Perception Discrepancies Among Cadets") + labs(fill = "Gender") + theme(plot.title = element_text(hjust=0.5))
print(self_error)

self_error_2 <- ggplot(data, aes(x = data$percep_ideal_diff, fill = data$gender)) + geom_density(alpha = 0.5) + xlab("Self Perception Discrepancy") +
  ylab("Frequency") + ggtitle("Self Perception Discrepancies Among Cadets") + labs(fill = "Gender") + theme(plot.title = element_text(hjust=0.5))
print(self_error_2)

#Displays Self Perception distribution for all genders catagorized by race
self_perception <- ggplot(data, aes(x = data$self_perception, color = data$race)) + geom_density(fill = "White", position = "identity")
print(self_perception)
print(range(data$self_ideal))

self_error <- ggplot(data, aes(x = data$percep_ideal_diff, fill = data$race)) + geom_histogram(bins = 40, color = "black") + xlab("Self Perception Discrepancy") +
  ylab("Frequency") + ggtitle("Self Perception Discrepancies Among Cadets") + labs(fill = "Race") + theme(plot.title = element_text(hjust=0.5))
print(self_error)

#Displays Self Ideal distribution for all genders catagorized by race
self_ideal <- ggplot(data, aes(x = data$self_ideal, color = data$race)) + geom_density(fill = "White", position = "identity")
print(self_ideal)
print(range(data$self_ideal))

#Displays Self Discrepacy distribution for all genders catagorized by race
self_discrepancy <- ggplot(data, aes(x=data$self_perception - data$self_ideal , color = data$race)) + geom_density(fill = "White", position = "identity")
print(self_discrepancy)

#Displays Self Discrepacy distribution for all genders catagorized by gender
self_discrepancy <- ggplot(data, aes(x=data$self_perception - data$self_ideal , color = data$gender)) + geom_density(fill = "White", position = "identity")
print(self_discrepancy)


#Discrepancy by class rank
self_error <- ggplot(data, aes(x = data$percep_ideal_diff, fill = data$class_rank)) + geom_density() + xlab("Self Perception Discrepancy") +
  ylab("Frequency") + ggtitle("Self Perception Discrepancies Among Cadets") + labs(fill = "Class Rank") + theme(plot.title = element_text(hjust=0.5))
print(self_error)



##---Seperated by Gender
ggplot(data, aes(x = data$gender, y = data$percep_ideal_diff)) + geom_boxplot()
ggplot(female_data, aes(x=female_data$race, y= female_data$percep_ideal_diff)) + geom_boxplot()

#These are similar to the distibution cust box, so I can pick
ggplot(w_self_diff, aes(x = w_self_diff$gender, y = w_self_diff$percep_ideal_diff)) + geom_boxplot()
ggplot(w_self_diff, aes(x = w_self_diff$race, y = w_self_diff$percep_ideal_diff)) + geom_boxplot()
ggplot(w_self_diff, aes(x = w_self_diff$class, y = w_self_diff$percep_ideal_diff)) + geom_boxplot()
ggplot(w_self_diff, aes(x = w_self_diff$class_rank, y = w_self_diff$percep_ideal_diff)) + geom_boxplot()


#---------Research Aim Two-----------------
female_typ_v_ideal <- ggplot(data, aes(x = data$typical_female, y = data$ideal_female)) + geom_point()
print(female_typ_v_ideal)
male_typ_v_ideal <- ggplot(data, aes(x = data$typical_male, y = data$ideal_male)) + geom_point()
print(male_typ_v_ideal)

#-Female Officer
ggplot(w_self_diff, aes(x = w_self_diff$gender, y = w_self_diff$fem_typ_ideal_diff)) + geom_boxplot()
female_officer_discrepancy <- ggplot(data, aes(x=data$self_perception - data$self_ideal , color = data$gender)) + geom_density(fill = "White", position = "identity")
print(self_discrepancy)
ggplot(w_self_diff, aes(x = w_self_diff$race, y = w_self_diff$fem_typ_ideal_diff)) + geom_boxplot()
ggplot(w_self_diff, aes(x = w_self_diff$class, y = w_self_diff$fem_typ_ideal_diff)) + geom_boxplot()
ggplot(w_self_diff, aes(x = w_self_diff$class_rank, y = w_self_diff$fem_typ_ideal_diff)) + geom_boxplot()

#-Male Officer
ggplot(w_self_diff, aes(x = w_self_diff$gender, y = w_self_diff$male_typ_ideal_diff)) + geom_boxplot()
ggplot(w_self_diff, aes(x = w_self_diff$race, y = w_self_diff$male_typ_ideal_diff)) + geom_boxplot()
ggplot(w_self_diff, aes(x = w_self_diff$class, y = w_self_diff$male_typ_ideal_diff)) + geom_boxplot()
ggplot(w_self_diff, aes(x = w_self_diff$class_rank, y = w_self_diff$male_typ_ideal_diff)) + geom_boxplot()


#--------Research Aim Three------------

#scatter plot (Acft overall score vs BMI)
plot(ACFT_analysis$ACFTScore, ACFT_analysis$BMI)

score_BMI <- ggplot(data, aes(x = data$ACFT_score, y = data$BMI)) + geom_point()
print(score_BMI)
       
#scatter plot (Acft overall score vs Self perception)
score_percetion <- ggplot(data, aes(x = data$self_perception, y = data$ACFT_score)) + 
  geom_point() + ggtitle("Self Perception Discrepancies versus ACFT Score") + xlab("Self Perception Discrepancy") + ylab("ACFT Score") + theme(plot.title = element_text(hjust=0.5))
print(score_percetion)

#scatter plot (Acft overall score vs Ideal perception)
score_ideal <- ggplot(data, aes(x = data$self_ideal, y = data$ACFT_score)) + geom_point()
print(score_ideal)

#scatter plot (Acft overall score vs perception discrepancy)
score_discrepancy <- ggplot(data, aes(x = ACFT_analysis$self_perception - data$self_ideal, y = data$ACFT_score)) + geom_point()
print(score_discrepancy)

score_discrepancy <- ggplot(data, aes(x = data$percep_ideal_diff, y = data$ACFT_score)) + 
  geom_point() + ggtitle("Self Perception Discrepancies versus ACFT Score") + xlab("Self Perception Discrepancy") + ylab("ACFT Score") + theme(plot.title = element_text(hjust=0.5))
print(score_discrepancy)


score_female_ideal <- ggplot(data, aes(x = data$ideal_female, y = data$ACFT_score)) +geom_point() +
  ggtitle("Female Officer Perception Discrepancies versus ACFT Score") + xlab("Female Officer Perception Discrepancy") + ylab("ACFT Score") + theme(plot.title = element_text(hjust=0.5))
print(score_female_ideal)

score_female_discrepancy <-ggplot(data, aes(x = data$typical_female - data$ideal_female, y = data$ACFT_score)) +geom_point()
print(score_female_discrepancy)

score_female_ideal <- ggplot(data, aes(x = data$fem_typ_ideal_diff, y = data$ACFT_score)) +geom_point() +
  ggtitle("Female Officer Perception Discrepancies versus ACFT Score") + xlab("Female Officer Perception Discrepancy") + ylab("ACFT Score") + theme(plot.title = element_text(hjust=0.5))
print(score_female_ideal)

score_male_ideal <- ggplot(data, aes(x = data$male_typ_ideal_diff, y = data$ACFT_score)) +geom_point() +
  ggtitle("Male Officer Perception Discrepancies versus ACFT Score") + xlab("Male Officer Perception Discrepancy") + ylab("ACFT Score") + theme(plot.title = element_text(hjust=0.5))
print(score_male_ideal)


#==================================ANOVA/LINEAR REGRESSION Analysis=================================

#--------Research Aim One--------------

#---ANOVA by Gender---
summary(aov(data$self_perception ~ data$gender, data)) ###There is a difference between genders

Diff_AOV_gender <- aov(data$percep_ideal_diff ~ gender, data)
summary(Diff_AOV_gender)
TukeyHSD(Diff_AOV_gender)
plot(TukeyHSD(Diff_AOV_gender)) 

mean(female_data$self_perception - female_data$self_ideal)
mean(male_data$self_perception - female_data$self_ideal)

#---ANOVA by Race---
summary(aov(data$self_perception ~ data$race, data))
summary(aov(data$self_ideal ~ data$race, data))

Diff_AOV_race <- aov(w_self_diff$percep_ideal_diff ~ race, w_self_diff)
summary(Diff_AOV_race)
TukeyHSD(Diff_AOV_race)
plot(TukeyHSD(Diff_AOV_race)) 

#Female
Diff_AOV_female_race <- aov(female_data$self_perception - female_data$self_ideal ~ race, female_data)
summary(Diff_AOV_female_race)
TukeyHSD(Diff_AOV_female_race)
plot(TukeyHSD(Diff_AOV_female_race)) 

#Male
Diff_AOV_male_race <- aov(male_data$self_perception - male_data$self_ideal ~ race, male_data)
summary(Diff_AOV_male_race)
TukeyHSD(Diff_AOV_male_race)
plot(TukeyHSD(Diff_AOV_male_race)) 

#---ANOVA by Class---
summary(aov(data$self_perception ~ data$class, data))

Diff_AOV_class <- aov(data$percep_ideal_diff ~ class, data)
summary(Diff_AOV_class)
TukeyHSD(Diff_AOV_class)
plot(TukeyHSD(Diff_AOV_class)) 

data$class <- factor(data$class, levels = c("Plebe", "Yearling","Cow", "Firstie"))

boxplot(data$percep_ideal_diff ~ data$class, data, xlab = "Cadet Class", ylab = "Cadet Self Perception Discrepancy", 
        main = "Self Perception Discrepancy by Class", col = c("grey", "yellow", "light green", "light blue"))

halves <- data %>%
  mutate(half = ifelse(data$class == "Plebe", 1,
         ifelse(data$class == "Yearling", 1, 
         ifelse(data$class == "Cow", 2,
         ifelse(data$class == "Firstie",2, "no")))))

under <- halves %>%
  filter(half == 1)
mean(under$percep_ideal_diff)

upper <- halves %>%
  filter(half == 2)
mean(upper$percep_ideal_diff)

#---ANOVA by Class Rank----
summary(aov(data$self_perception ~ data$class_rank, data))

Diff_AOV_classRank <- aov(data$percep_ideal_diff ~ class_rank, data)
summary(Diff_AOV_classRank)
res = TukeyHSD(Diff_AOV_classRank)
plot(TukeyHSD(Diff_AOV_classRank), xlab = "Class Rank Bucket", ylab = "Difference in Means in Class Rank") #, main = "Self Perception Discrepany by Class Rank") 
boxplot(data$percep_ideal_diff ~ data$class_rank, data, xlab = "Class Rank Bucket", ylab = "Cadet Self Perception Discrepancy", 
        main = "Self Perception Discrepancy by Class Rank", col = c("light green", "yellow", "orange"))

top <- data %>%
  filter(data$class_rank == "01-400")
mean(top$percep_ideal_diff)
middle <- data %>%
  filter(data$class_rank == "401-800")
mean(middle$percep_ideal_diff)
bottom <- data %>%
  filter(data$class_rank == "Over 800")
mean(bottom$percep_ideal_diff)


#-----Research Aim Two-------

#---ANOVA by Gender---
summary(aov(data$typical_female ~ data$gender, data))
femO_Diff_AOV_gender <- aov(data$fem_typ_ideal_diff ~ gender, data = data)
summary(femO_Diff_AOV_gender)
TukeyHSD(femO_Diff_AOV_gender)
plot(TukeyHSD(femO_Diff_AOV_gender))

mean(female_data$fem_typ_ideal_diff)
mean(male_data$fem_typ_ideal_diff)


summary(aov(data$typical_male ~ data$gender, data))
TukeyHSD(aov(data$typical_male ~ data$gender, data))
maleO_Diff_AOV_gender <- aov(data$typical_male - data$ideal_male ~ gender, data)
TukeyHSD(maleO_Diff_AOV_gender)
plot(TukeyHSD(maleO_Diff_AOV_gender))

mean(female_data$male_typ_ideal_diff)
mean(male_data$male_typ_ideal_diff)

#---ANOVA by Race---
summary(aov(data$typical_female ~ data$race, data))
femO_Diff_AOV_race <- aov(data$fem_typ_ideal_diff ~ race, data)
summary(femO_Diff_AOV_gender)
TukeyHSD(femO_Diff_AOV_race)
plot(TukeyHSD(femO_Diff_AOV_race))

ASIAN_mean <- data%>%
  filter(data$race == "Asian (e.g. Asian Indian, Chinese, Filipino, Japanese, Korean, Vietnamese)")
print(mean(ASIAN_mean$fem_typ_ideal_diff))

bi_mean <- data%>%
  filter(data$race == "Biracial")
print(mean(bi_mean$fem_typ_ideal_diff))


black_mean <- data%>%
  filter(data$race == "Black or African American")
print(mean(black_mean$fem_typ_ideal_diff))
Cau_mean <- data%>%
  filter(data$race == "Caucasian")
print(mean(Cau_mean$fem_typ_ideal_diff))

his_mean <- data%>%
  filter(data$race == "Hispanic")
print(mean(his_mean$fem_typ_ideal_diff))
mid_mean <- data%>%
  filter(data$race == "Middle Eastern")
print(mean(mid_mean$fem_typ_ideal_diff))
  
print(race_mean)

summary(aov(data$typical_male ~ data$race, data))
maleO_Diff_AOV_race <- aov(data$male_typ_ideal_diff ~ race, data)
summary(maleO_Diff_AOV_race)
TukeyHSD(maleO_Diff_AOV_race)
plot(TukeyHSD(maleO_Diff_AOV_race))

boxplot(data$male_typ_ideal_diff ~ data$race, data, xlab = "Race", ylab = "Cadet Male Officer Perception Discrepancy", 
        main = "Self Perception Discrepancy by Race",   col = c("light blue", "grey", "brown", "tan", "pink", "light green"), 
        names = c("Asian", "Biracial", "Black or African American", "Caucasian", "Hisplanic", "Middle Eastern"))

asian <- data %>%
  filter(data$race == "Asian (e.g. Asian Indian, Chinese, Filipino, Japanese, Korean, Vietnamese)")
mean(asian$male_typ_ideal_diff)
notAsian <- data %>%
  filter(data$race != "Asian (e.g. Asian Indian, Chinese, Filipino, Japanese, Korean, Vietnamese)")
mean(notAsian$male_typ_ideal_diff)

asian <- male_data %>%
  filter(male_data$race == "Asian (e.g. Asian Indian, Chinese, Filipino, Japanese, Korean, Vietnamese)")
mean(asian$male_typ_ideal_diff)
notAsian <- data %>%
  filter(data$race != "Asian (e.g. Asian Indian, Chinese, Filipino, Japanese, Korean, Vietnamese)")
mean(notAsian$male_typ_ideal_diff)


#---ANOVA by Class---
summary(aov(data$typical_female ~ data$class, data))
summary(aov(data$ideal_female ~ data$class, data))
femO_Diff_AOV_class<- aov(data$fem_typ_ideal_diff ~ class, data)
summary(femO_Diff_AOV_class)
TukeyHSD(femO_Diff_AOV_class)
plot(TukeyHSD(femO_Diff_AOV_class))

data$class <- factor(data$class, levels = c("Plebe", "Yearling","Cow", "Firstie"))
boxplot(data$fem_typ_ideal_diff ~ data$class, data, xlab = "Class", ylab = "Cadet Female Officer Perception Discrepancy", 
        main = "Self Perception Discrepancy by Class", col = c("grey", "yellow", "light green", "light blue"))

plebe <- data %>%
  filter(data$class == "Plebe")
mean(plebe$fem_typ_ideal_diff)
yuk <- data %>%
  filter(data$class == "Yearling")
mean(yuk$fem_typ_ideal_diff)

cow <- data %>%
  filter(data$class == "Cow")
mean(cow$fem_typ_ideal_diff)

firstie <- data %>%
  filter(data$class == "Firstie")
mean(firstie$fem_typ_ideal_diff)


summary(aov(data$typical_male ~ data$class, data))
maleO_Diff_AOV_class <- aov(data$male_typ_ideal_diff ~ class, data)
summary(maleO_Diff_AOV_class)
TukeyHSD(maleO_Diff_AOV_class)
plot(TukeyHSD(maleO_Diff_AOV_class))

mean(plebe$male_typ_ideal_diff)
mean(yuk$male_typ_ideal_diff)
mean(cow$fem_typ_ideal_diff)
mean(firstie$male_typ_ideal_diff)

boxplot(data$male_typ_ideal_diff ~ data$class, data, xlab = "Class", ylab = "Cadet Male Officer Perception Discrepancy", 
        main = "Self Perception Discrepancy by Class", col = c("grey", "yellow", "light green", "light blue"))




#---ANOVA by Class Rank----
summary(aov(data$typical_female ~ data$class_rank, data))
summary(aov(data$ideal_female ~ data$class_rank, data))
femO_Diff_AOV_classRank<- aov(data$fem_typ_ideal_diff ~ class_rank, data)
summary(femO_Diff_AOV_classRank)
TukeyHSD(femO_Diff_AOV_classRank)
plot(TukeyHSD(femO_Diff_AOV_classRank))

mean(top$fem_typ_ideal_diff)
mean(middle$fem_typ_ideal_diff)
mean(bottom$fem_typ_ideal_diff)



summary(aov(data$typical_male ~ data$class_rank, data))
maleO_Diff_AOV_classRank <- aov(data$male_typ_ideal_diff ~ class_rank, data)
summary(maleO_Diff_AOV_classRank)
TukeyHSD(maleO_Diff_AOV_classRank)
plot(TukeyHSD(maleO_Diff_AOV_classRank))

mean(top$male_typ_ideal_diff)
mean(middle$male_typ_ideal_diff)
mean(bottom$male_typ_ideal_diff)


#-----Research Aim Three-----------
######Linear Regression

#--------------NOT SEPARATED BY GENDER-----------------

BMI_score_lm <- lm(data$BMI ~ data$ACFT_score)
summary(BMI_score_lm)

#lin reg for self perception versus overall ACFT score - shows very litte correlation
score_perception_lm <- lm(data$self_perception ~ data$ACFT_score)
summary(score_perception_lm)

BMI_perception_lm <- lm(ACFT_analysis$self_perception ~ ACFT_analysis$BMI)
summary(BMI_perception_lm)

#lin reg for self ideal versus overall ACFT score - shows very litte correlation
score_ideal_lm <- lm(data$self_ideal ~ data$ACFT_score)
summary(score_ideal_lm)

BMI_ideal_lm <- lm(ACFT_analysis$self_ideal ~ ACFT_analysis$BMI)
summary(BMI_ideal_lm)

#lin reg for self ideal versus overall ACFT score - shows very litte correlation
score_diff_lm <- lm(data$self_perception - data$self_ideal~ data$ACFT_score)
summary(score_diff_lm)

BMI_diff_lm <- lm(ACFT_analysis$self_perception - ACFT_analysis$self_ideal ~ ACFT_analysis$BMI)
summary(BMI_diff_lm)

#lin reg for female officer ideal versus overall ACFT score - shows very litte correlation
score_female_ideal_lm <- lm(data$ideal_female ~ data$ACFT_score)
summary(score_female_ideal_lm)

BMI_female_ideal_lm <- lm(ACFT_analysis$ideal_female ~ ACFT_analysis$BMI)
summary(BMI_female_ideal_lm)

#lin reg for female typical - ideal discrepancy versus overall ACFT score - shows very litte correlation
score_female_discrepancy_lm <- lm(data$typical_female - data$ideal_female ~ data$ACFT_score)
summary(score_female_discrepancy_lm)

#lin reg for male officer ideal versus overall ACFT score - shows very litte correlation
score_male_ideal_lm <- lm(data$ideal_male ~ data$ACFT_score)
summary(score_male_ideal_lm)

#lin reg for male typical - ideal discrepancy versus overall ACFT score - shows very litte correlation
score_male_discrepancy_lm <- lm(data$typical_male - data$ideal_male ~ data$ACFT_score)
summary(score_male_discrepancy_lm)

#------------------MALES ONLY----------------------------
#lin reg for self perception versus overall ACFT score - shows very litte correlation
score_perception_lm <- lm(male_data$self_perception ~ data$ACFT_score)
summary(score_perception_lm)

#lin reg for self ideal versus overall ACFT score - shows very litte correlation
score_ideal_lm <- lm(male_data$self_ideal ~ data$ACFT_score)
summary(score_ideal_lm)

#lin reg for self ideal versus overall ACFT score - shows very litte correlation
score_diff_lm <- lm(male_data$self_perception - data$self_ideal~ data$ACFT_score)
summary(score_diff_lm)

#lin reg for male officer ideal versus overall ACFT score - shows very litte correlation
score_male_ideal_lm <- lm(male_data$ideal_male ~ data$ACFT_score)
summary(score_male_ideal_lm)

#lin reg for male typical - ideal discrepancy versus overall ACFT score - shows very litte correlation
score_male_discrepancy_lm <- lm(male_data$typical_male - data$ideal_male ~ data$ACFT_score)
summary(score_male_discrepancy_lm)







##also need to divide each thing into different genders to see if it makes a difference



