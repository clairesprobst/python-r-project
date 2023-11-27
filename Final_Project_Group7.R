################## PROJECT PYTHON AND R LAB ##################
# GROUP 7
# Emiliano Trombetta, Martina Romano, Emanuela Rremilli, Claire Probst

# essential libraries
library(ggplot2)
library(corrplot)
library(psych)
library(e1071)

# Exploratory Data Analysis
D = read.csv("~/Desktop/luiss/fall2023/python_r_lab/student-mat.csv", stringsAsFactors=TRUE)
attach(D)

# dimension of dataset, summary of dataset, check for null values
dim(D)
summary(D)
head(D, 5)
sum(is.na(D))

# Analyzing each variable: absolute frequency, relative frequency,
# boxplots in relation to G3 
par(mfrow = c(1,2))

###### Individual Aspect ######

#sex
tsex = table(sex)
round(prop.table(tsex),3)*100
barplot(table(data$sex), main = "Bar Plot of Categories")
pie(table(data$sex), main = "Pie Chart of Categories")
boxplot(G3 ~ sex, data = data, main = "Box Plot by Category", horizontal = T, pch = 19, col=c("pink", "lightblue"))

#reason
treas = table(reason)
round(prop.table(treas),3)*100
barplot(table(D$reason), main = "Bar Plot of Categories")
pie(table(D$reason), main = "Pie Chart of Categories")
boxplot(G3 ~ reason, data = D, main = "Box Plot by Category", horizontal = T, pch = 19, col=c("pink", "lightblue" ,"lightgreen", "lavender"))
ggplot(data, aes(x = reason, y = G3)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(title = "Bar Plot of Mean Numeric Variable by Category")

#traveltime
ttravel = table(traveltime)
round(prop.table(ttravel),3)*100
barplot(table(D$traveltime), main = "Bar Plot of Categories")
pie(table(D$traveltime), main = "Pie Chart of Categories")
boxplot(G3 ~ traveltime, data = D, main = "Box Plot by Category", horizontal = T, pch = 19, col=c("pink", "lightblue" ,"lightgreen", "lavender"))
ggplot(D, aes(x = traveltime, y = G3)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(title = "Bar Plot of Mean Numeric Variable by Category")

#studytime
tstudytime =table(studytime)
round(prop.table(tstudytime),3)*100
barplot(table(D$studytime), main = "Bar Plot of Categories")
pie(table(D$studytime), main = "Pie Chart of Categories")
boxplot(G3 ~ studytime, data = D, main = "Box Plot by Category", horizontal = T, pch = 19, col=c("pink", "lightblue" ,"lightgreen", "lavender"))
ggplot(D, aes(x = studytime, y = G3)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(title = "Bar Plot of Mean Numeric Variable by Category")

#higher
higher_t = table(higher)
higher_percent = prop.table(higher_t)*100
barplot(table(D$higher), main = "Bar Plot of Categories")
pie(table(D$higher), main = "Pie Chart of Categories")
boxplot(G3 ~ higher, data = D, main = "Box Plot by Category", horizontal = T, pch = 19, col=c("pink", "lightblue"))
ggplot(D, aes(x = higher, y = G3)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(title = "Bar Plot of Mean Numeric Variable by Category")

# Age
summary(data$age)
tage = table(age)
round(prop.table(tage),3)*100
hist(age, col = "lightblue", main = "histogram of age")
abline(v = mean(age), col = "black", lwd = 3)
boxplot(age, horizontal = T, col = "lightblue", pch = 19)
skewness(D$age)
kurtosis(D$age)

#absences
summary(D$absences)
tabs = table(absences)
round(prop.table(tabs),3)*100
hist(absences, col = "lightblue", main = "histogram of absences",breaks = 15)
abline(v = mean(absences), col = "black", lwd = 3)

# In this case the indicator of the skewness is positive, the distribution has a right-skew, indicating that the majority of data points are concentrated on the left side. En if the value is almost close to zero, so the asymmetry is really slight.
# while a value less than 3, as far as the kurtosis is concerned) indicates lighter tails and a flatter distribution (platykurtic).
# If kurtosis is less than 3, the distribution has lighter tails and a flatter shape, implying fewer extreme values.
skewness(D$absences)
kurtosis(D$absences)

# additional scatter plot of absences and G3 scores
abs_class = cut(absences, breaks = seq(from = 0, to = 75, by = 5), include.higher = F)
table(abs_class)
plot(G3[absences == 0], main = "scatterplot of score in G3 by number of absences", pch = 19, cex = 0.3, ylim = c(0,23))
points(G3[absences<5 & absences>0], col = "red", cex = 0.6, pch = 4)
points(G3[absences<10 & absences>5], col = "orange", cex = 0.6, pch = 4)
points(G3[absences<15 & absences>10], col = "yellow", cex = 0.6, pch = 4)
points(G3[absences<20 & absences>15], col = "green", cex = 0.6, pch = 4)
points(G3[absences<25 & absences>20], col = "lightblue", cex = 0.6, pch = 4)
points(G3[absences<35 & absences>25], col = "purple", cex = 0.6, pch = 4)
points(G3[absences>25], col = "pink", cex = 0.6, pch = 4)
legend("bottomright", fill = c("black","red","yellow","green","lightblue","purple","pink"), legend = c("0","0-5","5-10","10-15","15-20","20-25","25-35",">25"), cex = 0.4)

###### Family Background ######
# Address: Urban (U) or Rural (R)
tadd = table(address)
round(prop.table(tadd),3)*100
par(mfrow = c(1,2))
pie(round(prop.table(tadd),3)*100, labels = paste0(names(round(prop.table(tadd),3)*100)," : ",round(prop.table(tadd),3)*100," %"), col = c(7,12))
boxplot(G3~address, cex = 0.5, pch = 4, col = c(7,12), main = "boxplot of score in G3 by Address")

# Famsize: Greater than 3 (GT3) or Less than 3 (LE3)
tfam = table(famsize)
round(prop.table(tfam),3)*100
par(mfrow = c(1,2))
pie(round(prop.table(tfam),3)*100, labels = paste0(names(round(prop.table(tfam),3)*100)," : ",round(prop.table(tfam),3)*100," %"), col = c(7,12))
boxplot(G3~famsize, cex = 0.5, pch = 4, col = c(7,12), main = "boxplot of score in G3 by Size of the family")

# # Pstatus: Parents Apart (A) or Together (T)
tsta = table(Pstatus)
round(prop.table(tsta),3)*100
pie(round(prop.table(tsta),3)*100, labels = paste0(names(round(prop.table(tsta),3)*100)," : ",round(prop.table(tsta),3)*100," %"), col = c(7,12))
boxplot(G3~Pstatus, cex = 0.5, pch = 4, col = c(7,12), main = "boxplot of score in G3 by Parent statis")

#Medu: Mother Education Level (0 - none, 1 - primary education (4th grade), 2 – from 5th to 9th grade, 3 – secondary education or 4 – higher education)
tmed = table(Medu)
round(prop.table(tmed),3)*100
pie(round(prop.table(tmed),3)*100, labels = paste0(names(round(prop.table(tmed),3)*100)," : ",round(prop.table(tmed),3)*100," %"), col = c(7,12))
boxplot(G3~Medu, cex = 0.5, pch = 4, col = c(7,8,12,13,14), main = "boxplot of score in G3 by Medu")

# Fedu: Father Education Level (0 - none, 1 - primary education (4th grade), 2 – from 5th to 9th grade, 3 – secondary education or 4 – higher education)
tfed = table(Fedu)
round(prop.table(tfed),3)*100
pie(round(prop.table(tfed),3)*100, labels = paste0(names(round(prop.table(tfed),3)*100)," : ",round(prop.table(tfed),3)*100," %"), col = c(7:12))
boxplot(G3~Fedu, cex = 0.5, pch = 4, col = c(7,8,12,13,14), main = "boxplot of score in G3 by Fedu")

# Mjob: Mother Job ("teacher", "health" ,"services" (e.g. administrative or police), "at_home" or "other")
tmjo = table(Mjob)
round(prop.table(tmjo),3)*100
pie(round(prop.table(tmjo),3)*100, labels = paste0(names(round(prop.table(tmjo),3)*100)," : ",round(prop.table(tmjo),3)*100," %"), col = c(7:12))
boxplot(G3~Mjob, cex = 0.5, pch = 4, col = c(7,8,12,13,14), main = "boxplot of score in G3 by Mjob")

# Fjob: Father Job ("teacher", "health" ,"services" (e.g. administrative or police), "at_home" or "other"))
tfjo = table(Fjob)
round(prop.table(tfjo),3)*100
pie(round(prop.table(tfjo),3)*100, labels = paste0(names(round(prop.table(tfjo),3)*100)," : ",round(prop.table(tfjo),3)*100," %"), col = c(7:12))
boxplot(G3~Fjob, cex = 0.5, pch = 4, col = c(7,8,12,13,14), main = "boxplot of score in G3 by Mjob")

#guardian: Who watches over the student (Father, Mother, Other)
tguar = table(guardian)
round(prop.table(tguar),3)*100
barplot(tguar, col = c("lightblue","yellow","green","blue"))
boxplot(G3~guardian, col = c("lightblue","yellow","green"), main = "boxplot of score in G3 by Guardian", pch = 4 , cex = 0.5)

# famsup: if student receives family educational support (yes or no)
famsup = as.factor(famsup)
famsup_t = table(famsup)
pie(famsup_t, main="family education support", col=c("lightgreen", "lightblue"))
boxplot(G3~famsup, col=c("lightgreen", "lightblue"), pch = 19)

#internet: if student has internet access (yes or no)
internet_t = table(internet)
internet_percent = prop.table(internet_t)*100
pie(internet_percent, col = c(7,5), main="Internet access from home", labels=paste0(names(internet_percent),":", round(internet_percent, 1), "%"))
boxplot(G3~internet, cex=0.7, col = c(7,5))

# Famrel: ranking of family relationship (1 - 5)
famrel = as.factor(famrel)
famrel_t = table(famrel)
barplot(famrel_t, main="family relationships", col=c("pink", "lightblue", "lightgreen", "lavender", "lightyellow"))
boxplot(G3~famrel, col=c("pink", "lightblue", "lightgreen", "lavender", "lightyellow"), pch = 19, )

###### School Aspect ######

# School: Which Portuguese Schoool: Gabriel Pereira (GP) or Mousinho da Silveira (MS)
tsch = table(school)
round(prop.table(tsch),3)*100
pie(round(prop.table(tsch),3)*100, labels = paste0(names(round(prop.table(tsch),3)*100)," : ",round(prop.table(tsch),3)*100," %"), col = c(7,12))
boxplot(G3~school, cex = 0.5, pch = 4, col = c(7,12), main = "boxplot of score in G3 by sex")

#failures: number of failed courses
tfail = table(failures)
round(prop.table(tfail),3)*100
barplot(tfail, col = c("lightblue","yellow","green","blue"))
boxplot(G3~failures, col = c("lightblue","yellow","green","blue"), main = "boxplot of score in G3 by failures", pch = 4)

# schoolsup: if student receives school educational support (yes or no)
schoolsup_t = prop.table(table(schoolsup))
pie(schoolsup_t, main="school education support", col=c("lightgreen", "lightblue"))
boxplot(G3~schoolsup, col=c("lightgreen", "lightblue"), pch = 19)

# paid: if student paid for extra classes (yes or no)
paid_t = prop.table(table(paid))
pie(paid_t, main="extra paid classes", col=c("lightgreen", "lightblue"))
boxplot(G3~paid, col=c("lightgreen", "lightblue"), pch = 19)

# activities: if student participates in activities outside of school (yes or no)
activities_t=table(activities)
pie(activities_t, main="Activities", col=c("lightgreen", "lightblue"))
boxplot(G3~activities, col=c("lightgreen", "lightblue"), pch = 19)

##nursery: if student attended nursery school (yes or no)
nursery_t = table(nursery)
nursery_percent = prop.table(nursery_t)*100
pie(nursery_percent, main="attended nursery", labels = paste0(names(nursery_percent), ": ", round(nursery_percent, 1), "%"), col=c("7","5"))
boxplot(G3~nursery, cex = 0.7, col=c("7","5"), pch = 19)

# G1: score of first semester (1 to 20)
tg1 = table(G1)
round(prop.table(tg1),3)*100
hist(G1, col = "lightblue", main = "histogram of score in G1")
abline(v = mean(G1), col = "green")
boxplot(G1, col = "lightblue", main = "boxplot of score in G1", pch = 4)
abline(h = mean(G1), col = "green")

# G2: score of second semester (1 to 20)
tg2 = table(G2)
round(prop.table(tg2),3)*100
hist(G2, col = "lightblue", main = "histogram of score in G2")
abline(v = mean(G2), col = "green")
boxplot(G2, col = "lightblue", main = "boxplot of score in G2", pch = 4)
abline(h = mean(G2), col = "green")

# G3: final grade (1 to 20)
tg3 = table(G3)
round(prop.table(tg3),3)*100
hist(G3, col = "lightblue", main = "histogram of score in G3")
abline(v = mean(G3), col = "green", lwd = 3)
boxplot(G3, col = "lightblue", main = "boxplot of score in G3", pch = 4)
abline(h = mean(G3), col = "green", lwd = 3)

###### Social Aspect ######

#romantic: if student is in a romantic relationship (yes or no)
romantic_t=table(romantic)
romantic_percent=prop.table(romantic_t)*100
pie(romantic_percent, main="Students on a relationship", col = c(7,5), labels=paste0(names(romantic_percent),":", round(romantic_percent, 1), "%"))
boxplot(G3~romantic, col = c(7,5), pch = 19)

# freetime: freetime after school ranking (1 to 5)
freetime_mean = aggregate(G3~D$freetime, FUN = mean)
barplot(freetime_mean$G3, col="lightblue", names.arg = c(1, 2, 3, 4, 5), main="avg G3 score by free time ranking")
abline(h = mean(G3), col="red",  lwd=2)

# goout: goout with friends ranking (1 to 5)
goout_mean = aggregate(G3~D$goout, FUN = mean)
barplot(goout_mean$G3, col="lightblue", names.arg = c(1, 2, 3, 4, 5), main="avg G3 score by going out ranking")
abline(h = mean(G3), col="red",  lwd=2)

# Dalc: alcohol consumption during weekdays ranking (1 to 5)
dalc_mean = aggregate(G3~D$Dalc, FUN = mean)
barplot(dalc_mean$G3, col="lightblue", names.arg = c(1, 2, 3, 4, 5), main="avg G3 score by drinking during the week rank")
abline(h = mean(G3), col="red",  lwd=2)

# Walc: alcohol consumption during weekends ranking (1 to 5)
l = aggregate(G3~Walc, FUN = mean)
barplot(l$G3, horiz = F,col = "lightblue", names.arg  = c(l$Walc),ylim = c(0,14), main = "Group mean of G3 by Walc")
abline(h = mean(G3), col = "black", lwd = 3)

# health: health status ranking (1 to 5)
l = aggregate(G3~health, FUN = mean)
barplot(l$G3, horiz = F,col = "lightblue", names.arg  = c(l$health),ylim = c(0,14), main = "Group mean of G3 by Level of Health")
abline(h = mean(G3), col = "black", lwd = 3)

##### CORRELATION #####
# is there any correlation between variables?

### Individual Aspect ###

# sex: Thanks to this model we can understand that there is a positive and significant relation between the final vote and the variable 'sex'. if a student is male the final vote increase of 0.95
G3_by_sex = aggregate(G3~sex, FUN = mean, data = D)
G3_by_sex
t.test(G3~sex, data=data)
aov(G3~sex, data=data)
sex <- lm (G3~sex, data=D)
summary(sex)

# reason: there is not a significant relation between the final vote and the variable 'reason'.
G3_by_reason = aggregate(G3~reason, FUN = mean, data = D)
G3_by_reason
chisq.test(G3, reason)
reason <- lm (G3~reason, data=D)
summary(reason)

# traveltime: there is a significant relation between the final vote and traveltime, infact for each unit increase in traveltime, the final grade decreases by -0.7694.
G3_by_traveltime = aggregate(G3~traveltime, FUN = mean, data = D)
G3_by_traveltime
chisq.test(G3, traveltime)
aov(G3~traveltime)
traveltime <- lm (G3~traveltime, data=D)
summary(traveltime)

# studytime: unlike the previous case, in this case the relationship between the variables is positive, in fact for every unit increase in studytime the final grade increases by 0.5340 
G3_by_studytime = aggregate(G3~studytime, FUN = mean, data = D)
G3_by_studytime
chisq.test(G3, studytime)
aov(G3~studytime)
studytime <- lm (G3~studytime, data=D)
summary(studytime)

# higher: in this case the relation between the wo variable is significant. if a student want to continue with the study, the final vote increase of 3.8
G3_by_higher = aggregate(G3~higher, FUN = mean, data = D)
G3_by_higher
higher <- lm (G3~higher, data=D)
summary(higher)

# absences: variable not significant
absences <- lm (G3~absences, data=D)
summary(absences)

# age: variable is significant. the relation between the age of the student and his final vote is negative. for each unit increment of age, the final vote decrese of -0.5801. 
age <- lm (G3~age, data=D)
summary(age)

# correlation_matrix of age, traveltime, studytime, and absences
correlation_matrix <- cor(data[, c("age", "traveltime", "studytime", "absences")])
print(correlation_matrix)

# further analysis: study time vs gender
boxplot(studytime ~ sex, data = data, xlab = "Sex", ylab = "Study Time", main = "Study Time by Gender", horizontal = T)
plot(data$studytime, data$absences, col = ifelse(data$sex == "M", "blue", "red"), 
     xlab = "Study Time", ylab = "Absences", main = "Scatterplot di Study Time vs Absences")
legend("topright", legend = unique(data$sex), col = c("blue", "red"), pch = 1)


### Family Background ###

# Education level of family
# is there a relationship between Medu and Fedu?

# there is a positive and consistent correlation between the variables 
table(Medu,Fedu)
cor(Medu,Fedu)
prop_Familyedu = round(prop.table(table(Medu,Fedu))*100,2)
prop_Familyedu

# Fathers with the highest level of education are 9% more than mothers at the same level
# Considering the level of education 1 and 2, Mothers have an higher percentage 
sum(diag(prop_Familyedu))
colSums(prop_Familyedu) #Fedu
rowSums(prop_Familyedu) #Medu

# Total Education level: add Fedu and Medu variables to obtain a new variable of the sum of father and mother education
D$Family_Edu = Fedu+Medu
attach(D)
summary(Family_Edu)
boxplot(G3~Family_Edu, pch = 19, col = c(8:16))
aggregate(G3~Family_Edu, FUN = mean)

# the effect is positive, if Family edu increase G3 increase:
# increasing the level of education by 1, G3 score increase by 0.47 points
mod = lm(G3~Family_Edu) 
summary(mod)

# the effect of Family_edu alone is more significative then the single variables
mod1 = lm(G3~Family_Edu+Fedu+Medu) 
summary(mod1)


# Average Family Education Level: new variable of average of mother and father education level
D$Avarage_education_level = (Fedu+Medu)/2
attach(D)
summary(Avarage_education_level)
boxplot(G3~Family_Edu, pch = 19, col = c(8:16))
aggregate(G3~Family_Edu, FUN = mean)

# the effect is positive, if the avarage level of instruction increase G3 increase:
# increasing the avarage level of education by 1, G3 score increase by 0.95 points
mod = lm(G3~Avarage_education_level)
summary(mod)

# the effect of Family_edu alone is more significative then the single variables
mod1 = lm(G3~Family_Edu+Fedu+Medu)
summary(mod1)

## 2: Status of the family

# FAMSIZE
D$famsize = factor(famsize)
levels(D$famsize) = c("2-3",">3")

# FAMSIZE AND ADDRESS: # bigger families live in urban areas: urban citiz. are 4.6 more then rural citiz.
# smaller families prefer urban areas too: the proportion is more balanced (3.1)
aggregate(G3~address, FUN = length)
table(famsize,address)
addmargins(round(prop.table(table(famsize,address))*100,1))

# ADDRESS AND FAMSIZE:  the status divorced balance the distribution of
# familysize, 10% of the observation live in divorced family (5.3% have size
# less then 3, 5.1% higher then 3)
addmargins(round(prop.table(table(famsize,Pstatus))*100,1))

# FAMREL AND PSTATUS: the parental status don't affect the quality of the relationship,
# the ratio between the modalities of famrel is the same for A & T
addmargins(round(prop.table(table(famrel,Pstatus))*100,1))

## FAMILY EMPLOYMENT STATUS

# FJOB AND MJOB #

plot(Fjob, ylim = c(0,250))
plot(Mjob, add = T, col = "lightblue")

aggregate(G3~Fjob, FUN = mean)
aggregate(G3~Mjob, FUN = mean)
boxplot(G3~Fjob, pch = 19, cex = 0.5)
boxplot(G3~Mjob, pch = 19, cex = 0.5, add = T, col = "lightgreen")

# 43.5% of the parents have the same job
# female teachers are double compared to male teachers
# fathers at home are 1/3 respects to mothers at home
# 11% of the families work in the service sector
tabjob = round(addmargins(prop.table(table(Mjob,Fjob))*100),1)
tabjob
sum(diag(tabjob)) - 100

# FAMILY OCCUPATION: new variable to account for combination of both mother and father jobs

D$Family_occupation = paste(D$Mjob, D$Fjob, sep = "-")
D$Mjob = as.character(D$Mjob)
D$Fjob = as.character(D$Fjob)
D$Family_occupation = ifelse(D$Mjob == D$Fjob, D$Mjob, paste(D$Mjob, D$Fjob, sep = "-"))
D$Family_occupation = factor(D$Family_occupation, order = T)
attach(D)

boxplot(G3 ~ Family_occupation, data = D, pch = 19, cex = 0.4, las = 2, xlab = "")

# the effect of the family occupation now is stronger
# To isolate the effect of the family work lets plot only the family
# which have the parents with the same work
mod = lm(G3~ Family_occupation)
summary(mod)

# FAMSUP AND FAMREL

plot(prop.table(table(famrel)), ylim = c(0,1))
abline(h = 0.5, col = "lightgreen")
abline(h = 0.75, col = "lightgreen")
abline(h = 0.25, col = "lightgreen")

tabfam = round(addmargins(prop.table(table(famsup,famrel))*100),1)
tabfam

# FAMSUP AND FAMSIZE

table(famsup,famsize)
round(addmargins(prop.table(table(famsup,famsize))*100),1)

# FAMILY WELLBEING #
## The amount of the indicator is based on the evaluation of the quality of family relationship: 1-5
## if the student doesn't recieve any support from the family: Family_Wellbeing +2
#  if not: Family_Wellbeing + 1
## if extra lesson are payed: Family_wellbeing + 2
#  if not: Family_Wellbeing + 1

D$paid = factor(paid)
levels(D$paid)
levels(D$paid) = c(0,1)

D$Family_Wellbeing = as.numeric(D$famrel) + as.numeric(D$famsup) + as.numeric(D$paid)
attach(D)
summary(Family_Wellbeing)

mod = lm(G3~Family_Wellbeing)
summary(mod)

# INTERNET AND FAMILY EDUCATION
boxplot(Family_Edu~internet, pch = 19, cex = 0.3)
aggregate(as.numeric(internet)~D$Avarage_education_level, FUN = mean)
# increasing the level of education, in average the possibility to have an home Wi-Fi increase

table(internet,Family_Wellbeing)
addmargins(round(prop.table(table(internet,Family_Wellbeing)*100),2))
# having WiFi connection increase the Family_ Wellbeing

# FAMILY WELLBEING, WITH WIFI ADDED
## The amount of the indicator is based on the evaluation of the quality of family relationship: 1-5
## if the student doesn't recieve any support from the family: Family_Wellbeing +2
#  if not: Family_Wellbeing + 1
## if extra lesson are payed: Family_wellbeing + 2
#  if not: Family_Wellbeing + 1
## if the student have home WiFi: Family_Wellbeing + 2
#  if not: Family_Wellbeing + 1

D$Family_Wellbeing = as.numeric(D$famrel) + as.numeric(D$famsup) + as.numeric(D$paid) + as.numeric(D$internet)
attach(D)
summary(D$Family_Wellbeing)


# GUARDIAN AND INTERNET
table(internet,guardian)
trow = addmargins(round(prop.table(table(internet,guardian),1)*100,1))
tcol = addmargins(round(prop.table(table(internet,guardian),2)*100,1))
trow
tcol

#### SCHOOL ASPECT ####

D$schoolsup=factor(schoolsup)
levels(schoolsup)=c(0,1)
levels(schoolsup)
schoolsup_t<-table(schoolsup)


D$paid=factor(paid)
levels(paid)=C(0,1)
levels(paid)
paid_t<-table(paid)

D$activities=factor(activities)
levels(activities)=c(0,1)
levels(activities)
activities_t<-table(activities)

# school support and paid for extra classes
table_contingency <- table(schoolsup, paid)
print(table_contingency)

# paid for extra classes and activities participation
table_contingency2 <- table(paid, activities)
print(table_contingency2)

# school support and activities participation
table_contingency3<- table(schoolsup, activities)
print(table_contingency3)

# EXTRA ACTIVITIES: new variable to account for school support, paying for extra classes, and
# participating in activities 
D$extra_activities=as.numeric(D$schoolsup)+as.numeric(D$paid)+as.numeric(D$activities)
boxplot(G3 ~ D$extra_activities)

mod=lm(G3 ~ D$extra_activities)
summary(mod)

plot(D$G3_standardized[D$extra_activities==3], cex=0.2)
points(D$G3_standardized[D$extra_activities==4], cex=0.2, pch=4, col="red")
abline(h=-1)
abline(h=1)
abline(h=0)

#average_score represents the weighted average of students scores on three 
#tests (G1,G2,G3). the weighting of the score is done using specified weight 
#of 0.15 for G1, 0.25 for G2, 0.6 for G3, because we gave more importance to 
#the final grade. 
#the variable "average_score" represents the weighted average of students' 
#scores on three tests, with the specified weights. 

weight=c(0.6,0.25,0.15)
D$average_score=weighted.mean(c(G3,G2,G1),weight)

average_score=G3*0.6+G2*0.25+G1*0.15
D$average_score=average_score

#student_commitment variable combines 4 variables: studytime, that is a numeric variable that has 4 values (1 if the weekly study hours are <2)
#2 if they are from 2 to 5, 3 if they are from 5 to 10 hours, and 4 if >10). Then we have activities that we have made 
#numerical with 2 values (0 or 1), we have done the same with higher. Since absence is a numeric variable 
#from 0 to 93 (the number of absences) we cut the variable in 5 breaks and we assign 5 to who made a lot of absences
#to 1 who made few of them. then we revers the orders of the value and create the variable_sintetica as 
#( 5 breaks+1) - breaks
#then the variable student_commitment we summed those variables and we multipled studytime by 2 
#because we gave more importance to the time spent studying

D$activites_num=as.numeric(D$activities)-1

D$higher_num=as.numeric(D$higher)-1

intervalli= cut(D$absences, breaks = 5, labels = FALSE)

D$intervalli=intervalli

variabile_sintetica = 6 - intervalli

D$student_commitment=(D$studytime*2)+D$activites_num+variabile_sintetica+D$higher_num
summary(D$student_commitment)

#### SOCIAL ASPECT ####

## FREETIME AND GOOUT ##

# is there a relationship between amount of freetime a student has and how often they go out with friends?

plot(freetime, goout) # not clear - make a table
freetime_goout_t<-table(freetime, goout)
freetime_goout_t

prop_total_freetime = round(prop.table(freetime_goout_t)*100,2)
prop_total_freetime

# 40% of students ranked freetime and goout the same value
sum(diag(prop_total_freetime))

# linear regression model: there is no statistically significant relationship between
# G3 and freetime rank
D$freetime<-as.factor(D$freetime)
model.matrix(~D$freetime-1, data=D)
D<-cbind(D, model.matrix(~D$freetime-1, data=D))
lm<-lm(G3~D$freetime, data=D)
summary(lm)

# there is a statistically significant relationship between G3 and goout rank:
# goout rank 2 and 3 are statistically significant
# goout rank 1 (don't go out with friends often) does not have an effect on G3
# therefore, it can be said that students who ranked goout as a 2 will score higher than 
# students who ranked goout as a 3
D$goout<-as.factor(D$goout)
model.matrix(~D$goout-1, data=D)
D<-cbind(D, model.matrix(~D$goout-1, data=D))
lm1<-lm(D$G3~D$goout, data=D)
summary(lm1)

## ROMANTIC AND FREETIME ##

# is there a relationship between being in a romantic relationship and the amount of 
# freetime a student has?
par(mfrow=c(1,1))
D$romantic<-ifelse(D$romantic== "yes", 1, 0)
freetime_rom_t<-table(romantic, freetime)
freetime_rom_t

# there is a higher frequency of students that are not in a romantic relationship (blue) 
hist(rep(1:5, times = freetime_rom_t[1,]), breaks=0:6, main="Romantic Relationship and Freetime", xlab="Freetime Ranking", col="lightblue", ylim=c(0, 120))
hist(rep(1:5, times=freetime_rom_t[2,]), breaks=0:6, add=T, col="pink")
legend("topright", legend=c("not in romantic relationship", "in romantic relationship"), fill=c("lightblue", "pink"))

# after performing a chi squared test on romantic and freetime, it can be inferred that there is not a 
# statistically significant relationship between romantic relationship and freetime (p > 0.05)
chisq.test(freetime_rom_t)

# however, being in a romantic relationship has a stat. significant 
# effect on G3 - if in a romantic relationship, G3 is 1.2607 points less
# than if not in romantic relationship
lm3<-lm(G3~D$romantic, data=D)
summary(lm3)


### ROMANTIC AND GOING OUT ###
# is there a relationship between being in a romantic relationship and how often
# a student goes out with friends? 

# make separate tables for romantic relationship or not, and go out ranking
goout_romantic<-table(D$romantic, D$goout)
goout_romantic_yes<-goout_romantic[2,]
goout_romantic_no<-goout_romantic[1,]

# there is a higher frequency of students that are not in a romantic relationship (blue)
hist(rep(1:5, times = goout_romantic_no), breaks=0:6, main="Romantic Relationship and Going Out", xlab="Goout Ranking", col="lightblue", ylim=c(0, 120))
hist(rep(1:5, times=goout_romantic_yes), breaks=0:6, add=T, col="pink")
legend("topright", legend=c("not in romantic relationship", "in romantic relationship"), fill=c("lightblue", "pink"))

# use wilcoxon ranked test to compare the if the goout ranking is affected by relationship status
# p value > 0.05: goout ranking does not depend on if in relationship or not
goout_relation_yes<-D[D$romantic==1,]
goout_relation_no<-D[D$romantic==0,]
wilcox.test(goout_romantic_no, goout_romantic_yes, alternative="two.sided")

# comparing goout ranking and G3 score for in a relationship or not in a relationship
# G3 score is higher for those students not in a relationship by goout ranking
boxplot(goout_relation_yes$G3~goout_relation_yes$goout, data=goout_relation_yes, main="G3 Score vs Ranking of Going out With Friends: In Relationship", col="pink", xlab="Goout Ranking", ylab="G3 Score", ylim=c(0,20))
boxplot(goout_relation_no$G3~goout_relation_no$goout, data=goout_relation_no, main="G3 Score vs Ranking of Going out With Friends: Not in Relationship", col="lightblue", xlab="Goout Ranking", ylab="G3 Score", ylim=c(0,20))
aggregate(goout_relation_yes$G3~goout_relation_yes$goout, FUN = mean)
aggregate(goout_relation_no$G3~goout_relation_no$goout, FUN = mean)

## DALC AND WALC ##

# is there a relationship between drinking alcohol during the week and during the weekend?

# create a barplot to compare Dalc and Walc frequencies. 
# most students do not drink alcohol during the weekdays, 
# while more students drink alcohol during the weekend
alcohol_t2<-rbind(table(Dalc), table(Walc))
alcohol_t2
barplot(alcohol_t2, beside = TRUE, col = c("lightblue", "pink"), main = "Dalc and Walc Rankings", xlab = "Ranking", ylab = "Frequency")
legend("topright", legend=c("Dalc", "Walc"), fill = c("lightblue", "pink"))

# Dalc ranking (individually) has no effect on G3, only when Dalc = 2
D$Dalc<-as.factor(D$Dalc)
model.matrix(~D$Dalc-1, data=D)
D<-cbind(D, model.matrix(~D$Dalc-1, data=D))

lm4<-lm(G3~D$Dalc, data=D)
summary(lm4)

# Walc ranking (individually) also has no effect on G3
D$Walc<-as.factor(D$Walc)
model.matrix(~D$Walc-1, data=D)
D<-cbind(D, model.matrix(~D$Walc-1, data=D))

lm5<-lm(G3~D$Walc, data=D)
summary(lm5)



D$total_alc<-Dalc + Walc
attach(D)
table(total_alc)
barplot(table(total_alc), main = "Total alcohol consumption", xlab="combined ranking", ylab="frequency")

# AVG_ALC - average alcohol consumption between weekend and weekday
D$avg_alc<-(as.numeric(D$Dalc) + as.numeric(D$Walc)) / 2
attach(D)

# there are "half values" (1.5, 2.5, etc): to avoid a "half ranking", 
# categorize the half values to the higher of the two rankings 
table(D$avg_alc)
D$avg_alc[D$avg_alc == 1.5] <- 2
D$avg_alc[D$avg_alc == 2.5] <- 3
D$avg_alc[D$avg_alc == 3.5] <- 4
D$avg_alc[D$avg_alc == 4.5] <- 5

# new categorization
barplot(table(D$avg_alc), main = "avg alcohol consumption throughout the week", xlab="average ranking", ylab="frequency")
boxplot(G3 ~ D$avg_alc, data = D)

table(D$avg_alc)

# avg alcohol consumption throughout the week also has no effect on G3
lm8<-lm(G3~D$avg_alc, data=D)
summary(lm8)

D$avg_alc<-as.factor(D$avg_alc)
model.matrix(~D$avg_alc-1, data=D)
D<-cbind(D, model.matrix(~D$avg_alc-1, data=D))

#lm9<-lm(G3~D$`D$avg1`+ D$`D$avg2`+ D$`D$avg3`+ D$`D$avg4`, data = D)
lm9<-lm(G3~D$avg_alc, data=D)
summary(lm9)

### HEALTH AND ALCOHOL ###

# does health have any relationship with alcohol? Dalc, Walc, avg alc
plot(freetime, goout) # not clear - make a table
freetime_goout_t<-table(freetime, goout)
freetime_goout_t

prop_total_freetime = round(prop.table(freetime_goout_t)*100,2)
prop_total_freetime

# looking at the prop table for avg alcohol ranking: 11.65% students
# reported very good health status (5) and low average alcohol consumption
# avg_alc = 1 , avg_alc = 2
health_avgalcohol<-table(health, D$avg_alc)
health_avgalcohol

prop_avg_alcohol = round(prop.table(health_avgalcohol)*100, 2)
prop_avg_alcohol

# health ranking of 3 and 4 have an effect on G3 score
D$health<-as.factor(D$health)
model.matrix(~D$health-1, data=D)
D<-cbind(D, model.matrix(~D$health-1, data=D))

lm10<-lm(G3~D$health, data=D)
summary(lm10)

# now, combine health and avg_alc: health_avgalc : average of health ranking and alcohol consumption
D$health_avgalc<-(as.numeric(D$health) + as.numeric(D$avg_alc)) / 2
attach(D)

# there are "half values" (1.5, 2.5, etc): to avoid a "half ranking", 
# categorize the half values to the higher of the two rankings 
table(D$health_avgalc)
D$health_avgalc[D$health_avgalc == 1.5] <- 2
D$health_avgalc[D$health_avgalc == 2.5] <- 3
D$health_avgalc[D$health_avgalc == 3.5] <- 4
D$health_avgalc[D$health_avgalc == 4.5] <- 5

barplot(table(D$health_avgalc), main = "avg alcohol consumption throughout the week and health score", xlab="average ranking", ylab="frequency")
boxplot(G3 ~ D$health_avgalc, data = D)

# lin reg: average health ranking and alcohol consumption of 4 has a significant effect on G3
D$health_avgalc<-as.factor(D$health_avgalc)
model.matrix(~D$health_avgalc-1, data=D)
D<-cbind(D, model.matrix(~D$health_avgalc-1, data=D))

lm11<-lm(G3~D$health_avgalc, data=D)
summary(lm11)

###### further analysis of correlation between variables ######

# G1,G2

# G1 and G2: 0.85 correlation
G1_G2 = cor(G1,G2)
cor.test(G1, G2)

# G2 and G3: 0.91 correlation
G2_G3 = cor(G2,G3)
cor.test(G2, G3)

# G1 and G3: 0.80 correlation
G1_G3 = cor(G1,G3)
cor.test(G1, G3)

# visualize as heatmap
cor_mat = cor(data.frame(G1,G2,G3))
corrplot(cor_mat)

# absences and G3: 0.034 correlation
cor(absences,G3)
cor.test(absences,G3)

# create a correlation heatmap of G1, G2, G3, failures, and absences - all quantitative variables of dataset
cor_mat1 = cor(data.frame(G1,G2, G3,failures,absences))
corrplot(cor_mat1, method = "color")


#### Model ####

##            Logistic Model: stepwise variable selection             ##

D$G3_binary = ifelse(D$G3 < 10, 0, 1)
attach(D)
table(G3_binary)
plot(G3_binary, cex = 0.2)
barplot(table(G3_binary), col = c("lavender","lightyellow"), ylim = c(0,300), main = "Boxplot of frequency: success/failure in G3")
legend("topleft", col = c("lavender","lightyellow"), fill = c("lavender","lightyellow"), legend = c("1: Pass","0: Not pass"))
box()

# Stepwise method

##            Logistic Model: Original dataset selction           ##
Data_regr = D[,c(-33)]
empty_mod = lm(G3_binary ~ 1, Data_regr)
full_mod = lm(G3_binary ~ ., Data_regr)

# 1: Forward Selection

mod_forward = step(empty_mod, direction='forward', scope=formula(full_mod), trace=0)
summary(mod_forward)

mod_forward$anova

# 2: Backward Selection

mod_backward = step(full_mod, direction='backward', scope=formula(empty_mod), trace=0)
summary(mod_backward)

mod_backward$anova

# 3: Both Selection

mod_both = step(empty_mod, direction='both', scope=formula(full_mod), trace=0)
summary(mod_both)

mod_both$anova

##            Logistic Model: New Dataset selection             ##
attach(D)
New_dataset = data.frame(Family_Edu,Family_occupation,Family_Wellbeing,Avarage_education_level,extra_activities,student_commitment,total_alc,avg_alc,avg_alc,G3)
New_dataset$Family_Wellbeing = as.numeric(Family_Wellbeing)
New_dataset$Family_Edu = as.numeric(Family_Edu)
New_dataset$Avarage_education_level = as.numeric(Avarage_education_level)
New_dataset$extra_activities = as.numeric(extra_activities)
New_dataset$student_commitment = as.numeric(student_commitment)
New_dataset$total_alc = as.numeric(total_alc)
New_dataset$avg_alc = as.numeric(Family_Wellbeing)
New_dataset = New_dataset[,c(-9)]
attach(New_dataset)

empty_mod = lm(G3 ~ 1, New_dataset)
full_mod = lm(G3 ~ ., New_dataset)

# 1: Forward Selection

mod_forward = step(empty_mod, direction='forward', scope=formula(full_mod), trace=0)
summary(mod_forward)

mod_forward$anova

# 2: Backward Selection

mod_backward = step(full_mod, direction='backward', scope=formula(empty_mod), trace=0)
summary(mod_backward)

mod_backward$anova

# 3: Both Selection

mod_both = step(empty_mod, direction='both', scope=formula(full_mod), trace=0)
summary(mod_both)

mod_both$anova

##   final model: influence of the family education level and the student commitment on G3   ##

mod_final = lm(G3~Family_Edu+student_commitment, data = New_dataset)
summary(mod_final)
