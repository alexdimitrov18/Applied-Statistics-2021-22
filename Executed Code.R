
rm(list=ls())
# Зареждане данните
data <- read.csv("insurance.csv") 

# Представяме нечисловите стойности като фактори
data$sex <- as.factor(data$sex)
data$smoker <- as.factor(data$smoker)
data$region <- as.factor(data$region)

# Вземаме всички фактори и ги преобразуваме в числови данни за по-лесна обработка
indx <- sapply(data, is.factor)
data[indx] <- lapply(data[indx], function(x)as.numeric(x))

# Извеждаме си основна информация за обработената извадка
summary(data)

# Изчисляваме корелацията между отделните данните
cor(data)

# Тестов модел за проверка на крайните модели - ако не разделяме на непушачи и пушачи
testmodel11 <- lm(data$charges~data$age+data$bmi+data$smoker)
plot(testmodel11)
summary(testmodel11)

# Разделяме извадката на 2 части data1 - пушачи, data2 - непушачи
data1<-data[data$smoker==2,]
data2<-data[data$smoker==1,]

data1$smoker<-NULL
data2$smoker<-NULL


# Работа с даннните за пушачи

## Проверяваме какви са връзките между данните за пушачи
cor(data1)

## Визуална представа за разпределението на разходи/възраст и разходи/BMI
plot(data1$age, data1$charges, main="Възраст спрямо разходи", xlab = "Възраст на пушач", ylab= "Медицински разходи")
plot(data1$bmi, data1$charges, main="BMI спрямо разходи", xlab = "BMI на пушач", ylab= "Медицински разходи")

## Тестов модел за сравняване - ако не групираме по BMI

testmodel1 <- lm(data1$charges~data1$age+data1$bmi)
plot(testmodel1)
summary(testmodel1)

## Разделяме си извадката с пушачите на две части - data11 - тези с наднормено тегло, data12 - тези с нормално тегло
## Извличане на хората с наднормено тегло и филтриране на излишните данни
data11 <-data1[data1$bmi>=30,]
data11 <-data11[!(data11$age>=40&data11$age<60&data11$charges>55000),]
data11 <-data11[!(data11$charges<30000),]
data11 <-data11[!(data11$age>=20&data11$age<40&data11$charges>50000),]

## Финален изглед на данните
plot(data11$age, data11$charges,main="Възраст спрямо разходи при наднормно тегло", xlab = "Възраст на пушач", ylab= "Медицински разходи")
plot(data11$bmi, data11$charges,main="BMI спрямо разходи при наднормно тегло", xlab = "BMI на пушач", ylab= "Медицински разходи")
#Корелацията им
cor(data11)

## Премахваме хората с наднормено/поднормено тегло и филтриране на излишните данни
data12 <- data1[data1$bmi<30,]
data12<- data12[data12$bmi>18,]
data12<- data12[!(data12$age>30&data12$age<50&data12$charges>30000),]
data12<- data12[!(data12$age<30&data12$charges>25000),]

#Корелацията им
cor(data12)

## Финален изглед на данните
plot(data12$age, data12$charges,main="Възраст спрямо разходи при нормално тегло", xlab = "Възраст на пушач", ylab= "Медицински разходи")
plot(data12$bmi, data12$charges,main="Възраст спрямо разходи при нормално тегло", xlab = "BMI на пушач", ylab= "Медицински разходи")


## Модел за изчисляване на медицинските разходи на пушачи с наднормено тегло
model11<-lm(data11$charges~data11$age+data11$bmi)
plot(model11)
summary(model11)

## Модел за изчисляване на медицинските разходи на пушачи с нормално тегло тегло 
model12<-lm(data12$charges~data12$age+data12$bmi)
plot(model12)
summary(model12)

# Работа с данните за непушачи
cor(data2)
plot(data2$age,data2$charges,main="Възраст спрямо разходи", xlab = "Възраст на непушач", ylab= "Медицински разходи")
abline(model2,col="red",lwd=3)


## Създаване на модел без филтрирани излишните данни
model2<-lm(data2$charges~data2$age)
plot(model2)
summary(model2)

## Филтриране и визуализиране на филтрираните данни. Филтрирането е на база създадения горен модел. 
data222<-data2[abs(data2$charges-((data2$age)*267.25-2091.42))<4000,]
plot(data222$age,data222$charges,main="Възраст спрямо разходи", xlab = "Възраст на непушач", ylab= "Медицински разходи")

## Съзадаване на модел върху последните дани
model222<-lm(data222$charges~data222$age)
plot(model222)
summary(model222)

## Визуализиране на регресионната права върху данните
plot(data222$age,data222$charges,main="Възраст спрямо разходи", xlab = "Възраст на непушач", ylab= "Медицински разходи")
abline(model222,col="red",lwd=3)


# Допълнителни plot-ове за документацията

barplot(table(data$age), main="Брой спрямо възраст", xlab="Възраст", ylab="Брой")
summary(data$age)

barplot(table(data$sex), main="Брой спрямо пол", xlab="Пол", ylab="Брой",ylim=c(0,700))

barplot(table(data$region), main="Брой спрямо регион", xlab="Регион", ylab="Брой",ylim=c(0,400))

barplot(table(data$smoker), main="Брой спрямо пушене", xlab="Категория", ylab="Брой",ylim=c(0,1300), name=c("Непушачи", "Пушачи"))

barplot(table(data$children), main="Брой спрямо деца", xlab="Брой деца", ylab="Брой",ylim=c(0,600))
summary(data$children)

cc <- cut(data$bmi, breaks=c(seq(15,54,4)), include.lowest=TRUE)
barplot(table(cc), main="Брой спрямо индекс на телесна маса", xlab="BMI", ylab="Брой",ylim=c(0,350))
summary(data$bmi)

dd <- cut(data$charges, breaks = c(seq(1122,63770,12529.6)))
summary(data$charges)
barplot(table(dd), main="Брой спрямо разходи", xlab="Разход", ylab="Брой",ylim=c(0,1000))
summary(data$charges)



