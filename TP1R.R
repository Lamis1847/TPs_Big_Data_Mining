str(cars)
plot(cars, xlab = "Speed (mph)", ylab = "Stopping distance (ft)", las = 1)
lines(lowess(cars$speed, cars$dist, f = 2/3, iter = 3), col = "red")

plot(cars, xlab = "Speed (mph)", ylab = "Stopping distance (ft)", las = 1, log = "xy")
lines(lowess(cars$speed, cars$dist, f = 2/3, iter = 3), col = "red")
#2eme partie

cars$dist <-cars$dist* 0.3048
cars$dist_m <- cars$dist * 0.3048
head(cars)
data1= scale(cars)
boxplot(data1)

summary(cars)

normalize <- function(x) {
    return((x - min(x)) /(max(x) - min(x)))
}


data2=normalize(cars)
boxplot(data2)

cars$speed_catego2 <- cut(cars$speed, breaks = c(-Inf, 10, 20, Inf), right = FALSE)
str(cars)
cars$speed_catego2
cars[c(5:7,38:40), c("speed","speed_catego2")]
#exemple2

v1 <- c(2,3,4,1,2,3,5,6,5,4)
v2 <- c(1,4,2,3,5,4,6,2,5,3)
Etudiants <- rep(c("SIT", "SIL"), each = 5)
data <- data.frame(v1, v2, Etudiants)
data
data$v2[7] <- NA
data$Etudiants[3] <- NA
data1 <-data.frame(lateralite = rep(c("gaucher", "droitier"), each = 5),age = rep(c(22, 51), each = 5))
data1
data2 <-data.frame(region = rep(c("Bejaia", "Jijel"), each = 5),age = rep(c(22, 51), each = 5))
data2
cbind(data, data1, data2)
data3=data.frame(data, data1, data2)
supp <- data.frame(v1 = c(2,5), v2 = c(5, 6), Etudiants = c("SIT", "SIL"), row.names = paste0("l", 11:12))
na.omit(data3)
or=order(data3$v2)
data3[or,]
#gestion des valeurs manquantes
dataset <-airquality
airquality
sapply(dataset, function(x) length(unique(x)))
sapply(dataset,function(x) sum(is.na(x)))
colonne=dataset$Solar.R
sum(is.na(colonne))
colonne1=dataset$Ozone
sum(is.na(colonne1))
library(Amelia)
missmap(dataset, main = "Missing values vs observed")
   dataset$Colonne <- NULL
dataset_2 <- subset(dataset,!is.na(colonne))
dataset$colonne[is.na(colonne)] <- mean(colonne,na.rm=T)
colonne
dataset$colonne[is.na(colonne)] <- median(colonne,na.rm=T)
colonne