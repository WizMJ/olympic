
setwd("D:/R/Olympic/winter data")
library(dplyr)
library(AER)
library(ggplot2)

year <- seq(1994,2014,4)
year <- c(1992,year)
data <- c()
for (i in year){
        file.name <-paste0("winter_df",i,".csv")
        temp_df <- read.csv(file.name,header=T,sep=',')
        temp_df$year <- as.factor(i)
        data <- rbind(data,temp_df)
}
data <- data[,-1]

year <- year[-1]
tidy.data <- c()
for (i in year){
        if (i == 1994){m <- 2
        } else { m <- 4}
        sub.data1 <- subset(data, select = c(Country, Total), year == i - m)
        colnames(sub.data1)[2] <- "pre_Total"
        sub.data1$pre_Total_s <- sub.data1$pre_Total/sum(sub.data1$pre_Total, na.rm = TRUE)*100
        sub.data2 <- subset(data, year == i)
        tidy.data <- rbind(tidy.data, merge(sub.data2,sub.data1,all.x=TRUE, by ="Country"))
}


city_temp <- read.csv("D:/R/olympic/city temp/country temp.csv", header =T, sep =",")
temp <- merge(tidy.data, city_temp[,c("Country","Avg_temp","Min_temp")], all.x=TRUE, by="Country")
tidy.data <-  arrange(temp, year)

### factor cases for city temp: Cold, Warm, Hot
#Case1: <=-5 for Cold, >=10 for Hot
tidy.data$Min_temp <- tidy.data$Min_temp <- ifelse(tidy.data$Min_temp <=-5, "Cold",
                              ifelse(tidy.data$Min_temp >=10, "Hot", "Warm"))
tidy.data$Min_temp <- as.factor(tidy.data$Min_temp)
tidy.data$Min_temp <- relevel(tidy.data$Min_temp, ref="Warm")

###------------tidy data completed -------------
### All NA's are removed and then simulate
### Linear Regressoin
coef_pval <- c()
for (i in year){
        train <- subset(tidy.data, year ==i)
        train$Total_s <- train$Total/sum(train$Total, na.rm = TRUE)
        train <- na.omit(train)
        temp <- summary(lm(Total_s ~ pre_Total_s + log(GDP) + log(POP) +Min_temp,
                           data = train))$coefficients[,c(1,4)]
        coef_pval <- cbind(coef_pval,temp)
}

name <- c()
for (i in year){
        name <- c(name, paste(i,"Coef"),paste(i,"p-value"))
}
dimnames(coef_pval) <- list(c("Intercept", "pre_Total","log(GDP)","log(POP)","factor(Cold)",
                              "factor(Hot)"),c(name))

### Tobit Model
coef_pval <- c()
for (i in 1994){
        train <- subset(tidy.data, year ==i)
        train$Total_s <- train$Total/sum(train$Total, na.rm = TRUE)
        train <- na.omit(train)
        temp <- summary(tobit(Total_s ~ pre_Total_s + log(GDP) + log(POP) + Min_temp, left = 0, right =Inf,
                              dist ="gaussian", data = train))$coefficients[-7 ,c(1,4)]  #log scale remove
        coef_pval <- cbind(coef_pval,temp)
}
dimnames(coef_pval) <- list(c("Intercept", "pre_Total","log(GDP)","log(POP)","factor(Cold)",
                              "factor(Hot)"),c(name))


### na all 0 fill up and simulation
### Linear Regressoin

