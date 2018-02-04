setwd("D:/R/olympic/pop gdp")
gdp <- read.csv("gdp.csv", header =T)
gdp.un <- read.csv("gdp.un.csv", header =T, sep =',')

setwd("D:/R/olympic/winter data")
library(dplyr)
library(AER)
library(ggplot2)
library(VGAM)

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
### Preprocessing by fill up and remove

### 1994 year 
### Missing data fill-up
train <- subset(tidy.data, year ==1994)
train$Total_s <- train$Total/sum(train$Total)*100
temp <- is.na(train$pre_Total)
train[temp,c("pre_Total", "pre_Total_s")] <- 0

### GDP fill-up
### GDP Fill up function
gdp_fill <- function(country, year){
     gdp_temp <- t(rbind(gdp[gdp$Country == country, 13:59], gdp.un[gdp.un$Country == country,3:49]))
     gdp_temp <- as.data.frame(gdp_temp)
     colnames(gdp_temp) <- c("gdp","gdp.un")
     fit <- lm(gdp ~ gdp.un, data = gdp_temp[complete.cases(gdp_temp),])
     return(predict(fit, newdata=data.frame(gdp.un=gdp_temp[year-1969,"gdp.un"])))
}
train[which(train$Country == "Estonia"),"GDP"] <- gdp_fill("Estonia",1994) # 1994 Estonia gdp
train[which(train$Country == "Croatia"),"GDP"] <- gdp_fill("Croatia",1994) # 1994 Croatia gdp
train[which(train$Country == "Latvia"),"GDP"] <- gdp_fill("Latvia",1994) # 1994 Latvia gdp
train[which(train$Country == "Lithuania"),"GDP"] <- gdp_fill("Lithuania",1994) # 1994 Latvia gdp
train[which(train$Country == "San Marino"),"GDP"] <- gdp_fill("San Marino",1994) # 1994 San Marino gdp
train[which(train$Country == "Slovenia"),"GDP"] <- gdp_fill("Slovenia",1994) # 1994 Slovenia gdp
train[which(train$Country == "Moldova"),"GDP"] <- gdp_fill("Moldova",1994) # 1994 Slovenia gdp

### correlation test between gdp and gdp.un
#gdp_temp <- t(rbind(gdp[gdp$Country == "Moldova", 13:59], gdp.un[gdp.un$Country == "Moldova",3:49]))
#gdp_temp <- as.data.frame(gdp_temp)
#colnames(gdp_temp) <- c("gdp","gdp.un")
#fit <- lm(gdp ~ gdp.un, data = gdp_temp[complete.cases(gdp_temp),])

# 5 countries are removed
remove <- train$Country %in% c("Slovakia", "Czech Republic", "Chinese Taipei", "American Samoa","Virgin Islands")
train <- train[!remove,] 
fit_lm <- lm(Total_s ~ pre_Total_s + log(GDP) + log(POP) +Min_temp, data = train)
fit_tb <- tobit(Total_s ~ pre_Total_s + log(GDP) + log(POP) + Min_temp, left = 0, right =Inf,
                dist ="gaussian", data = train)
summary(m <- vglm(Total_s ~ pre_Total_s + log(GDP) + log(POP) +Min_temp, tobit(Lower = 0), data = train))

### Linear Regressoin
coef_pval <- c()
for (i in year){
        train <- subset(tidy.data, year ==i)
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