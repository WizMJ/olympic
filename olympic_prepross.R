
setwd("./olympic")
### Load data
gdp <- read.csv("gdp.csv", header = T, sep=",")
pop <- read.csv("pop.csv", header = T, sep=",")
medal <- read.csv("medal92to00.csv", header = T, sep=",")

### Imputation
### Czech and slovakia 7(Gold(4), Silver(2), Bronze(1)) in 1992
### Czech 11 (Gold(4), Silver(3), Bronze(4)) and Slovakia 3 (Gold(1), Silver(1), Bronze(1)) in 1996
### Czech 8 (Gold(2), Silver(3), Bronze(3)) and Slovakia 5 (Gold(1), Silver(3), Bronze(1)) in 2000
medal$Total92[which(medal$Country == "Czech Republic")] <- round(7*19/27)
medal$Total92[which(medal$Country == "Slovakia")] <- 7- medal$Total92[which(medal$Country == "Czech Republic")]   

#### IOC for Serbia and Monte, Macedonia
### Total 3 for S&B (Silver(1), Bronze(2) in 1992
medal[which(medal$Country == "Serbia and Montenegro"), 9:13] <- c(52, 0,1,2,3)
medal[which(medal$Country == "Macedonia"), 9:13] <- c(6, 0,0,0,0)

#### Unified Team Medal Distribution
UT <- c("Armenia","Azerbaijan","Belarus","Georgia","Kazakhstan","Kyrgyzstan",
        "Moldova","Russia","Tajikistan","Turkmenistan","Ukraine","Uzbekistan")
UT_df <- medal[medal$Country %in% UT,]
UT_df$ratio92 <- UT_df$Total96/sum(UT_df$Total96)
UT_df$Total92 <- round(112*UT_df$ratio92)
medal$Total92[medal$Country %in% UT] <- UT_df$Total92

### dataframe extraction for 1996 medal prediction

df_medal <- medal[,c("Country","Total92","Total96")]
df_pop96 <- pop[,c("Country","X1996")]
df_gdp96 <-  gdp[,c("Country","X1996")]
df <- merge(df_medal,df_pop96, all.x = TRUE, by="Country")
df <- merge(df,df_gdp96, all.x = TRUE, by="Country")
names(df) <- c("Country","Medal92","Medal96", "POP96", "GDP96")
df <- df[!is.na(df$Medal92),]
df$Medal92_share <- df$Medal92/sum(df$Medal92)*100
df$Medal96_share <- df$Medal96/sum(df$Medal96)*100

### 인구 및 gdp missing data imputation
df[df$Country == "Chinese Taipei", c("POP96","GDP96")] <- c(20353000, 14381.1)
df[df$Country == "Serbia and Montenegro",c("POP96","GDP96")] <- c(10471653,2000.513)
df[df$Country == "North Korea",c("POP96","GDP96")] <- c(22113548, 1115.261)
df <- df[complete.cases(df),]

### Data exploration
library(ggplot2)
pop96_head <- head(sort(df$POP96, decreasing =T), 30)
g <- ggplot(df, aes(x = Country, y = POP96))
g + geom_bar(stat ="identity")

# linear regression
lm.fit <- lm(Medal96_share ~ Medal92_share + log(POP96) + log(GDP96), data =df)
hist(log(df$POP96))

#tobit
library(VGAM)
L
tobit <- vglm(Medal96_share ~ Medal92_share + log(POP96) + log(GDP96), family ="tobit", data = df)
summary(tobit)

