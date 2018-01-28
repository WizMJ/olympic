library(dplyr)
library(rvest)
library(stringr)

setwd("./olympic")
# 1992 medal
html1 <- read_html("https://en.wikipedia.org/wiki/1992_Summer_Olympics_medal_table")
medal92 <- html_table(html_nodes(html1,"table")[[2]], fill=TRUE)
medal92$Country <- str_sub(medal92$NOC, 1,-7)
medal92$NOC <- str_sub(medal92$NOC,-4,-2)
medal92$NOC[6] <- "ESP"
medal92$Country[6] <- "Spain"
medal92 <- medal92[-65,-1]

# 1992 participants
html2 <- read_html("https://en.wikipedia.org/wiki/1992_Summer_Olympics#Medal_count")
temp <- html_table(html_nodes(html2,'table')[9], fill=TRUE)
temp <-as.character(temp[[1]])
temp <- strsplit(temp, split="[()]")
part92 <- matrix(temp[[1]], 170,2, byrow=TRUE)
part92 <- data.frame(Country = part92[-141,1], athletes92 =as.numeric(part92[-141,2]))
temp2 <- str_replace(part92$Country,"<U+00A0>","")
part92$Country <- c(str_sub(temp2[1],1,-2),str_sub(temp2[-1], 3,-2))

# part92와 medal92 merge
df92 <- merge(part92, medal92, all = TRUE, by = "Country")

# 1996 medal
html3 <- read_html("https://en.wikipedia.org/wiki/1996_Summer_Olympics_medal_table")
medal96 <- html_table(html_nodes(html3,"table")[[2]], fill=TRUE)
medal96$Country <- str_sub(medal96$NOC, 1,-7)
medal96$NOC <- str_sub(medal96$NOC,-4,-2)
medal96$NOC[1] <- "USA"
medal96$Country[1] <- "United States"
medal96 <- medal96[-80,-1]

# 1996 participants
html4 <- read_html("https://en.wikipedia.org/wiki/1996_Summer_Olympics")
temp <- html_table(html_nodes(html4,'table')[10], fill=TRUE)
temp <-as.character(temp[[1]])
temp <- strsplit(temp, split="[()]")
part96 <- matrix(temp[[1]], 197,2, byrow=TRUE)
part96 <- data.frame(Country = part96[-185,1], athletes96 =as.numeric(part96[-185,2]))
temp2 <- str_replace(part96$Country,"<U+00A0>","")
part96$Country <- c(str_sub(temp2[1],1,-2),str_sub(temp2[-1], 3,-2))
part96$Country[193] <- "Serbia and Montenegro" 
part96$Country[172] <- "Tajikistan"

# part96와 medal96 merge
df96 <- merge(part96, medal96, all = TRUE, by = "Country")
df96$athletes96[42] <- 74  #Chinese Taipei

names(df92) <- c("Country","athletes92", "NOC", "Gold92", "Silver92", "Bronze92", "Total92")
names(df96) <- c("Country","athletes96", "NOC", "Gold96", "Silver96", "Bronze96", "Total96")
df <- merge(df96, df92, all = TRUE, by = "Country")
df <- df[-c(52,83,188),] # Czeco, IOC, Unified Team in 1992 제거
df[df$Country == "Zaire","Country"] <- "DR Congo"  # 2000년 부터 DR Congo로 참가

# NOC 하나로 통일
df$NOC <- df$NOC.x[df$NOC.x == df$NOC.y]
df$NOC[!is.na(df$NOC.x) & is.na(df$NOC.y)] <- df$NOC.x[!is.na(df$NOC.x) & is.na(df$NOC.y)]
df$NOC[is.na(df$NOC.x) & !is.na(df$NOC.y)] <- df$NOC.y[is.na(df$NOC.x) & !is.na(df$NOC.y)]
df <- df[,-c(3,9)]

df[is.na(df$Total96),3:6] <-0  #1996은 모두 참가 
df[!is.na(df$athletes92) & is.na(df$Total92) ,8:11] <-0
df <- cbind(Country = df[,1],NOC= df[,"NOC"],df[,2:11])

# NOC fill-up 
html.noc <- read_html("https://en.wikipedia.org/wiki/List_of_IOC_country_codes")
noc.table <- html_table(html_nodes(html.noc,"table")[[1]], fill=TRUE)
noc.table <- noc.table[,c(1,3)]
names(noc.table) <-c("NOC1","Country")
noc.table[189,"Country"] <- "Chinese Taipei"
df1 <- merge(df, noc.table, all = TRUE, by = "Country")
df1$NOC1[c(126,156,195)] <- c("AHO","SCG","ZAI")
df1$NOC <- df1$NOC1; df1 <- df1[,-13]  #NOC1 제거
### write.csv(df1, "df9296.csv")

# 2000 medal
html5 <- read_html("https://en.wikipedia.org/wiki/2000_Summer_Olympics_medal_table")
medal00 <- html_table(html_nodes(html5,"table")[[2]], fill=TRUE)
medal00$Country <- str_sub(medal00$NOC, 1,-7)
medal00$NOC <- str_sub(medal00$NOC,-4,-2)
medal00$NOC[4] <- "AUS"
medal00$Country[4] <- "Australia"
medal00 <- medal00[-81,-1]

# 2000 participants
html6 <- read_html("https://en.wikipedia.org/wiki/2000_Summer_Olympics")
temp <- html_table(html_nodes(html6,'table')[10], fill=TRUE)
temp <-as.character(temp[[1]])
temp <- strsplit(temp, split="[()]")
part00 <- matrix(temp[[1]], 201,2, byrow=TRUE)
part00 <- data.frame(Country = part00[-11,1], athletes00 =as.numeric(part00[-11,2])) #host
temp2 <- str_replace(part00$Country,"<U+00A0>","")
part00$Country <- c(str_sub(temp2[1],1,-2),str_sub(temp2[-1], 3,-2))
part00$Country[198] <- "Serbia and Montenegro" 

# part00와 medal00 merge
df00 <- merge(part00, medal00, all = TRUE, by = "Country")
names(df00) <- c("Country","athletes00", "NOC", "Gold00", "Silver00", "Bronze00", "Total00")
df00 <- df00[, -3]
df00[which(df00$Country == "Independent Olympic Athletes"),"Country"] <- "Timor-Leste"
df00[is.na(df00$Total00),3:6] <-0

df92to00 <- merge(df1, df00, all =TRUE, by ="Country")
df92to00 <- df92to00[-c(204,205), ] # Serbia 와 Montenegro는 2000년 까지는 같은 나라로 참가함.
write.csv(df92to00, "medal92to00.csv")
##### 92년 169개국 중 Czecoslovakia, IOC, Unified Team 의 3개 그룹이 제외된 166개국 data임 ####
##### 92년에 제외된 3개 그룹은 최종 나라면 기준으로 fill-up 해야함
##### 96년은 총 197개국 참석
##### 00년은 총 199개국 + 1개국(IOC(후의 동티모르))참석ㅎ

## GDP와 df92to00의 Country 이름 Match up
#gdp <- read.csv("gdp_worldbank.csv", skip =4, header =T)
#gdp <- gdp[, c(1,2, 37, 41, 45)]
#gdp$Country.Name <- as.character(gdp$Country.Name)
#names(gdp) <- c("Country", "NOC", "1992", "1996", "2000")
#medal_gdp <- merge(df92to00[,1:2], gdp[,1:2], all =TRUE, by ="NOC")
#medal_gdp <- merge(df92to00[,1:2], gdp[,1:2], all =TRUE, by ="Country")
### write.csv(medal_gdp,"match_name.csv")

### GDP.csv, POP.csv save
gdp <- read.csv("gdp_worldbank.csv", skip =4, header =T )
gdp <- gdp[, -c(2,3,4,62,63)]
colnames(gdp) <- c("Country", paste(1960:2016))
gdp$Country <- as.character(gdp$Country)

pop <- read.csv("pop_worldbank.csv", skip =4, header =T)
pop <- pop[, -c(2,3,4,62,63)]
colnames(pop) <- c("Country", paste(1960:2016))
pop$Country <- as.character(pop$Country)

#gdp$Country.Name <- as.character(gdp$Country.Name)
country_match <- read.csv("match_name_for_use.csv", header=T, colClasses = "character")
country_match <- country_match[complete.cases(country_match),]

### gdp pop 이름을 medal 이름으로 변ㄱ
for (i in 1:27){
        gdp$Country[gdp$Country == country_match$GDP.Country[i]] <- country_match$Olympic.Country[i]
}

for (i in 1:27){
        pop$Country[pop$Country == country_match$GDP.Country[i]] <- country_match$Olympic.Country[i]
}
write.csv(gdp, "gdp.csv")
write.csv(pop, "pop.csv")


