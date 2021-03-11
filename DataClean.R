library(readr)
library(readxl)
library(dplyr)
library(tidyr)
gdp <- read_csv("GDP.csv")
gni <- read_csv("GNI.csv")
age15_64 <- read_csv("age15_64.csv")
Birth <- read_csv("Birth.csv")
inflationByCpi <- read_csv("InflationbyCpi.csv")
Population <- read_csv("population.csv")
unemployment <- read_csv("Unemployment_rate.csv")
ecoGrowthRate <- read_csv("EconomicGrowthRate.csv")
Taiwan <- read_excel("Taiwan.xlsx")
BigMac <- read_excel("BigMac.xlsx")
CountryName <- read_csv("country_continent.csv")


colnames(ecoGrowthRate)[1] <- "Country Name"

gdplong <- gdp %>%
  select(-`Indicator Name`, -`Indicator Code`) %>%
  gather(key = "Year", value = "GDP", -`Country Name`, -`Country Code`)
gnilong <- gni %>%
  select(-`Indicator Name`, -`Indicator Code`) %>%
  gather(key = "Year", value = "GNI", -`Country Name`, -`Country Code`)
agelong <- age15_64 %>%
  select(-`Indicator Name`, -`Indicator Code`) %>%
  gather(key = "Year", value = "age15_64", -`Country Name`, -`Country Code`)
birthlong <- Birth %>%
  select(-`Indicator Name`, -`Indicator Code`) %>%
  gather(key = "Year", value = "Birth", -`Country Name`, -`Country Code`)

inflationByCpilong <- inflationByCpi %>%
  select(-`Indicator Name`, -`Indicator Code`) %>%
  gather(key = "Year", value = "inflationByCpi", -`Country Name`, -`Country Code`)
Populationlong <- Population %>%
  select(-`Indicator Name`, -`Indicator Code`) %>%
  gather(key = "Year", value = "Population", -`Country Name`, -`Country Code`)
unemploymentlong <- unemployment %>%
  select(-`Indicator Name`, -`Indicator Code`) %>%
  gather(key = "Year", value = "unemployment", -`Country Name`, -`Country Code`)
ecoGrowthRatelong <- ecoGrowthRate %>%
  select(-`Indicator Name`, -`Indicator Code`) %>%
  gather(key = "Year", value = "ecoGrowthRate", -`Country Name`, -`Country Code`)

combine <- full_join(gdplong, gnilong, by = c("Country Name", "Country Code", "Year")) %>% 
  full_join(agelong, by = c("Country Name", "Country Code", "Year")) %>%
  full_join(birthlong, by = c("Country Name", "Country Code", "Year")) %>%
  full_join(inflationByCpilong, by = c("Country Name", "Country Code", "Year")) %>%
  full_join(Populationlong, by = c("Country Name", "Country Code", "Year")) %>%
  full_join(unemploymentlong, by = c("Country Name", "Country Code","Year")) %>%
  full_join(ecoGrowthRatelong, by = c("Country Name", "Country Code", "Year"))
combine$GDP <- combine$GDP/combine$Population  
combine <- rbind(combine, Taiwan)

colnames(BigMac)[3] <- "Country Code"
BigMac$Year <- as.character(BigMac$Year)
Taiwan$Year <- as.character(Taiwan$Year)
combine2 <- left_join(BigMac, combine, by = c("Country Code", "Year")) %>%
  select(-`Country Name`) %>%
  filter(Year!=2019, `Country`!="Euro area")
combine2$GNI<-combine2$GNI/combine2$Population
combine2$unemployment<-as.numeric(combine2$unemployment)

allData<-filter(combine2 , Country!="United States")
allData$dollar_ppp <- NULL
allData$dollar_price <- NULL
allData$euro_valuation <- NULL
allData$sterling_valuation <- NULL
allData$yen_valuation <- NULL
allData$yuan_valuation <- NULL
allData$currency_code <- NULL
allData$dollar_valuation<- NULL
allData$Population<- NULL
allData$unemployment<-as.numeric(allData$unemployment)

df<-filter(combine2, `Country Code`=="USA")%>%
  select(Year, local_price, GDP, GNI, age15_64,Birth,inflationByCpi,unemployment,ecoGrowthRate)
names(df)[2:9] <-c("usa_price", "usa_GDP", "usa_GNI","usa_age","usa_Birth","usa_inflation","usa_une","usa_eco")

allData<-left_join(allData, df, by="Year")
allData$BMindex<-allData$local_price/allData$usa_price
allData$Country <- gsub("United Arab Emirates","UAE",allData$Country)
allData[allData$Year ==2000 & allData$Country == "Argentina",]$inflationByCpi <- -0.9
allData[allData$Year ==2001 & allData$Country == "Argentina",]$inflationByCpi <- -1.1
allData[allData$Year ==2002 & allData$Country == "Argentina",]$inflationByCpi <- 25.9
allData[allData$Year ==2003 & allData$Country == "Argentina",]$inflationByCpi <- 13.4
allData$age15_64 <- allData$age15_64/100
allData$unemployment <- allData$unemployment/100
allData$ecoGrowthRate <- allData$ecoGrowthRate/100
allData$inflationByCpi <- allData$inflationByCpi/100
allData$usa_age <- allData$usa_age/100
allData$usa_une <- allData$usa_une/100
allData$usa_eco <- allData$usa_eco/100
allData$usa_inflation <- allData$usa_inflation/100
allData$GDP <- log(allData$GDP)
allData$GNI<- log(allData$GNI)
allData$usa_GDP <- log(allData$usa_GDP)
allData$usa_GNI <- log(allData$usa_GNI)
allData <- allData[complete.cases(allData),]

colnames(CountryName)[5] <- "Country_Code"
colnames(allData)[3] <- "Country_Code"
CountryName <- CountryName %>% dplyr::select(Continent_Name,Country_Code)
allDatatemp <- left_join(allData,CountryName,by = "Country_Code")

allDataIQR <- allData %>%
  filter(dollar_ex >= quantile(allData$dollar_ex)[1],
         dollar_ex <=　quantile(allData$dollar_ex)[3])

