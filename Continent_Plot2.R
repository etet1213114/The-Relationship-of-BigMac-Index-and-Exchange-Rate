library(ggplot2)
summary(Europe_AIC)
Europe_BMindex <- round(summary(Europe_AIC) $coefficients [2][1],2) 
Europe_GDP <- round(summary(Europe_AIC) $coefficients [3][1],2) 
Europe_GNI <- round(summary(Europe_AIC) $coefficients [4][1],2) 
Europe_Brith <- round(summary(Europe_AIC) $coefficients [5][1],2) 
Europe_inflationByCpi <- round(summary(Europe_AIC) $coefficients [6][1],2) 
Europe_Intercept <- round(summary(Europe_AIC) $coefficients [1][1],2)

tempEurope <- allDatatemp %>%
  filter(Continent_Name == "Europe")
tempEurope$reg_Europe <- Europe_BMindex*tempEurope$BMindex + 
  Europe_GDP*tempEurope$GDP + Europe_GNI*tempEurope$GNI + Europe_Brith*tempEurope$Birth +
  Europe_inflationByCpi*tempEurope$inflationByCpi + Europe_Intercept

Britain_long <- tempEurope %>%
  filter(Country == "Britain") %>%
  dplyr::select(Year, dollar_ex, BMindex, reg_Europe) %>%
  gather(key = Type, value = Value, -Year)
ggplot(Britain_long, aes(x = Year, y = Value, colour = Type, group=Type)) + 
  geom_line(size=1.2)+
  labs(x="", y="匯率(/$)", title="英國歷年匯率")

Russia_long <- tempEurope %>%
  filter(Country == "Russia") %>%
  dplyr::select(Year, dollar_ex, BMindex, reg_Europe) %>%
  gather(key = Type, value = Value, -Year)

ggplot(Russia_long, aes(x = Year, y = Value, colour = Type, group=Type)) + 
  geom_line()+
  labs(x="", y="匯率", title="俄羅斯歷年匯率")

summary(NAmerica_AIC)
NAmerica_BMindex <- round(summary(NAmerica_AIC) $coefficients[2][1],2) 
NAmerica_GDP <- round(summary(NAmerica_AIC) $coefficients[3][1],2) 
NAmerica_Brith <- round(summary(NAmerica_AIC) $coefficients[4][1],2)
NAmerica_inflationByCpi <- round(summary(NAmerica_AIC) $coefficients[5][1],2)
NAmerica_unemployment <- round(summary(NAmerica_AIC) $coefficients[6][1],2)
NAmerica_ecoGrowthRate <- round(summary(NAmerica_AIC) $coefficients[7][1],2)
NAmerica_usa_GDP <- round(summary(NAmerica_AIC) $coefficients[8][1],2)
NAmerica_usa_inflation <- round(summary(NAmerica_AIC) $coefficients[9][1],2)
NAmerica_usa_une <- round(summary(NAmerica_AIC) $coefficients[10][1],2)
NAmerica_usa_eco <- round(summary(NAmerica_AIC) $coefficients[11][1],2)
NAmerica_Intercept <- round(summary(NAmerica_AIC) $coefficients[1][1],2)

tempNAmerica <- allDatatemp %>%
  filter(Continent_Name == "North America")
tempNAmerica$reg_NAmerica <- NAmerica_BMindex*tempNAmerica$BMindex + NAmerica_GDP*tempNAmerica$GDP + 
  NAmerica_Brith*tempNAmerica$Birth + NAmerica_inflationByCpi*tempNAmerica$inflationByCpi +
  NAmerica_unemployment*tempNAmerica$unemployment + NAmerica_ecoGrowthRate*tempNAmerica$ecoGrowthRate +
  NAmerica_usa_GDP*tempNAmerica$usa_GDP + NAmerica_usa_inflation*tempNAmerica$usa_inflation +
  NAmerica_usa_une*tempNAmerica$usa_une + NAmerica_usa_eco*tempNAmerica$usa_eco + NAmerica_Intercept

Canada_long <- tempNAmerica %>%
  filter(Country == "Canada") %>%
  dplyr::select(Year, dollar_ex, BMindex,reg_NAmerica) %>%
  gather(key = "匯率種類", value = Value, -Year)

ggplot(Canada_long, aes(x = Year, y = Value, colour = 匯率種類, group=匯率種類)) + 
  geom_line(size=1.2)+
  scale_colour_discrete(labels = c("大麥克指數","實際值","修正值")) +
  labs(x="", y="匯率(/$)", title="加拿大歷年匯率")

summary(SAmerica_AIC)
SAmerica_BMindex <- round(summary(SAmerica_AIC) $coefficients[2][1],2) 
SAmerica_usa_Birth <- round(summary(SAmerica_AIC) $coefficients[3][1],2)
SAmerica_usa_inflation <- round(summary(SAmerica_AIC) $coefficients[4][1],2)
SAmerica_usa_une <- round(summary(SAmerica_AIC) $coefficients[5][1],2)
SAmerica_Intercept <- round(summary(SAmerica_AIC) $coefficients[1][1],2)

tempSAmerica <- allDatatemp %>%
  filter(Continent_Name == "South America")
tempSAmerica$reg_SAmerica <- SAmerica_BMindex*tempSAmerica$BMindex +
  SAmerica_usa_Birth*tempSAmerica$usa_Birth + SAmerica_usa_inflation*tempSAmerica$usa_inflation +
  SAmerica_usa_une*tempSAmerica$usa_une + SAmerica_Intercept

Brazil_long <- tempSAmerica %>%
  filter(Country == "Brazil") %>%
  dplyr::select(Year, dollar_ex, BMindex, reg_SAmerica) %>%
  gather(key = Type, value = Value, -Year)
ggplot(Brazil_long, aes(x = Year, y = Value, colour = Type, group=Type)) + 
  geom_line()+
  labs(x="", y="匯率(/$)", title="巴西歷年匯率")

Argentina_long <- tempSAmerica %>%
  filter(Country == "Argentina") %>%
  dplyr::select(Year, dollar_ex, BMindex, reg_SAmerica) %>%
  gather(key = Type, value = Value, -Year)
ggplot(Argentina_long, aes(x = Year, y = Value, colour = Type, group=Type)) + 
  geom_line()+
  labs(x="", y="匯率", title="阿根廷歷年匯率")

Chile_long <- tempSAmerica %>%
  filter(Country == "Chile") %>%
  dplyr::select(Year, dollar_ex, BMindex, reg_SAmerica) %>%
  gather(key = Type, value = Value, -Year)
ggplot(Chile_long, aes(x = Year, y = Value, colour = Type, group=Type)) + 
  geom_line()+
  labs(x="", y="匯率", title="智利歷年匯率")

Colombia_long <- tempSAmerica %>%
  filter(Country == "Colombia") %>%
  dplyr::select(Year, dollar_ex, BMindex, reg_SAmerica) %>%
  gather(key = "匯率種類", value = Value, -Year)
ggplot(Colombia_long, aes(x = Year, y = Value, colour = 匯率種類, group=匯率種類)) + 
  geom_line(size=1.2)+
  scale_colour_discrete(labels = c("大麥克指數","實際值","修正值"))+
  labs(x="", y="匯率(/$)", title="哥倫比亞歷年匯率")

Peru_long <- tempSAmerica %>%
  filter(Country == "Peru") %>%
  dplyr::select(Year, dollar_ex, BMindex, reg_SAmerica) %>%
  gather(key = Type, value = Value, -Year)
ggplot(Peru_long, aes(x = Year, y = Value, colour = Type, group=Type)) + 
  geom_line()+
  labs(x="", y="匯率(/$)", title="Peru歷年匯率")

Uruguay_long <- tempSAmerica %>%
  filter(Country == "Uruguay") %>%
  dplyr::select(Year, dollar_ex, BMindex, reg_SAmerica) %>%
  gather(key = Type, value = Value, -Year)
ggplot(Uruguay_long, aes(x = Year, y = Value, colour = Type, group=Type)) + 
  geom_line()+
  labs(x="", y="匯率", title="Uruguay歷年匯率")