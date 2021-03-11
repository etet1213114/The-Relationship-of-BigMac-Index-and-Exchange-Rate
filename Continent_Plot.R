library(ggplot2)
Asia_BMindex <- round(summary(Asia_AIC) $coefficients [2][1],2)
Asia_GDP <- round(summary(Asia_AIC) $coefficients [3][1],2)
Asia_age15_64 <- round(summary(Asia_AIC) $coefficients [4][1],2)
Asia_inflationByCpi <- round(summary(Asia_AIC) $coefficients [5][1],2) 
Asia_unemployment <- round(summary(Asia_AIC) $coefficients [6][1],2)
Asia_usa_GDP <- round(summary(Asia_AIC) $coefficients [7][1],2)
Asia_usa_Birth <- round(summary(Asia_AIC) $coefficients [8][1],2)
Asia_usa_inflation <- round(summary(Asia_AIC) $coefficients [9][1],2)
Asia_usa_une <- round(summary(Asia_AIC) $coefficients [10][1],2)
Asia_Intercept <- round(summary(Asia_AIC) $coefficients [1][1],2)

allDatatemp$reg_asia <- 
  Asia_BMindex*allDatatemp$BMindex + Asia_GDP*allDatatemp$GDP + 
  Asia_age15_64*allDatatemp$age15_64 + Asia_inflationByCpi*allDatatemp$inflationByCpi + 
  Asia_unemployment*allDatatemp$unemployment+ Asia_usa_GDP*allDatatemp$usa_GDP + 
  Asia_usa_Birth*allDatatemp$usa_Birth+ Asia_usa_inflation*allDatatemp$usa_inflation+ 
  Asia_usa_une*allDatatemp$usa_une+ Asia_Intercept

Japan_long <- allDatatemp %>% 
  filter(Country == "Japan") %>% 
  dplyr::select(Year,dollar_ex,BMindex,reg_asia) %>% 
  gather(key = "Type", value = "value", -Year)
ggplot(Japan_long, 
       aes(x = Year,y = value,colour=Type,group=Type)) +
  geom_line()+
  labs(x="時間", y="匯率", title="日本歷年匯率")

China_long <- allDatatemp %>% 
  filter(Country == "China") %>% 
  dplyr::select(Year,dollar_ex,BMindex,reg_asia) %>% 
  gather(key = "Type", value = "value", -Year)
ggplot(China_long, 
       aes(x = Year,y = value,colour=Type,group=Type)) +
  geom_line()+
  labs(x="時間", y="匯率", title="中國歷年匯率")

HK_long <- allDatatemp %>% 
  filter(Country == "Hong Kong") %>% 
  dplyr::select(Year,dollar_ex,BMindex,reg_asia) %>% 
  gather(key = "Type", value = "value", -Year)
ggplot(HK_long, 
       aes(x = Year,y = value,colour=Type,group=Type)) +
  geom_line()+
  labs(x="時間", y="匯率", title="香港歷年匯率")

Twn_long <- allDatatemp %>% 
  filter(Country == "Taiwan") %>% 
  dplyr::select(Year,dollar_ex,BMindex,reg_asia) %>% 
  gather(key = "Type", value = "value", -Year)
ggplot(Twn_long, 
       aes(x = Year,y = value,colour=Type,group=Type)) +
  geom_line()+
  labs(x="時間", y="匯率", title="台灣歷年匯率")

kor_long <- allDatatemp %>% 
  filter(Country == "South Korea") %>% 
  dplyr::select(Year,dollar_ex,BMindex,reg_asia) %>% 
  gather(key = "Type", value = "value", -Year)
ggplot(kor_long, 
       aes(x = Year,y = value,colour=Type,group=Type)) +
  geom_line()+
  labs(x="時間", y="匯率", title="南韓歷年匯率")


rus_long <- allDatatemp %>% 
  filter(Country == "Russia") %>% 
  dplyr::select(Year,dollar_ex,BMindex,reg_asia) %>% 
  gather(key = "Type", value = "value", -Year)
ggplot(rus_long, 
       aes(x = Year,y = value,colour=Type,group=Type)) +
  geom_line()+
  labs(x="時間", y="匯率", title="俄羅斯(亞洲)歷年匯率")
