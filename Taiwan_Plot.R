library(ggplot2)
Twn_BMindex <- round(summary(Taiwan_AIC) $coefficients [2][1],2)
Twn_GNI <- round(summary(Taiwan_AIC) $coefficients [3][1],2)
Twn_age15_64 <- round(summary(Taiwan_AIC) $coefficients [4][1],2)
Twn_unemployment <- round(summary(Taiwan_AIC) $coefficients [5][1],2)
Twn_ecoGrowthRate <- round(summary(Taiwan_AIC) $coefficients [6][1],2)
Twn_usa_GDP <- round(summary(Taiwan_AIC) $coefficients [7][1],2)
Twn_usa_GNI <- round(summary(Taiwan_AIC) $coefficients [8][1],2)
Twn_usa_age <- round(summary(Taiwan_AIC) $coefficients [9][1],2)
Twn_usa_inflation <- round(summary(Taiwan_AIC) $coefficients [10][1],2)
Twn_usa_eco <- round(summary(Taiwan_AIC) $coefficients [11][1],2)
Twn_Intercept <- round(summary(Taiwan_AIC) $coefficients [1][1],2)

allData$reg_twn<- Twn_BMindex*allData$BMindex + Twn_GNI*allData$GNI+ Twn_age15_64*allData$age15_64 +
  Twn_unemployment*allData$unemployment + Twn_ecoGrowthRate*allData$ecoGrowthRate+
  Twn_usa_GDP*allData$usa_GDP + Twn_usa_GNI*allData$usa_GNI + Twn_usa_age*allData$usa_age +
  Twn_usa_inflation*allData$usa_inflation  + Twn_usa_eco*allData$usa_eco + Twn_Intercept

Taiwan_long <- allData %>% 
  filter(Country == "Taiwan") %>% 
  dplyr::select(Year,dollar_ex,BMindex,reg_ex,reg_twn) %>% 
  gather(key = "Type", value = "value", -Year)

library(ggplot2)
ggplot(Taiwan_long, 
       aes(x = Year,y = value,colour=Type,group=Type)) +
  geom_line()+
  labs(x="時間", y="匯率", title="台灣歷年匯率")
