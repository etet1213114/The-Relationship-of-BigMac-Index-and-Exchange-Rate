library(ggplot2)
All_BMindex <- round(summary(Whole_AIC) $coefficients [2][1],2) 
All_GDP <- round(summary(Whole_AIC) $coefficients [3][1],2) 
All_inflationByCpi <- round(summary(Whole_AIC) $coefficients [4][1],2) 
All_Intercept <- round(summary(Whole_AIC) $coefficients [1][1],2) 

allData$reg_all <- 
  All_BMindex*allData$BMindex + All_GDP*allData$GDP + 
  All_inflationByCpi*allData$inflationByCpi + All_Intercept

Canada_long <- allData %>% 
  filter(Country == "Canada") %>% 
  dplyr::select(Year,dollar_ex,BMindex,reg_all) %>% 
  gather(key = "匯率種類", value = "value", -Year)

ggplot(Canada_long, 
       aes(x = Year,y = value,colour=匯率種類,group=匯率種類)) +
  geom_line(size = 1.2) +
  scale_colour_discrete(labels = c("大麥克指數","實際值","修正值")) +
  labs(x="",y="匯率(/$)", title="加拿大歷年匯率")
