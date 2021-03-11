library(ggplot2)
All_BMindex <- round(summary(Whole_AIC) $coefficients [2][1],2) 
All_GDP <- round(summary(Whole_AIC) $coefficients [3][1],2) 
All_inflationByCpi <- round(summary(Whole_AIC) $coefficients [4][1],2) 
All_Intercept <- round(summary(Whole_AIC) $coefficients [1][1],2) 

allData$reg_ex <- 
  All_BMindex*allData$BMindex + All_GDP*allData$GDP + 
  All_inflationByCpi*allData$inflationByCpi + All_Intercept
Brazil_long <- allData %>% 
  filter(Country == "Brazil") %>% 
  dplyr::select(Year,dollar_ex,BMindex,reg_ex) %>% 
  gather(key = "Type", value = "value", -Year)
Britain_long <- allData %>% 
  filter(Country == "Britain") %>% 
  dplyr::select(Year,dollar_ex,BMindex,reg_ex) %>% 
  gather(key = "Type", value = "value", -Year)


library(ggplot2)
ggplot(Brazil_long, 
       aes(x = Year,y = value,colour=Type,group=Type)) +
  geom_line()+
  labs(x="時間", y="匯率", title="巴西歷年匯率")
ggplot(Britain_long, 
       aes(x = Year,y = value,colour=Type,group=Type)) +
  geom_line()+
  labs(x="時間", y="匯率", title="英國歷年匯率")
