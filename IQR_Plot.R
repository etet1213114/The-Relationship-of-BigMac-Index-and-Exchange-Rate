library(ggplot2)
IQR_BMindex <- round(summary(IQR_AIC) $coefficients [2][1],2)
IQR_GDP <- round(summary(IQR_AIC) $coefficients [3][1],2)
IQR_GNI <- round(summary(IQR_AIC) $coefficients [4][1],2) 
IQR_age15_64 <- round(summary(IQR_AIC) $coefficients [5][1],2)
IQR_Birth <- round(summary(IQR_AIC) $coefficients [6][1],2)
IQR_ecoGrowthRate <- round(summary(IQR_AIC) $coefficients [7][1],2)
IQR_usa_Birth <- round(summary(IQR_AIC) $coefficients [8][1],2)
IQR_Intercept <- round(summary(IQR_AIC) $coefficients [1][1],2)

allData$reg_IQR <- IQR_BMindex*allData$BMindex + IQR_GDP*allData$GDP +
  IQR_GNI*allData$GNI + IQR_age15_64*allData$age15_64 +
  IQR_Birth*allData$Birth + IQR_ecoGrowthRate*allData$ecoGrowthRate +
  IQR_usa_Birth*allData$usa_Birth + IQR_Intercept

#China
Argentina_long <- allData %>% 
  filter(Country == "China") %>% 
  dplyr::select(Year,dollar_ex,BMindex,reg_IQR) %>% 
  gather(key = "匯率種類", value = "value", -Year)
ggplot(Argentina_long, 
       aes(x = Year,y = value,colour=匯率種類,group=匯率種類)) +
  geom_line(size = 1.2) +
  scale_colour_discrete(labels = c("大麥克指數","實際值","修正值")) +
  labs(x="",y="匯率(/$)", title="中國歷年匯率") 

#Argentina
Argentina_long <- allData %>% 
  filter(Country == "Argentina") %>% 
  dplyr::select(Year,dollar_ex,BMindex,reg_IQR) %>% 
  gather(key = "匯率種類", value = "value", -Year)
ggplot(Argentina_long, 
       aes(x = Year,y = value,colour=匯率種類,group=匯率種類)) +
  geom_line(size = 1.2) +
  scale_colour_discrete(labels = c("大麥克指數","實際值","修正值")) +
  labs(x="",y="匯率(/$)", title="阿根廷歷年匯率") 

#Egypt
Egypt_long <- allData %>% 
  filter(Country == "Egypt") %>% 
  dplyr::select(Year,dollar_ex,BMindex,reg_IQR) %>% 
  gather(key = "匯率種類", value = "value", -Year)
ggplot(Egypt_long, 
       aes(x = Year,y = value,colour=匯率種類,group=匯率種類)) +
  geom_line(size = 1.2) +
  scale_colour_discrete(labels = c("大麥克指數","實際值","修正值")) +
  labs(x="",y="匯率(/$)", title="埃及歷年匯率") 

#Ukraine
Ukraine_long <- allData %>% 
  filter(Country == "Ukraine") %>% 
  dplyr::select(Year,dollar_ex,BMindex,reg_IQR) %>% 
  gather(key = "匯率種類", value = "value", -Year)
ggplot(Ukraine_long, 
       aes(x = Year,y = value,colour=匯率種類,group=匯率種類)) +
  geom_line(size = 1.2) +
  scale_colour_discrete(labels = c("大麥克指數","實際值","修正值")) +
  labs(x="",y="匯率(/$)", title="烏克蘭歷年匯率") 

#Chile
Chile_long <- allData %>% 
  filter(Country == "Chile") %>% 
  dplyr::select(Year,dollar_ex,BMindex,reg_IQR) %>% 
  gather(key = "匯率種類", value = "value", -Year)
ggplot(Chile_long, 
       aes(x = Year,y = value,colour=匯率種類,group=匯率種類)) +
  geom_line(size = 1.2) +
  scale_colour_discrete(labels = c("大麥克指數","實際值","修正值")) +
  labs(x="",y="匯率(/$)", title="智利歷年匯率") 

#Czech Republic
Czech_long <- allData %>% 
  filter(Country == "Czech Republic") %>% 
  dplyr::select(Year,dollar_ex,BMindex,reg_IQR) %>% 
  gather(key = "匯率種類", value = "value", -Year)
ggplot(Czech_long, 
       aes(x = Year,y = value,colour=匯率種類,group=匯率種類)) +
  geom_line(size = 1.2) +
  scale_colour_discrete(labels = c("大麥克指數","實際值","修正值")) +
  labs(x="",y="匯率(/$)", title="捷克歷年匯率") 

#Hungary
Hungary_long <- allData %>% 
  filter(Country == "Hungary") %>% 
  dplyr::select(Year,dollar_ex,BMindex,reg_IQR) %>% 
  gather(key = "匯率種類", value = "value", -Year)
ggplot(Hungary_long, 
       aes(x = Year,y = value,colour=匯率種類,group=匯率種類)) +
  geom_line(size = 1.2) +
  scale_colour_discrete(labels = c("大麥克指數","實際值","修正值")) +
  labs(x="",y="匯率(/$)", title="匈牙利歷年匯率") 

#Poland
Poland_long <- allData %>% 
  filter(Country == "Poland") %>% 
  dplyr::select(Year,dollar_ex,BMindex,reg_IQR) %>% 
  gather(key = "匯率種類", value = "value", -Year)
ggplot(Poland_long, 
       aes(x = Year,y = value,colour=匯率種類,group=匯率種類)) +
  geom_line(size = 1.2) +
  scale_colour_discrete(labels = c("大麥克指數","實際值","修正值")) +
  labs(x="",y="匯率(/$)", title="波蘭歷年匯率") 

#Sweden
Sweden_long <- allData %>% 
  filter(Country == "Sweden") %>% 
  dplyr::select(Year,dollar_ex,BMindex,reg_IQR) %>% 
  gather(key = "匯率種類", value = "value", -Year)
ggplot(Sweden_long, 
       aes(x = Year,y = value,colour=匯率種類,group=匯率種類)) +
  geom_line(size = 1.2) +
  scale_colour_discrete(labels = c("大麥克指數","實際值","修正值")) +
  labs(x="",y="匯率(/$)", title="瑞典歷年匯率") 

#Saudi Arabia
SaudiArabia_long <- allData %>% 
  filter(Country == "Saudi Arabia") %>% 
  dplyr::select(Year,dollar_ex,BMindex,reg_IQR) %>% 
  gather(key = "匯率種類", value = "value", -Year)
ggplot(SaudiArabia_long, 
       aes(x = Year,y = value,colour=匯率種類,group=匯率種類)) +
  geom_line(size = 1.2) +
  scale_colour_discrete(labels = c("大麥克指數","實際值","修正值")) +
  labs(x="",y="匯率(/$)", title="沙烏地阿拉伯歷年匯率") 

#UAE
UAE_long <- allData %>% 
  filter(Country == "UAE") %>% 
  dplyr::select(Year,dollar_ex,BMindex,reg_IQR) %>% 
  gather(key = "匯率種類", value = "value", -Year)
ggplot(UAE_long, 
       aes(x = Year,y = value,colour=匯率種類,group=匯率種類)) +
  geom_line(size = 1.2) +
  scale_colour_discrete(labels = c("大麥克指數","實際值","修正值")) +
  labs(x="",y="匯率(/$)", title="阿拉伯聯合大公國歷年匯率") 

#Canada
Canada_long2 <- allData %>% 
  filter(Country == "Canada") %>% 
  dplyr::select(Year,dollar_ex,BMindex,reg_IQR) %>% 
  gather(key = "匯率種類", value = "value", -Year)
ggplot(Canada_long2, 
       aes(x = Year,y = value,colour=匯率種類,group=匯率種類)) +
  geom_line(size = 1.2) +
  scale_colour_discrete(labels = c("大麥克指數","實際值","修正值")) +
  labs(x="",y="匯率(/$)", title="加拿大歷年匯率") 
