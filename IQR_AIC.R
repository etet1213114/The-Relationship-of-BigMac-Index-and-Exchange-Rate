library(MASS)
IQR_g <- glm(dollar_ex~
               BMindex + GDP + GNI + age15_64 + Birth + 
               inflationByCpi + unemployment + ecoGrowthRate + 
               usa_GDP +usa_GNI + usa_age + usa_Birth + 
               usa_inflation + usa_une + usa_eco,
             data = allDataIQR)
IQR_AIC <- stepAIC(IQR_g, direction = "both", trace=F)
summary(IQR_AIC)
