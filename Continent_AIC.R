library(MASS)
SAmerica <- filter(allDatatemp, Continent_Name == "South America")
SAmerica_g <- glm(dollar_ex~
                 BMindex + GDP + GNI + age15_64 + Birth + 
                 inflationByCpi + unemployment + ecoGrowthRate + 
                 usa_GDP +usa_GNI + usa_age + usa_Birth + 
                 usa_inflation + usa_une + usa_eco,
               data = SAmerica)
SAmerica_AIC <- stepAIC(SAmerica_g, direction = "both", trace=F)
summary(SAmerica_AIC)

NAmerica <- filter(allDatatemp, Continent_Name == "North America")
NAmerica_g <- glm(dollar_ex~
              BMindex + GDP + GNI + age15_64 + Birth + 
              inflationByCpi + unemployment + ecoGrowthRate + 
              usa_GDP +usa_GNI + usa_age + usa_Birth + 
              usa_inflation + usa_une + usa_eco,
            data = NAmerica)
NAmerica_AIC <- stepAIC(NAmerica_g, direction = "both", trace=F)
summary(NAmerica_AIC)

Oceania <- filter(allDatatemp, Continent_Name == "Oceania")
Oceania_g <- glm(dollar_ex~
              BMindex + GDP + GNI + age15_64 + Birth + 
              inflationByCpi + unemployment + ecoGrowthRate + 
              usa_GDP +usa_GNI + usa_age + usa_Birth + 
              usa_inflation + usa_une + usa_eco,
            data = Oceania)
Oceania_AIC <- stepAIC(Oceania_g, direction = "both", trace=F)
summary(Oceania_AIC)

Europe <- filter(allDatatemp, Continent_Name == "Europe")
Europe_g <- glm(dollar_ex~
              BMindex + GDP + GNI + age15_64 + Birth + 
              inflationByCpi + unemployment + ecoGrowthRate + 
              usa_GDP +usa_GNI + usa_age + usa_Birth + 
              usa_inflation + usa_une + usa_eco,
            data = Europe)
Europe_AIC <- stepAIC(Europe_g, direction = "both", trace=F)
summary(Europe_AIC)

Asia <- filter(allDatatemp, Continent_Name == "Asia")
Asia_g <- glm(dollar_ex~
              BMindex + GDP + GNI + age15_64 + Birth + 
              inflationByCpi + unemployment + ecoGrowthRate + 
              usa_GDP +usa_GNI + usa_age + usa_Birth + 
              usa_inflation + usa_une + usa_eco,
            data = Asia)
Asia_AIC <- stepAIC(Asia_g, direction = "both", trace=F)
summary(Asia_AIC)

Africa <- filter(allDatatemp, Continent_Name == "Africa")
Africa_g <- glm(dollar_ex~
              BMindex + GDP + GNI + age15_64 + Birth + 
              inflationByCpi + unemployment + ecoGrowthRate + 
              usa_GDP +usa_GNI + usa_age + usa_Birth + 
              usa_inflation + usa_une + usa_eco,
            data = Africa)
Africa_AIC <- stepAIC(Africa_g, direction = "both", trace=F)
summary(Africa_AIC)