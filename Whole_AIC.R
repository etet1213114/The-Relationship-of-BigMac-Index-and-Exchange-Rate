library(MASS)
Whole_g <- glm(dollar_ex~
               BMindex + GDP + GNI + age15_64 + Birth + 
               inflationByCpi + unemployment + ecoGrowthRate + 
               usa_GDP +usa_GNI + usa_age + usa_Birth + 
               usa_inflation + usa_une + usa_eco,
             data = allData)
Whole_AIC <- stepAIC(Whole_g, direction = "both", trace=F)
summary(Whole_AIC)

#every three year
y0103 <- filter(allData, Year %in% 2001:2003)
y0103_g <- glm(dollar_ex~
                 BMindex + GDP + inflationByCpi,
               data = y0103)
summary(y0103_g)

y0406 <- filter(allData, Year %in% 2004:2006)
y0406_g <- glm(dollar_ex~
                 BMindex + GDP + inflationByCpi,
               data = y0406)
summary(y0406_g)

y0709 <- filter(allData, Year %in% 2007:2009)
y0709_g <- glm(dollar_ex~
                 BMindex + GDP + inflationByCpi,
               data = y0709)
summary(y0709_g)

y1012 <- filter(allData, Year %in% 2010:2012)
y1012_g <- glm(dollar_ex~
                 BMindex + GDP + inflationByCpi,
               data = y1012)
summary(y1012_g)

y1315 <- filter(allData, Year %in% 2013:2015)
y1315_g <- glm(dollar_ex~
                 BMindex + GDP + inflationByCpi,
               data = y1315)
summary(y1315_g)

y1618 <- filter(allData, Year %in% 2016:2018)
y1618_g <- glm(dollar_ex~
                 BMindex + GDP + inflationByCpi,
               data = y1618)
summary(y1618_g)

#country
jpn <- filter(allData, Country == "Japan")
jpn_g <- glm(dollar_ex~
               BMindex + GDP + inflationByCpi,
             data = jpn)
summary(jpn_g)

chn <- filter(allData, Country == "China")
chn_g <- glm(dollar_ex~
               BMindex + GDP + inflationByCpi,
             data = chn)
summary(chn_g)

kor <- filter(allData, Country == "South Korea")
kor_g <- glm(dollar_ex~
               BMindex + GDP + inflationByCpi,
             data = kor)
summary(kor_g)

hkg <- filter(allData, Country == "Hong Kong")
hkg_g <- glm(dollar_ex~
               BMindex + GDP + inflationByCpi,
             data = hkg)
summary(hkg_g)

sgp <- filter(allData, Country == "Singapore")
sgp_g <- glm(dollar_ex~
               BMindex + GDP + inflationByCpi,
             data = sgp)
summary(sgp_g)

gbr <- filter(allData, Country == "Britain")
gbr_g <- glm(dollar_ex~
               BMindex + GDP + inflationByCpi,
             data = gbr)
summary(gbr_g)

can <- filter(allData, Country == "Canada")
can_g <- glm(dollar_ex~
               BMindex + GDP + inflationByCpi,
             data = can)
summary(can_g)

rus <- filter(allData, Country == "Russia")
rus_g <- glm(dollar_ex~
               BMindex + GDP + inflationByCpi,
             data = rus)
summary(rus_g)

bra <- filter(allData, Country == "Brazil")
bra_g <- glm(dollar_ex~
               BMindex + GDP + inflationByCpi,
             data = bra)
summary(bra_g)

twn <- filter(allData, Country == "Taiwan")
twn_g <- glm(dollar_ex~
               BMindex + GDP + inflationByCpi,
             data = twn)
summary(twn_g)