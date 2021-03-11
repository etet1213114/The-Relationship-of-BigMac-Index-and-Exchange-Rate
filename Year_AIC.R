library(MASS)
y2000 <- filter(allData, Year == 2000)
y2000_g <- glm(dollar_ex~
                 BMindex + GDP + GNI + age15_64 + Birth + 
                 inflationByCpi + unemployment + ecoGrowthRate + 
                 usa_GDP +usa_GNI + usa_age + usa_Birth + 
                 usa_inflation + usa_une + usa_eco,
               data = y2000)
y2000_AIC <- stepAIC(y2000_g, direction = "both", trace=F)
summary(y2000_AIC)

y2001 <- filter(allData, Year == 2001)
y2001_g <- glm(dollar_ex~
                 BMindex + GDP + GNI + age15_64 + Birth + 
                 inflationByCpi + unemployment + ecoGrowthRate + 
                 usa_GDP +usa_GNI + usa_age + usa_Birth + 
                 usa_inflation + usa_une + usa_eco,
               data = y2001)
y2001_AIC <- stepAIC(y2001_g, direction = "both", trace=F)
summary(y2001_AIC)

y2002 <- filter(allData, Year == 2002)
y2002_g <- glm(dollar_ex~
                 BMindex + GDP + GNI + age15_64 + Birth + 
                 inflationByCpi + unemployment + ecoGrowthRate + 
                 usa_GDP +usa_GNI + usa_age + usa_Birth + 
                 usa_inflation + usa_une + usa_eco,
               data = y2002)
y2002_AIC <- stepAIC(y2002_g, direction = "both", trace=F)
summary(y2002_AIC)

y2003 <- filter(allData, Year == 2003)
y2003_g <- glm(dollar_ex~
                 BMindex + GDP + GNI + age15_64 + Birth + 
                 inflationByCpi + unemployment + ecoGrowthRate + 
                 usa_GDP +usa_GNI + usa_age + usa_Birth + 
                 usa_inflation + usa_une + usa_eco,
               data = y2003)
y2003_AIC <- stepAIC(y2003_g, direction = "both", trace=F)
summary(y2003_AIC)

y2004 <- filter(allData, Year == 2004)
y2004_g <- glm(dollar_ex~
                 BMindex + GDP + GNI + age15_64 + Birth + 
                 inflationByCpi + unemployment + ecoGrowthRate + 
                 usa_GDP +usa_GNI + usa_age + usa_Birth + 
                 usa_inflation + usa_une + usa_eco,
               data = y2004)
y2004_AIC <- stepAIC(y2002_g, direction = "both", trace=F)
summary(y2004_AIC)

y2005 <- filter(allData, Year == 2005)
y2005_g <- glm(dollar_ex~
                 BMindex + GDP + GNI + age15_64 + Birth + 
                 inflationByCpi + unemployment + ecoGrowthRate + 
                 usa_GDP +usa_GNI + usa_age + usa_Birth + 
                 usa_inflation + usa_une + usa_eco,
               data = y2005)
y2005_AIC <- stepAIC(y2005_g, direction = "both", trace=F)
summary(y2005_AIC)

y2006 <- filter(allData, Year == 2006)
y2006_g <- glm(dollar_ex~
                 BMindex + GDP + GNI + age15_64 + Birth + 
                 inflationByCpi + unemployment + ecoGrowthRate + 
                 usa_GDP +usa_GNI + usa_age + usa_Birth + 
                 usa_inflation + usa_une + usa_eco,
               data = y2006)
y2006_AIC <- stepAIC(y2006_g, direction = "both", trace=F)
summary(y2006_AIC)

y2007 <- filter(allData, Year == 2007)
y2007_g <- glm(dollar_ex~
                 BMindex + GDP + GNI + age15_64 + Birth + 
                 inflationByCpi + unemployment + ecoGrowthRate + 
                 usa_GDP +usa_GNI + usa_age + usa_Birth + 
                 usa_inflation + usa_une + usa_eco,
               data = y2007)
y2007_AIC <- stepAIC(y2007_g, direction = "both", trace=F)
summary(y2007_AIC)

y2008 <- filter(allData, Year == 2008)
y2008_g <- glm(dollar_ex~
                 BMindex + GDP + GNI + age15_64 + Birth + 
                 inflationByCpi + unemployment + ecoGrowthRate + 
                 usa_GDP +usa_GNI + usa_age + usa_Birth + 
                 usa_inflation + usa_une + usa_eco,
               data = y2008)
y2008_AIC <- stepAIC(y2008_g, direction = "both", trace=F)
summary(y2008_AIC)

y2009 <- filter(allData, Year == 2009)
y2009_g <- glm(dollar_ex~
                 BMindex + GDP + GNI + age15_64 + Birth + 
                 inflationByCpi + unemployment + ecoGrowthRate + 
                 usa_GDP +usa_GNI + usa_age + usa_Birth + 
                 usa_inflation + usa_une + usa_eco,
               data = y2009)
y2009_AIC <- stepAIC(y2009_g, direction = "both", trace=F)
summary(y2009_AIC)

y2010 <- filter(allData, Year == 2010)
y2010_g <- glm(dollar_ex~
                 BMindex + GDP + GNI + age15_64 + Birth + 
                 inflationByCpi + unemployment + ecoGrowthRate + 
                 usa_GDP +usa_GNI + usa_age + usa_Birth + 
                 usa_inflation + usa_une + usa_eco,
               data = y2010)
y2010_AIC <- stepAIC(y2010_g, direction = "both", trace=F)
summary(y2010_AIC)

y2011 <- filter(allData, Year == 2011)
y2011_g <- glm(dollar_ex~
                 BMindex + GDP + GNI + age15_64 + Birth + 
                 inflationByCpi + unemployment + ecoGrowthRate + 
                 usa_GDP +usa_GNI + usa_age + usa_Birth + 
                 usa_inflation + usa_une + usa_eco,
               data = y2011)
y2011_AIC <- stepAIC(y2011_g, direction = "both", trace=F)
summary(y2011_AIC)

y2012 <- filter(allData, Year == 2012)
y2012_g <- glm(dollar_ex~
                 BMindex + GDP + GNI + age15_64 + Birth + 
                 inflationByCpi + unemployment + ecoGrowthRate + 
                 usa_GDP +usa_GNI + usa_age + usa_Birth + 
                 usa_inflation + usa_une + usa_eco,
               data = y2012)
y2012_AIC <- stepAIC(y2012_g, direction = "both", trace=F)
summary(y2012_AIC)

y2013 <- filter(allData, Year == 2013)
y2013_g <- glm(dollar_ex~
                 BMindex + GDP + GNI + age15_64 + Birth + 
                 inflationByCpi + unemployment + ecoGrowthRate + 
                 usa_GDP +usa_GNI + usa_age + usa_Birth + 
                 usa_inflation + usa_une + usa_eco,
               data = y2013)
y2013_AIC <- stepAIC(y2013_g, direction = "both", trace=F)
summary(y2013_AIC)

y2014 <- filter(allData, Year == 2014)
y2014_g <- glm(dollar_ex~
                 BMindex + GDP + GNI + age15_64 + Birth + 
                 inflationByCpi + unemployment + ecoGrowthRate + 
                 usa_GDP +usa_GNI + usa_age + usa_Birth + 
                 usa_inflation + usa_une + usa_eco,
               data = y2014)
y2014_AIC <- stepAIC(y2014_g, direction = "both", trace=F)
summary(y2014_AIC)

y2015 <- filter(allData, Year == 2015)
y2015_g <- glm(dollar_ex~
                 BMindex + GDP + GNI + age15_64 + Birth + 
                 inflationByCpi + unemployment + ecoGrowthRate + 
                 usa_GDP +usa_GNI + usa_age + usa_Birth + 
                 usa_inflation + usa_une + usa_eco,
               data = y2015)
y2015_AIC <- stepAIC(y2015_g, direction = "both", trace=F)
summary(y2015_AIC)

y2016 <- filter(allData, Year == 2016)
y2016_g <- glm(dollar_ex~
                 BMindex + GDP + GNI + age15_64 + Birth + 
                 inflationByCpi + unemployment + ecoGrowthRate + 
                 usa_GDP +usa_GNI + usa_age + usa_Birth + 
                 usa_inflation + usa_une + usa_eco,
               data = y2016)
y2016_AIC <- stepAIC(y2016_g, direction = "both", trace=F)
summary(y2016_AIC)

y2017 <- filter(allData, Year == 2017)
y2017_g <- glm(dollar_ex~
                 BMindex + GDP + GNI + age15_64 + Birth + 
                 inflationByCpi + unemployment + ecoGrowthRate + 
                 usa_GDP +usa_GNI + usa_age + usa_Birth + 
                 usa_inflation + usa_une + usa_eco,
               data = y2017)
y2017_AIC <- stepAIC(y2017_g, direction = "both", trace=F)
summary(y2017_AIC)


y2018 <- filter(allData,Year == 2018,BMindex < 300)
y2018_g <- glm(dollar_ex~
                 BMindex + GDP + GNI + age15_64 + Birth + 
                 inflationByCpi + unemployment + ecoGrowthRate + 
                 usa_GDP +usa_GNI + usa_age + usa_Birth + 
                 usa_inflation + usa_une + usa_eco,
               data = y2018)
y2018_AIC <- stepAIC(y2018_g, direction = "both", trace=F)
summary(y2018_AIC)