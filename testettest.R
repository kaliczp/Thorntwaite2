et.test(200, test.meteo.xts$t, test.meteo.xts$P, PET.proj, test.meteo.xts$ET-CREMAP)

SOIL.MAX <- optimize(et.test, interval=c(100,10000), temp = test.meteo.xts$t, prec=test.meteo.xts$P, pet.real=PET.proj, cremap=test.meteo.xts$ET-CREMAP)$minimum

## Visual scanning SOIL.MAX parameter
tempsoilm <- seq(100,1000,50)
tempsoilm.df <- data.frame(soilmax=tempsoilm, RSS=numeric(length(tempsoilm)))
for(tti in 1:nrow(tempsoilm.df)){
  tempsoilm.df[tti,"RSS"] <- et.test(tempsoilm.df[tti,"soilmax"],test.meteo.xts$t, test.meteo.xts$P, PET.proj, test.meteo.xts$ET-CREMAP)
}
plot(tempsoilm.df, type="p", , pch=".", xlab="SOIL_MAX", ylab="RSS")

et.calc(SOIL.MAX,Temp = test.meteo.xts$t, Prec=test.meteo.xts$P, PET.real=PET.proj)
