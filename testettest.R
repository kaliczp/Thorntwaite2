et.test(200, test.meteo.xts$t, test.meteo.xts$P, PET.proj, test.meteo.xts$ET-CREMAP)

SOIL.MAX <- optimize(et.test, interval=c(100,10000), temp = test.meteo.xts$t, prec=test.meteo.xts$P, pet.real=PET.proj, cremap=test.meteo.xts$ET-CREMAP)$minimum

## Visual scanning SOIL.MAX parameter
plot(700,et.test(700,test.meteo.xts$t, test.meteo.xts$P, PET.proj, test.meteo.xts$ET-CREMAP),xlim=c(100,10000),ylim=c(1000,300000),type="n", xlab="SOIL_MAX", ylab="RSS")
for(tti in seq(100,1000,50)){
  points(tti,et.test(tti,test.meteo.xts$t, test.meteo.xts$P, PET.proj, test.meteo.xts$ET-CREMAP), pch=".")
}
axis(1,500,tck=1,lab=F)
SOIL.MAX <- 500

et.calc(SOIL.MAX,Temp = test.meteo.xts$t, Prec=test.meteo.xts$P, PET.real=PET.proj)
