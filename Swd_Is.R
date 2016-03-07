plot(Future.knmi$SOIL_M)
plot(Future.remo$SOIL_M)
plot(Future.sm$SOIL_M, ylim=c(100, 520))
plot(Future.dm$SOIL_M)

rcm.soilm.avg = (Future.knmi['2001/2098','SOIL_M'] + Future.sm['2001/2098','SOIL_M'] + Future.remo['2001/2098','SOIL_M'] + Future.dm['2001/2098','SOIL_M']) / 4
plot(rcm.soilm.avg, ylim=c(100,520))
axis(2,SOIL.MAX*0.4, lab=F,tck=1,col=2)

## Szárazság stressz havi
## KNMI

swd.havi.knmi = Future.knmi[(SOIL.MAX*0.4-Future.knmi$SOIL_M) >0,"SOIL_M"]
plot(swd.havi.knmi,type = "h")

## (SOIL.MAX*0.4-Future.knmi$SOIL_M) >0  --->>> TRUE/FALSE -ba melyik a vízstresszes!

swd.eves.knmi=apply.yearly(swd.havi.knmi,mean)
sum(coredata(swd.eves.knmi['2070/2100']))/SOIL.MAX/30
sum(coredata(swd.eves.knmi['2040/2070']))/SOIL.MAX/30
sum(coredata(swd.eves.knmi['2010/2040']))/SOIL.MAX/30
sum(coredata(swd.eves.knmi['1980/2010']))/SOIL.MAX/30

sum(coredata(swd.havi.knmi['2071']))/SOIL.MAX
############################################################
## DM

swd.havi.dm = Future.dm[(SOIL.MAX*0.4-Future.dm$SOIL_M) >0,"SOIL_M"]
plot(swd.havi.dm,type = "h")
swd.eves.dm=apply.yearly(swd.havi.dm,mean)
sum(coredata(swd.eves.dm['2070/2100']))/SOIL.MAX/30
sum(coredata(swd.eves.dm['2040/2070']))/SOIL.MAX/30
sum(coredata(swd.eves.dm['2010/2040']))/SOIL.MAX/30
sum(coredata(swd.eves.dm['1980/2010']))/SOIL.MAX/30
##############################################################
## SM

swd.havi.sm = Future.sm[(SOIL.MAX*0.4-Future.sm$SOIL_M) >0,"SOIL_M"]
plot(swd.havi.sm,type = "h")
swd.eves.sm=apply.yearly(swd.havi.sm,mean)
sum(coredata(swd.eves.sm['2070/2100']))/SOIL.MAX/30
sum(coredata(swd.eves.sm['2040/2070']))/SOIL.MAX/30
sum(coredata(swd.eves.sm['2010/2040']))/SOIL.MAX/30
sum(coredata(swd.eves.sm['2001/2010']))/SOIL.MAX/10
#############################################################
## remo

swd.havi.remo = Future.remo[(SOIL.MAX*0.4-Future.remo$SOIL_M) >0,"SOIL_M"]
plot(swd.havi.remo,type = "h")
swd.eves.remo=apply.yearly(swd.havi.remo,mean)
sum(coredata(swd.eves.remo['2070/2100']))/SOIL.MAX/30
sum(coredata(swd.eves.remo['2040/2070']))/SOIL.MAX/30
sum(coredata(swd.eves.remo['2010/2040']))/SOIL.MAX/30
sum(coredata(swd.eves.remo['1980/2010']))/SOIL.MAX/30
#################################################################

## rew(model=Future.remo$SOIL_M,soil.max=SOIL.MAX) --->> Ezzel megy a rew a remo esetén
 
rew = function(model, soil.max) {
  REW = model/soil.max
  plot(REW)
  mean.2001= mean(REW['1980/2009'])
  mean.2010= mean(REW['2010/2039'])
  mean.2040= mean(REW['2040/2069'])
  mean.2070= mean(REW['2070/2099'])
  data.frame(mean.2001,mean.2010,mean.2040,mean.2070)
}

################ Present REW

Present$SOIL_M/SOIL.MAX
rewPresent= Present$SOIL_M/SOIL.MAX
plot(rewPresent, ylim=c(0.3, 1))
################ Present SWD

PresentSWD=SOIL.MAX*0.4-Present$SOIL_M
PresentSWD[PresentSWD >0]
