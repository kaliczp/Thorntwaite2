#Step-by-step work.
library(et.proj)
raw = read.csv2("raw.csv", stringsAsFactors= FALSE) 
raw.xts <- xts(raw[-1] , order.by = as.Date(raw$Index))

PETH.xts <- PETH.gen(raw.xts$t)

forsegment.df <- df.segmentit.gen(raw.xts[,1], PETH.xts, raw.xts$P)

## Initial linear regression
lm.fit <- lm(CREMAP ~ PETH - 1 , data=forsegment.df)
## segmented regression based lm.fit
seg.result <- segmented(lm.fit, seg.Z= ~PETH, psi=40)

## A graphical test
plot(forsegment.df,xlim=c(0,max(forsegment.df$PETH)*1.05),ylim=c(0,max(forsegment.df$CREMAP)*1.05), type="n", xlab="PETH [mm]",ylab="ET CREMAP [mm/month]", xaxs="i", yaxs="i")
points(forsegment.df[forsegment.df$PETH < seg.result$psi[2],], col="red", pch=24, bg="red")
points(forsegment.df[forsegment.df$PETH >= seg.result$psi[2],], col="blue", pch=23, bg="blue")
plot(seg.result,add=T, rug=F, lwd=2)
# slope(seg.result)
curve(slope(seg.result)$PETH[1,1]*x,from=0,to=seg.result$psi[2],add=T, lwd=2)
axis(1,seg.result$psi[2], tck=1, lty="dotted", lab=F)
text(seg.result$psi[2], 0.5, lab=round(seg.result$psi[2],1), srt=90, adj=c(0, -0.3))

PET.proj <- predict.PETH(seg.result, PETH.xts)

SOIL.MAX <- optimize(et.test, interval=c(100,10000), temp = raw.xts$t, prec=raw.xts$P, pet.real=PET.proj, cremap=raw.xts[,1])$minimum

Present = et.calc(SOIL_MAX=SOIL.MAX,Temp = raw.xts$t, Prec=raw.xts$P, PET.real=PET.proj)
Present

PETHknmi.xts <- PETH.gen(knmi.xts$t)
PET.proj.knmi <- predict.PETH(seg.result, PETHknmi.xts)
Future.knmi <- et.calc(SOIL_MAX=SOIL.MAX,Temp = knmi.xts$t, Prec=knmi.xts$P, PET.real=PET.proj.knmi)

PETHdm.xts <- PETH.gen(dm.xts$t)
PET.proj.dm <- predict.PETH(seg.result, PETHdm.xts)
Future.dm <- et.calc(SOIL_MAX=SOIL.MAX,Temp = dm.xts$t, Prec=dm.xts$P, PET.real=PET.proj.dm)

PETHsm.xts <- PETH.gen(sm.xts$t)
PET.proj.sm <- predict.PETH(seg.result, PETHsm.xts)
Future.sm <- et.calc(SOIL_MAX=SOIL.MAX,Temp = sm.xts$t, Prec=sm.xts$P, PET.real=PET.proj.sm)

PETHremo.xts <- PETH.gen(remo.xts$t)
PET.proj.remo <- predict.PETH(seg.result, PETHremo.xts)
Future.remo <- et.calc(SOIL_MAX=SOIL.MAX,Temp = remo.xts$t, Prec=remo.xts$P, PET.real=PET.proj.remo)

####################################################################################################

dat.win <- c('2001/2010','2010/2040','2040/2070','2070/2100')
et.sum.dm <- numeric(4)
for(tti in 1:length(dat.win)) et.sum.dm[tti] <- mean(Future.dm$ET_M[dat.win[tti]],na.rm=T)
ttpredict.time <- c('2005-06-15','2025-06-15','2055-06-15','2085-06-15')
plot(xts(et.sum.dm,as.POSIXct(ttpredict.time)),type="p",pch=18,main="", xaxt="n", ylab="ET_M [mm/month]")
axis(1,at=as.POSIXct(c('2005-06-15','2025-06-15','2055-06-15','2085-06-15')), lab=dat.win)

soil.sum.dm <- numeric(4)
for(tti in 1:length(dat.win)) soil.sum.dm[tti] <- mean(Future.dm$SOIL_M[dat.win[tti]],na.rm=T)
plot(xts(soil.sum.dm,as.Date(ttpredict.time)),type="p",pch=18,main="", xaxt="n", ylab="SOIL_M [mm/month]")
axis(1,at=as.POSIXct(c('2005-06-15','2025-06-15','2055-06-15','2085-06-15')), lab=dat.win)


soil.min.dm <- numeric(4)
for(tti in 1:length(dat.win)) soil.min.dm[tti] <-min(Future.dm$SOIL_M[dat.win[tti]],na.rm=T)
plot(xts(soil.min.dm,as.Date(ttpredict.time)),type="p",pch=18,main="", xaxt="n", ylab="Min. of SOIL_M [mm/month]")
axis(1,at=as.POSIXct(c('2005-06-15','2025-06-15','2055-06-15','2085-06-15')), lab=dat.win)

####################################################x

dat.win <- c('2001/2010','2010/2040','2040/2070','2070/2100')
et.sum.sm <- numeric(4)
for(tti in 1:length(dat.win)) et.sum.sm[tti] <- mean(Future.sm$ET_M[dat.win[tti]],na.rm=T)
ttpredict.time <- c('2005-06-15','2025-06-15','2055-06-15','2085-06-15')
plot(xts(et.sum.sm,as.POSIXct(ttpredict.time)),type="p",pch=18,main="SMHIRCA models evapotranspiration prediction", xaxt="n", ylab="ET_M [mm/month]")
axis(1,at=as.POSIXct(c('2005-06-15','2025-06-15','2055-06-15','2085-06-15')), lab=dat.win)


soil.sum.sm <- numeric(4)
for(tti in 1:length(dat.win)) soil.sum.sm[tti] <- mean(Future.sm$SOIL_M[dat.win[tti]],na.rm=T)
plot(xts(soil.sum.sm,as.Date(ttpredict.time)),type="p",pch=18,main="SMHIRCA models soil moisture prediction", xaxt="n", ylab="SOIL_M [mm/month]")
axis(1,at=as.POSIXct(c('2005-06-15','2025-06-15','2055-06-15','2085-06-15')), lab=dat.win)


soil.min.sm <- numeric(4)
for(tti in 1:length(dat.win)) soil.min.sm[tti] <-min(Future.sm$SOIL_M[dat.win[tti]],na.rm=T)
plot(xts(soil.min.sm,as.Date(ttpredict.time)),type="p",pch=18,main="", xaxt="n", ylab="Min. of SOIL_M [mm/month]")
axis(1,at=as.POSIXct(c('2005-06-15','2025-06-15','2055-06-15','2085-06-15')), lab=dat.win)

################################################################################################

dat.win <- c('2001/2010','2010/2040','2040/2070','2070/2100')
et.sum.remo <- numeric(4)
for(tti in 1:length(dat.win)) et.sum.remo[tti] <- mean(Future.remo$ET_M[dat.win[tti]],na.rm=T)
ttpredict.time <- c('2005-06-15','2025-06-15','2055-06-15','2085-06-15')
plot(xts(et.sum.remo,as.POSIXct(ttpredict.time)),type="p",pch=18,main="REMO models evapotranspiration prediction", xaxt="n", ylab="ET_M [mm/month]")
axis(1,at=as.POSIXct(c('2005-06-15','2025-06-15','2055-06-15','2085-06-15')), lab=dat.win)


soil.sum.remo <- numeric(4)
for(tti in 1:length(dat.win)) soil.sum.remo[tti] <- mean(Future.remo$SOIL_M[dat.win[tti]],na.rm=T)
plot(xts(soil.sum.remo,as.Date(ttpredict.time)),type="p",pch=18,main="Remo models soil moisture prediction", xaxt="n", ylab="SOIL_M [mm/month]")
axis(1,at=as.POSIXct(c('2005-06-15','2025-06-15','2055-06-15','2085-06-15')), lab=dat.win)


soil.min.remo <- numeric(4)
for(tti in 1:length(dat.win)) soil.min.remo[tti] <-min(Future.remo$SOIL_M[dat.win[tti]],na.rm=T)
plot(xts(soil.min.remo,as.Date(ttpredict.time)),type="p",pch=18,main="", xaxt="n", ylab="Min. of SOIL_M [mm/month]")
axis(1,at=as.POSIXct(c('2005-06-15','2025-06-15','2055-06-15','2085-06-15')), lab=dat.win)

###################################################################################################

dat.win <- c('2001/2010','2010/2040','2040/2070','2070/2100')
et.sum.knmi <- numeric(4)
for(tti in 1:length(dat.win)) et.sum.knmi[tti] <- mean(Future.knmi$ET_M[dat.win[tti]],na.rm=T)
ttpredict.time <- c('2005-06-15','2025-06-15','2055-06-15','2085-06-15')
plot(xts(et.sum,as.POSIXct(ttpredict.time)),type="p",pch=18,main="KNMIRACMO2 models evapotranspiration prediction", xaxt="n", ylab="ET_M [mm/month]")
axis(1,at=as.POSIXct(c('2005-06-15','2025-06-15','2055-06-15','2085-06-15')), lab=dat.win)


soil.sum.knmi <- numeric(4)
for(tti in 1:length(dat.win)) soil.sum.knmi[tti] <- mean(Future.knmi$SOIL_M[dat.win[tti]],na.rm=T)
plot(xts(soil.sum.knmi,as.Date(ttpredict.time)),type="p",pch=18, main="KNMIRACMO2 models soil moisture prediction", xaxt="n", ylab="SOIL_M [mm/month]")
axis(1,at=as.POSIXct(c('2005-06-15','2025-06-15','2055-06-15','2085-06-15')), lab=dat.win)


soil.min.knmi <- numeric(4)
for(tti in 1:length(dat.win)) soil.min.knmi[tti] <-min(Future.knmi$SOIL_M[dat.win[tti]],na.rm=T)
plot(xts(soil.min.knmi,as.Date(ttpredict.time)),type="p",pch=18,main="", xaxt="n", ylab="Min. of SOIL_M [mm/month]")
axis(1,at=as.POSIXct(c('2005-06-15','2025-06-15','2055-06-15','2085-06-15')), lab=dat.win)


###################################################################################################

### Közös ábra ET

plot(xts(et.sum.dm,as.POSIXct(ttpredict.time)),type="p",pch=18,main="", xaxt="n", ylab="ET_M [mm/month]",ylim=c(35,55))
axis(1,at=as.POSIXct(c('2005-06-15','2025-06-15','2055-06-15','2085-06-15')), lab=dat.win)
points(xts(et.sum.dm,as.POSIXct(ttpredict.time)),pch=18)
points(xts(et.sum.sm,as.POSIXct(ttpredict.time)),pch=17,col="darkgreen")
points(xts(et.sum.remo,as.POSIXct(ttpredict.time)),pch=16,col="darkblue")
points(xts(et.sum.knmi,as.POSIXct(ttpredict.time)),pch=16,col="red")

legend("bottomright",c("remo","smhirca","dm", "knmiracmo2"),pch=c(16,17,18,16),col=c("darkblue","darkgreen","black", "red"), cex=0.8)


###################################################################################################

#### SOIL közös ábára

## Közös ábra
plot(xts(soil.sum.dm,as.POSIXct(ttpredict.time)),type="p",pch=18,main="", xaxt="n", ylab="SOIL_M [mm/month]",ylim=c(150,620))
axis(1,at=as.POSIXct(c('2005-06-15','2025-06-15','2055-06-15','2085-06-15')), lab=dat.win)
points(xts(soil.sum.dm,as.POSIXct(ttpredict.time)),pch=18)
points(xts(soil.sum.sm,as.POSIXct(ttpredict.time)),pch=17,col="darkgreen")
points(xts(soil.sum.remo,as.POSIXct(ttpredict.time)),pch=16,col="darkblue")
points(xts(soil.sum.knmi,as.POSIXct(ttpredict.time)),pch=19,col="red")
legend("bottomright",c("remo","smhirca","dm", "knmiracmo2"),pch=c(16,17,18,15),col=c("darkblue","darkgreen","black", "red"), cex=0.7)
lines(xts(soil.sum.allaverage,as.POSIXct(ttpredict.time)),pch=19,col="gold")

########################

#### SOIL_M !MIN! közös ábra

## Közös ábra
plot(xts(soil.min.dm,as.POSIXct(ttpredict.time)),type="p",pch=18,main="", xaxt="n", ylab="SOIL_M [mm/month]",ylim=c(0,405))
axis(1,at=as.POSIXct(c('2005-06-15','2025-06-15','2055-06-15','2085-06-15')), lab=dat.win)
points(xts(soil.min.dm,as.POSIXct(ttpredict.time)),pch=18)
points(xts(soil.min.sm,as.POSIXct(ttpredict.time)),pch=17,col="darkgreen")
points(xts(soil.min.remo,as.POSIXct(ttpredict.time)),pch=16,col="darkblue")
points(xts(soil.min.knmi,as.POSIXct(ttpredict.time)),pch=19,col="red")

legend("topleft",c("remo","smhirca","dm", "knmiracmo2"),pch=c(16,17,18,15),col=c("darkblue","darkgreen","black", "red"), cex=0.65)
lines(xts(soil.min.allaverage,as.POSIXct(ttpredict.time)),pch=19,col="gold")
##################################################################################################

plot.prectemp <- function(temp, prec , xaxt = "s") {
  ## plot temp.
  temp.min = -10
  temp.max = 35
  plot(temp, main="", ylim=c(temp.min, temp.max), xaxs= "i", yaxs = "i", xaxt = xaxt)
  par( new=TRUE)
  ## plot prec.
  prec.max = 350
  plot(prec, type="h", ylim=c(prec.max, 0), main="", axes=FALSE, xaxs= "i", yaxs = "i")
  axis(4)
}

par(mfrow = c(3,1), mar=c(0, 4.1, 0, 4.1), oma = c(4.4, 0, 0.5, 0), las=1)
plot.prectemp(raw.xts$t, raw.xts$P, xaxt = "n")

plot(raw.xts$ET.CREMAP, main="", xaxt = "n", xaxs= "i")
lines(Present$ET_M, col="magenta")

plot(Present$SOIL_M, main ="", xaxs= "i")

##############################################################################################x

dir(patt="00.csv")

rcm.readin <- function(file.past, file.future) {
    past <- read.csv2(file.past, stringsAsFactors= FALSE)
    future <- read.csv2(file.future, stringsAsFactors= FALSE)
    raw <- rbind(past,future)
    date.full <- as.Date(raw$Date, format="%Y-%m-%d")
    xts(raw[-1] , order.by = date.full )
}

## MEGNÉZNI!!! A jövőben 30 naposak a hónapok!!! Hogyan korrigáljuk? A múltban miért nem 30 napos?
dmi.arpege <- rcm.readin("DMI_HIRHAM5_A1B_ARPEGE_1951_2000.csv","DMI_HIRHAM5_A1B_ARPEGE_2001_2100.csv")

plot(dmi.arpege$T, xaxs="i")
plot(dmi.arpege$P, typ="h", xaxs="i")

## Elgondolkodtató
pdf(width=28)
plot(apply.yearly(dmi.arpege$P,sum),xaxs="i",type="n",main="DMI.ARPEGE csapadék")
lines(apply.yearly(dmi.arpege$P,sum),typ="h",col="royalblue",lwd=2)

dmi.echam <- rcm.readin( "DMI_HIRHAM5_ECHAM5_1951_2000.csv","DMI_HIRHAM5_ECHAM5_2001_2100.csv")

plot(apply.yearly(dmi.echam$P,sum),xaxs="i",type="n",main="DMI.ECHAM csapadék")
lines(apply.yearly(dmi.echam$P,sum),typ="h",col="royalblue",lwd=2)


dmechproj = read.csv2("DMI_HIRHAM5_ECHAM5_1951_2000.csv", stringsAsFactors= FALSE) 
dmechproj.xts <- xts(dmechproj[-1] , order.by = as.Date(dmechproj$Date, format="%Y-%m-%d"))
plot(dmechproj.xts$T)

remoproj = read.csv2("MPI-M-REMO_SCN_ECHAM5_1951_2000.csv", stringsAsFactors= FALSE) 
remoproj.xts <- xts(remoproj[-1] , order.by = as.Date(remoproj$Date, format="%Y-%m-%d"))
plot(remoproj.xts$T)

dmproj = read.csv2("DMI_HIRHAM5_A1B_ARPEGE_1951_2000.csv", stringsAsFactors= FALSE) 
dmproj.xts <- xts(dmproj[-1] , order.by = as.Date(dmproj$Date, format="%Y-%m-%d"))
plot(dmproj.xts$T)

knmiproj = read.csv2("KNMI_RACMO2_A1B_ECHAM5_r3_1951_2000.csv", stringsAsFactors= FALSE) 
knmiproj.xts <- xts(knmiproj[-1] , order.by = as.Date(knmiproj$Date, format="%Y-%m-%d"))
plot(knmiproj.xts$T)

smhircaproj = read.csv2("SMHIRCA_A1B_BCM_1961_2000.csv", stringsAsFactors= FALSE) 
smhircaproj.xts <- xts(smhircaproj[-1] , order.by = as.Date(smhircaproj$Date, format="%Y-%m-%d"))
plot(smhircaproj.xts$T)

dmfutureproj = read.csv2("DMI_HIRHAM5_ECHAM5_2001_2100.csv", stringsAsFactors= FALSE) 
dmfutureproj.xts <- xts(dmfutureproj[-1] , order.by = as.Date(dmfutureproj$Date, format="%Y-%m-%d"))
plot(dmfutureproj.xts$T)
