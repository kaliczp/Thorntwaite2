#Step-by-step work.

###### Present; 'Base model' ##########

library(et.proj)
library(ncdf4)
raw = read.csv2("raw.csv", stringsAsFactors= FALSE) 
raw.xts <- xts(raw[-1] , order.by = as.Date(raw$Index,"%Y.%m.%d"))

PETH.xts <- PETH.gen(raw.xts$t)

forsegment.df <- df.segmentit.gen(raw.xts[,1], PETH.xts, raw.xts$P)

## Initial linear regression
lm.fit <- lm(CREMAP ~ PETH - 1 , data=forsegment.df)
## segmented regression based lm.fit
seg.result <- segmented(lm.fit, seg.Z= ~PETH, psi=40)

## A graphical test; segmented regression 
plot(forsegment.df,xlim=c(0,max(forsegment.df$PETH)*1.05),ylim=c(0,max(forsegment.df$CREMAP)*1.05), type="n", xlab="PETH [mm]",ylab="ET CREMAP [mm/month]", xaxs="i", yaxs="i")
points(forsegment.df[forsegment.df$PETH < seg.result$psi[2],], col="red", pch=24, bg="red")
points(forsegment.df[forsegment.df$PETH >= seg.result$psi[2],], col="blue", pch=23, bg="blue")
plot(seg.result,add=T, rug=F, lwd=2)
# slope(seg.result)
curve(slope(seg.result)$PETH[1,1]*x,from=0,to=seg.result$psi[2],add=T, lwd=2)
axis(1,seg.result$psi[2], tck=1, lty="dotted", lab=F)
text(seg.result$psi[2], 0.5, lab=round(seg.result$psi[2],1), srt=90, adj=c(0, -0.3))

PET.proj <- predict.PETH(seg.result, PETH.xts)

SOIL.MAX <- optimize(et.test, interval=c(100,10000), temp = raw.xts$t, prec=raw.xts$P, pet.real=PET.proj, cremap=raw.xts$ET.CREMAP)$minimum

Present = et.calc(SOIL_MAX=SOIL.MAX,Temp = raw.xts$t, Prec=raw.xts$P, PET.real=PET.proj)

########################################################################

## Adatok kinyerése a jelenre

plot(coredata(raw.xts$ET.CREMAP) ~ coredata(Present$ET_M), pch=18,col="darkgreen", xlab="ET_M", ylab="PET CREMAP", xlim = c(0,140), ylim=c(0,160))
Tttmp.lm <- lm(coredata(raw.xts$ET.CREMAP) ~ coredata(Present$ET_M))
Tttmp.sum <- summary(Tttmp.lm)
abline(Tttmp.lm)
legend("topleft",c(paste("ET CREMAP =",round(coef(Tttmp.lm)[2],2) ,"* ET_M +",round(coef(Tttmp.lm)[1],2)),paste("R^2 =",round(Tttmp.sum$r.squared,2))))

################################## csap és hőmérséklet egy ábárán!! ##############

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

plot(raw.xts$ET.CREMAP, main="", xaxs= "i")
lines(Present$ET_M, col="green")

plot(Present$SOIL_M, main ="", xaxs= "i")

### Future phase ###
##########################################################################################
##########################################################################################

### rcm.readin függvény; egy idősorba olvassa be a multbeli és a jövőbeli modellezést

dir(patt="00.csv")

rcm.readin <- function(file.past, file.future) {
    past <- read.csv2(file.past, stringsAsFactors= FALSE)
    future <- read.csv2(file.future, stringsAsFactors= FALSE)
    raw <- rbind(past,future)
    date.full <- as.Date(raw$Date, format="%Y-%m-%d")
    xts(raw[-1] , order.by = date.full )
}

## MEGNÉZNI!!! A jövőben 30 naposak a hónapok!!! Hogyan korrigáljuk? A múltban miért nem 30 napos?
## dmi.arpege <- rcm.readin("DMI_HIRHAM5_A1B_ARPEGE_1951_2000.csv","DMI_HIRHAM5_A1B_ARPEGE_2001_2100.csv")
## dmi.echam <- rcm.readin( "DMI_HIRHAM5_ECHAM5_1951_2000.csv","DMI_HIRHAM5_ECHAM5_2001_2100.csv")
## knmi.racmo2 <- rcm.readin("KNMI_RACMO2_A1B_ECHAM5_r3_1951_2000.csv","KNMI_RACMO2_A1B_ECHAM5_r3_2001_2100.csv")
## remo.echam <- rcm.readin("MPI-M-REMO_SCN_ECHAM5_1951_2000.csv","MPI-M-REMO_SCN_ECHAM5_2001_2100.csv")
      ## MEGNÉZNI vmi hiba!
## smhirca.bcm <- rcm.readin("SMHIRCA_A1B_BCM_1961_2000.csv","SMHIRCA_A1B_BCM_2001_2100.csv")
## smhirca.bcm <- smhirca.bcm['1961-01-01/2100-01-11',]

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

## smhirca.bcm=smhirca.bcm['/2099-12-31'] ITT: 2100-01-11. UTÁN rossz az adatsor!
## Ezért: az utolsó teljes év lett kiválasztva.
  
plot(smhirca.bcm$T, xaxs="i")
plot(dmi.echam$P, typ="h", xaxs="i") ## xaxs ="i" szélét kihúzza

######## Havi átlagok képzése

monthly.T_dmi.arpege <- apply.monthly(dmi.arpege$T,mean)
monthly.P_dmi.arpege <- apply.monthly(dmi.arpege$P,sum)

monthly.T_dmi.echam <- apply.monthly(dmi.echam$t,mean)
monthly.P_dmi.echam <- apply.monthly(dmi.echam$p,sum)

monthly.T_knmi.racmo2 <- apply.monthly(knmi.racmo2$t,mean)
monthly.P_knmi.racmo2 <- apply.monthly(knmi.racmo2$p,sum)

monthly.T_remo.echam <- apply.monthly(remo.echam$t,mean)
monthly.P_remo.echam <- apply.monthly(remo.echam$p,sum)

monthly.T_smhirca.bcm <- apply.monthly(smhirca.bcm$t,mean)
monthly.P_smhirca.bcm <- apply.monthly(smhirca.bcm$p,sum)

###################### RCMS átlagok, T-re és P-re! a 4 vizsgálati időszakba #######

mean(monthly.T_smhirca.bcm['1980/2009'])
mean(monthly.T_smhirca.bcm['2010/2039'])
mean(monthly.T_smhirca.bcm['2040/2069'])
mean(monthly.T_smhirca.bcm['2070/2100'])

mean(monthly.T_remo.echam['2070/2100'])
mean(monthly.T_dmi.echam['2070/2100'])
mean(monthly.T_knmi.racmo2['2070/2100'])

mean(apply.yearly(monthly.P_smhirca.bcm, sum)['1980/2009'])
sum(monthly.P_dmi.echam['2030'])

######################## PETH future; PET.proj.knmi: becsül AET-t! Future.knmi: AET-ből vízmérleg. 

PETHknmi.xts <- PETH.gen(monthly.T_knmi.racmo2)  ### LATITUDE-ra figyelni.
PET.proj.knmi <- predict.PETH(seg.result, PETHknmi.xts)
Future.knmi <- et.calc(SOIL_MAX=SOIL.MAX,Temp = monthly.T_knmi.racmo2$t, Prec=monthly.P_knmi.racmo2$p, PET.real=PET.proj.knmi)

PETHdm.xts <- PETH.gen(monthly.T_dmi.echam)  ####### !!!!! ARPEGE KELL MÉG!!!!!!!!!!!!! ########
PET.proj.dm <- predict.PETH(seg.result, PETHdm.xts)
Future.dm <- et.calc(SOIL_MAX=SOIL.MAX,Temp = monthly.T_dmi.echam$t, Prec=monthly.P_dmi.echam$p, PET.real=PET.proj.dm)

PETHsm.xts <- PETH.gen(monthly.T_smhirca.bcm)
PET.proj.sm <- predict.PETH(seg.result, PETHsm.xts)
Future.sm <- et.calc(SOIL_MAX=SOIL.MAX,Temp = monthly.T_smhirca.bcm$t, Prec=monthly.P_smhirca.bcm$p, PET.real=PET.proj.sm)

PETHremo.xts <- PETH.gen(monthly.T_remo.echam)
PET.proj.remo <- predict.PETH(seg.result, PETHremo.xts)
Future.remo <- et.calc(SOIL_MAX=SOIL.MAX,Temp = monthly.T_remo.echam$t, Prec=monthly.P_remo.echam$p, PET.real=PET.proj.remo)

####################################################################################################

dat.win <- c('1980/2009','2010/2039','2040/2069','2070/2099')
dat.win.plt <- c('1980/2010','2010/2040','2040/2070','2070/2100')
et.sum.dm <- numeric(4)
for(tti in 1:length(dat.win)) {et.sum.dm[tti] <- mean(Future.dm$ET_M[dat.win[tti]],na.rm=T)}
ttpredict.time <- c('1995-06-15','2025-06-15','2055-06-15','2085-06-15')
plot(xts(et.sum.dm,as.POSIXct(ttpredict.time)),type="p",pch=18,main="", xaxt="n", ylab="ET_M [mm/month]")
axis(1,at=as.POSIXct(c('1995-06-15','2025-06-15','2055-06-15','2085-06-15')), lab=dat.win.plt)

soil.sum.dm <- numeric(4)
for(tti in 1:length(dat.win)) soil.sum.dm[tti] <- mean(Future.dm$SOIL_M[dat.win[tti]],na.rm=T)
plot(xts(soil.sum.dm,as.Date(ttpredict.time)),type="p",pch=18,main="", xaxt="n", ylab="SOIL_M [mm/month]")
axis(1,at=as.POSIXct(c('1995-06-15','2025-06-15','2055-06-15','2085-06-15')), lab=dat.win.plt)

soil.min.dm <- numeric(4)
for(tti in 1:length(dat.win)) soil.min.dm[tti] <-min(Future.dm$SOIL_M[dat.win[tti]],na.rm=T)
plot(xts(soil.min.dm,as.Date(ttpredict.time)),type="p",pch=18,main="", xaxt="n", ylab="Min. of SOIL_M [mm/month]")
axis(1,at=as.POSIXct(c('1995-06-15','2025-06-15','2055-06-15','2085-06-15')), lab=dat.win.plt)

#####################################################################################################

dat.win <- c('1980/2009','2010/2039','2040/2069','2070/2099')
et.sum.sm <- numeric(4)
for(tti in 1:length(dat.win)) et.sum.sm[tti] <- mean(Future.sm$ET_M[dat.win[tti]],na.rm=T)
ttpredict.time <- c('1995-06-15','2025-06-15','2055-06-15','2085-06-15')
plot(xts(et.sum.sm,as.POSIXct(ttpredict.time)),type="p",pch=18,main="SMHIRCA models evapotranspiration prediction", xaxt="n", ylab="ET_M [mm/month]")
axis(1,at=as.POSIXct(c('1995-06-15','2025-06-15','2055-06-15','2085-06-15')), lab=dat.win.plt)

soil.sum.sm <- numeric(4)
for(tti in 1:length(dat.win)) soil.sum.sm[tti] <- mean(Future.sm$SOIL_M[dat.win[tti]],na.rm=T)
plot(xts(soil.sum.sm,as.Date(ttpredict.time)),type="p",pch=18,main="SMHIRCA models soil moisture prediction", xaxt="n", ylab="SOIL_M [mm/month]")
axis(1,at=as.POSIXct(c('1995-06-15','2025-06-15','2055-06-15','2085-06-15')), lab=dat.win.plt)

soil.min.sm <- numeric(4)
for(tti in 1:length(dat.win)) soil.min.sm[tti] <-min(Future.sm$SOIL_M[dat.win[tti]],na.rm=T)
plot(xts(soil.min.sm,as.Date(ttpredict.time)),type="p",pch=18,main="", xaxt="n", ylab="Min. of SOIL_M [mm/month]")
axis(1,at=as.POSIXct(c('1995-06-15','2025-06-15','2055-06-15','2085-06-15')), lab=dat.win.plt)

################################################################################################

dat.win <- c('1980/2009','2010/2039','2040/2069','2070/2099')
et.sum.remo <- numeric(4)
for(tti in 1:length(dat.win)) et.sum.remo[tti] <- mean(Future.remo$ET_M[dat.win[tti]],na.rm=T)
ttpredict.time <- c('1995-06-15','2025-06-15','2055-06-15','2085-06-15')
plot(xts(et.sum.remo,as.POSIXct(ttpredict.time)),type="p",pch=18,main="REMO models evapotranspiration prediction", xaxt="n", ylab="ET_M [mm/month]")
axis(1,at=as.POSIXct(c('1995-06-15','2025-06-15','2055-06-15','2085-06-15')), lab=dat.win.plt)

soil.sum.remo <- numeric(4)
for(tti in 1:length(dat.win)) soil.sum.remo[tti] <- mean(Future.remo$SOIL_M[dat.win[tti]],na.rm=T)
plot(xts(soil.sum.remo,as.Date(ttpredict.time)),type="p",pch=18,main="Remo models soil moisture prediction", xaxt="n", ylab="SOIL_M [mm/month]")
axis(1,at=as.POSIXct(c('1995-06-15','2025-06-15','2055-06-15','2085-06-15')), lab=dat.win.plt)

soil.min.remo <- numeric(4)
for(tti in 1:length(dat.win)) soil.min.remo[tti] <-min(Future.remo$SOIL_M[dat.win[tti]],na.rm=T)
plot(xts(soil.min.remo,as.Date(ttpredict.time)),type="p",pch=18,main="", xaxt="n", ylab="Min. of SOIL_M [mm/month]")
axis(1,at=as.POSIXct(c('1995-06-15','2025-06-15','2055-06-15','2085-06-15')), lab=dat.win.plt)

###################################################################################################

dat.win <- c('1980/2009','2010/2039','2040/2069','2070/2099')
et.sum.knmi <- numeric(4)
for(tti in 1:length(dat.win)) et.sum.knmi[tti] <- mean(Future.knmi$ET_M[dat.win[tti]],na.rm=T)
ttpredict.time <- c('1995-06-15','2025-06-15','2055-06-15','2085-06-15')
plot(xts(et.sum.knmi,as.POSIXct(ttpredict.time)),type="p",pch=18,main="KNMIRACMO2 models evapotranspiration prediction", xaxt="n", ylab="ET_M [mm/month]")
axis(1,at=as.POSIXct(c('1995-06-15','2025-06-15','2055-06-15','2085-06-15')), lab=dat.win.plt)

soil.sum.knmi <- numeric(4)
for(tti in 1:length(dat.win)) soil.sum.knmi[tti] <- mean(Future.knmi$SOIL_M[dat.win[tti]],na.rm=T)
plot(xts(soil.sum.knmi,as.Date(ttpredict.time)),type="p",pch=18, main="KNMIRACMO2 models soil moisture prediction", xaxt="n", ylab="SOIL_M [mm/month]")
axis(1,at=as.POSIXct(c('1995-06-15','2025-06-15','2055-06-15','2085-06-15')), lab=dat.win.plt)

soil.min.knmi <- numeric(4)
for(tti in 1:length(dat.win)) soil.min.knmi[tti] <-min(Future.knmi$SOIL_M[dat.win[tti]],na.rm=T)
plot(xts(soil.min.knmi,as.Date(ttpredict.time)),type="p",pch=18,main="", xaxt="n", ylab="Min. of SOIL_M [mm/month]")
axis(1,at=as.POSIXct(c('1995-06-15','2025-06-15','2055-06-15','2085-06-15')), lab=dat.win.plt)

###################################################################################################
###################################################################################################

### Közös ábra ET

plot(xts(et.sum.dm/et.sum.dm[1]*100,as.POSIXct(ttpredict.time)),type="p",pch=15, cex=1.4 ,main="", xaxt="n", ylab="ET_M [%]",ylim=c(90,120))
axis(1,at=as.POSIXct(c('1995-06-15','2025-06-15','2055-06-15','2085-06-15')), lab=dat.win.plt)
points(xts(et.sum.dm/et.sum.dm[1]*100,as.POSIXct(ttpredict.time)),pch=15, cex=1.4)
points(xts(et.sum.sm/et.sum.sm[1]*100,as.POSIXct(ttpredict.time)),pch=17, cex=1.4, col="darkgreen")
points(xts(et.sum.remo/et.sum.remo[1]*100,as.POSIXct(ttpredict.time)),pch=16, cex=1.4, col="darkblue")
points(xts(et.sum.knmi/et.sum.knmi[1]*100,as.POSIXct(ttpredict.time)),pch=8, cex=1.4, col="red")
lines(xts(et.sum.allavg/et.sum.allavg[1]*100,as.POSIXct(ttpredict.time)),pch=19,col="gold")

legend("topleft",c("remo","smhirca","dm", "knmiracmo2"),pch=c(16,17,15,8),col=c("darkblue","darkgreen","black", "red"), cex=0.8)

###################################################################################################

#### SOIL közös ábára

## Közös ábra
plot(xts(soil.sum.dm/soil.sum.dm[1]*100,as.POSIXct(ttpredict.time)),type="p",pch=15, cex=1.4, main="", xaxt="n", ylab="SOIL_M [%]",ylim=c(65,104))
axis(1,at=as.POSIXct(c('1995-06-15','2025-06-15','2055-06-15','2085-06-15')), lab=dat.win.plt)
points(xts(soil.sum.dm/soil.sum.dm[1]*100,as.POSIXct(ttpredict.time)),pch=15, cex=1.4)
points(xts(soil.sum.sm/soil.sum.sm[1]*100,as.POSIXct(ttpredict.time)),pch=17, cex=1.4, col="darkgreen")
points(xts(soil.sum.remo/soil.sum.remo[1]*100,as.POSIXct(ttpredict.time)),pch=16, cex=1.4, col="darkblue")
points(xts(soil.sum.knmi/soil.sum.knmi[1]*100,as.POSIXct(ttpredict.time)),pch=8, cex=1.4, col="red")
legend("topleft",c("remo","smhirca","dm", "knmiracmo2"),pch=c(16,17,15,8),col=c("darkblue","darkgreen","black", "red"), cex=0.7)
lines(xts(soil.sum.allavg/soil.sum.allavg[1]*100,as.POSIXct(ttpredict.time)),pch=19,col="gold")

####################################################################################################

#### SOIL_M !MIN! közös ábra

## Közös ábra
plot(xts(soil.min.dm/soil.min.dm[1]*100,as.POSIXct(ttpredict.time)),type="p",pch=15, cex=1.4, main="", xaxt="n", ylab="SOIL_M [%]",ylim=c(0,400))
axis(1,at=as.POSIXct(c('1995-06-15','2025-06-15','2055-06-15','2085-06-15')), lab=dat.win.plt)
points(xts(soil.min.dm/soil.min.dm[1]*100,as.POSIXct(ttpredict.time)),pch=15, cex=1.4)
points(xts(soil.min.sm/soil.min.sm[1]*100,as.POSIXct(ttpredict.time)),pch=17, cex=1.4, col="darkgreen")
points(xts(soil.min.remo/soil.min.remo[1]*100,as.POSIXct(ttpredict.time)),pch=16, cex=1.4, col="darkblue")
points(xts(soil.min.knmi/soil.min.knmi[1]*100,as.POSIXct(ttpredict.time)),pch=8, cex=1.4, col="red")

legend("topleft",c("remo","smhirca","dm", "knmiracmo2"),pch=c(16,17,15,8),col=c("darkblue","darkgreen","black", "red"), cex=0.6)
lines(xts(soil.min.allavg/soil.min.allavg[1]*100,as.POSIXct(ttpredict.time)),pch=19,col="gold")
