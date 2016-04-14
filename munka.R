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

## dir(patt="00.csv")

##rcm.readin <- function(file.past, file.future) {
##    past <- read.csv2(file.past, stringsAsFactors= FALSE)
##    future <- read.csv2(file.future, stringsAsFactors= FALSE)
##    raw <- rbind(past,future)
##    date.full <- as.Date(raw$Date, format="%Y-%m-%d")
##    xts(raw[-1] , order.by = date.full )
##}

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

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  
############################# 2016-04-08- tól, FORESEE-s BIAS CORRECTED-es adatokhoz.
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

remo.echam_RAW = read.csv2("remo.echam.csv", stringsAsFactors= FALSE) 
dmi.echam_RAW = read.csv2("dmi.echam.csv", stringsAsFactors= FALSE)
knmi.racmo2_RAW = read.csv2("knmi.racmo2.csv", stringsAsFactors= FALSE)
smhirca.bcm_RAW = read.csv2("smhirca.bcm.csv", stringsAsFactors= FALSE)

present.foresee_RAW = read.csv2("present.foresee.csv", stringsAsFactors= FALSE) 

remo.echam=xts(remo.echam_RAW[c("p","t")],as.POSIXct(as.character(remo.echam_RAW$Index)))
dmi.echam=xts(dmi.echam_RAW[c("p","t")],as.POSIXct(as.character(dmi.echam_RAW$Index)))
knmi.racmo2=xts(knmi.racmo2_RAW[c("p","t")],as.POSIXct(as.character(knmi.racmo2_RAW$Index)))
smhirca.bcm=xts(smhirca.bcm_RAW[c("p","t")],as.POSIXct(as.character(smhirca.bcm_RAW$Index)))

present.foresee = xts(present.foresee_RAW[c("p","t")],as.POSIXct(as.character(present.foresee_RAW$Index)))

###################################### RCM-ek átlagai, szórás ###########################
##########################################################################################

remo.echam.T.avg = c(mean(remo.echam$t['2015/2044']), mean(remo.echam$t['2045/2074']), mean(remo.echam$t['2070/2099']))
knmi.racmo2.T.avg = c(mean(knmi.racmo2$t['2015/2044']), mean(knmi.racmo2$t['2045/2074']), mean(knmi.racmo2$t['2070/2099']))
smhirca.bcm.T.avg = c(mean(smhirca.bcm$t['2015/2044']), mean(smhirca.bcm$t['2045/2074']), mean(smhirca.bcm$t['2070/2099']))
dmi.echam.T.avg = c(mean(dmi.echam$t['2015/2044']), mean(dmi.echam$t['2045/2074']), mean(dmi.echam$t['2070/2099']))

present.foresee.T.avg = mean(present.foresee$t['1985/2014'])

dmi.echam.P.Yearly = apply.yearly(dmi.echam$p,sum)
remo.echam.P.Yearly = apply.yearly(remo.echam$p,sum)
knmi.racmo2.P.Yearly = apply.yearly(knmi.racmo2$p,sum)
smhirca.bcm.P.Yearly = apply.yearly(smhirca.bcm$p,sum)

present.foresee.P.Yearly = apply.yearly(present.foresee$p, sum)

remo.echam.P.avg = c(mean(remo.echam.P.Yearly$p['2015/2044']), mean(remo.echam.P.Yearly$p['2045/2074']), mean(remo.echam.P.Yearly$p['2070/2099']))
knmi.racmo2.P.avg = c(mean(knmi.racmo2.P.Yearly$p['2015/2044']), mean(knmi.racmo2.P.Yearly$p['2045/2074']), mean(knmi.racmo2.P.Yearly$p['2070/2099']))
smhirca.bcm.P.avg = c(mean(smhirca.bcm.P.Yearly$p['2015/2044']), mean(smhirca.bcm.P.Yearly$p['2045/2074']), mean(smhirca.bcm.P.Yearly$p['2070/2099']))
dmi.echam.P.avg = c(mean(dmi.echam.P.Yearly$p['2015/2044']), mean(dmi.echam.P.Yearly$p['2045/2074']), mean(dmi.echam.P.Yearly$p['2070/2099']))


present.foresee.P.avg = mean(present.foresee.P.Yearly)
####################################################################################
######## Havi átlagok képzése

## monthly.T_dmi.arpege <- apply.monthly(dmi.arpege$T,mean)
## monthly.P_dmi.arpege <- apply.monthly(dmi.arpege$P,sum)

monthly.T_dmi.echam <- apply.monthly(dmi.echam$t,mean)
monthly.P_dmi.echam <- apply.monthly(dmi.echam$p,sum)

monthly.T_knmi.racmo2 <- apply.monthly(knmi.racmo2$t,mean)
monthly.P_knmi.racmo2 <- apply.monthly(knmi.racmo2$p,sum)

monthly.T_remo.echam <- apply.monthly(remo.echam$t,mean)
monthly.P_remo.echam <- apply.monthly(remo.echam$p,sum)

monthly.T_smhirca.bcm <- apply.monthly(smhirca.bcm$t,mean)
monthly.P_smhirca.bcm <- apply.monthly(smhirca.bcm$p,sum)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

monthly.T_present = apply.monthly(present.foresee$t,mean)
monthly.P_present = apply.monthly(present.foresee$p,sum)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
######################## PETH future; PET.proj.knmi: becsül AET-t! Future.knmi: AET-ből vízmérleg. 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

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

#######################################################################################

PETHPresent.xts <- PETH.gen(monthly.T_present)
PET.proj.present <- predict.PETH(seg.result, PETHPresent.xts)
Future.Present <- et.calc(SOIL_MAX=SOIL.MAX,Temp = monthly.T_present$t, Prec=monthly.P_present$p, PET.real=PET.proj.present)

####################################################################################################

dat.win <- c('2015/2044','2045/2074','2070/2099')
dat.win.plt <- c('1985/2015', '2015/2045','2045/2075','2070/2100')

et.sum.dm <- numeric(4)
et.sum.dm[1] <- mean(Future.Present$ET_M['1984/2014'])
for(tti in 1:length(dat.win)) {et.sum.dm[tti+1] <- mean(Future.dm$ET_M[dat.win[tti]],na.rm=T)}
ttpredict.time <- c('2000-06-15','2030-06-15','2060-06-15','2085-06-15')
plot(xts(et.sum.dm,as.POSIXct(ttpredict.time)),type="p",pch=18,main="", xaxt="n", ylab="ET_M [mm/month]")
axis(1,at=as.POSIXct(c('2000-06-15','2030-06-15','2060-06-15','2085-06-15')), lab=dat.win.plt)

soil.sum.dm <- numeric(4)
soil.sum.dm[1] <- mean(Future.Present$SOIL_M['1984/2014'])
for(tti in 1:length(dat.win)) soil.sum.dm[tti+1] <-mean(Future.dm$SOIL_M[dat.win[tti]],na.rm=T)
plot(xts(soil.sum.dm,as.Date(ttpredict.time)),type="p",pch=18,main="", xaxt="n", ylab="SOIL_M [mm/month]")
axis(1,at=as.POSIXct(c('2000-06-05','2030-06-15','2060-06-15','2085-06-15')), lab=dat.win.plt)

soil.min.dm <- numeric(4)
soil.min.dm[1] <- min(Future.Present$SOIL_M['1984/2014'])
for(tti in 1:length(dat.win)) soil.min.dm[tti+1] <-min(Future.dm$SOIL_M[dat.win[tti]],na.rm=T)
plot(xts(soil.min.dm,as.Date(ttpredict.time)),type="p",pch=18,main="", xaxt="n", ylab="Min. of SOIL_M [mm/month]")
axis(1,at=as.POSIXct(c('2000-06-15','2030-06-15','2060-06-15','2085-06-15')), lab=dat.win.plt)

#####################################################################################################

et.sum.sm <- numeric(4)
et.sum.sm[1] <- mean(Future.Present$ET_M['1984/2014'])
for(tti in 1:length(dat.win)) et.sum.sm[tti+1] <- mean(Future.sm$ET_M[dat.win[tti]],na.rm=T)
ttpredict.time <- c('2000-06-15','2030-06-15','2060-06-15','2085-06-15')
plot(xts(et.sum.sm,as.POSIXct(ttpredict.time)),type="p",pch=18,main="SMHIRCA models evapotranspiration prediction", xaxt="n", ylab="ET_M [mm/month]")
axis(1,at=as.POSIXct(c('2000-06-15','2030-06-15','2060-06-15','2085-06-15')), lab=dat.win.plt)

soil.sum.sm <- numeric(4)
soil.sum.sm[1] <- mean(Future.Present$SOIL_M['1984/2014'])
for(tti in 1:length(dat.win)) soil.sum.sm[tti+1] <- mean(Future.sm$SOIL_M[dat.win[tti]],na.rm=T)
plot(xts(soil.sum.sm,as.Date(ttpredict.time)),type="p",pch=18,main="SMHIRCA models soil moisture prediction", xaxt="n", ylab="SOIL_M [mm/month]")
axis(1,at=as.POSIXct(c('2000-06-15','2030-06-15','2060-06-15','2085-06-15')), lab=dat.win.plt)

soil.min.sm <- numeric(4)
soil.min.sm[1] <- min(Future.Present$SOIL_M['1984/2014'])
for(tti in 1:length(dat.win)) soil.min.sm[tti+1] <-min(Future.sm$SOIL_M[dat.win[tti]],na.rm=T)
plot(xts(soil.min.sm,as.Date(ttpredict.time)),type="p",pch=18,main="", xaxt="n", ylab="Min. of SOIL_M [mm/month]")
axis(1,at=as.POSIXct(c('2000-06-16','2030-06-15','2060-06-15','2085-06-15')), lab=dat.win.plt)

################################################################################################

et.sum.remo <- numeric(4)
et.sum.remo[1] <- mean(Future.Present$ET_M['1984/2014'])
for(tti in 1:length(dat.win)) et.sum.remo[tti+1] <- mean(Future.remo$ET_M[dat.win[tti]],na.rm=T)
ttpredict.time <- c('2000-06-15','2030-06-15','2060-06-15','2085-06-15')
plot(xts(et.sum.remo,as.POSIXct(ttpredict.time)),type="p",pch=18,main="REMO models evapotranspiration prediction", xaxt="n", ylab="ET_M [mm/month]")
axis(1,at=as.POSIXct(c('2000-06-15','2030-06-15','2060-06-15','2085-06-15')), lab=dat.win.plt)

soil.sum.remo <- numeric(4)
soil.sum.remo[1] <- mean(Future.Present$SOIL_M['1984/2014'])
for(tti in 1:length(dat.win)) soil.sum.remo[tti+1] <- mean(Future.remo$SOIL_M[dat.win[tti]],na.rm=T)
plot(xts(soil.sum.remo,as.Date(ttpredict.time)),type="p",pch=18,main="Remo models soil moisture prediction", xaxt="n", ylab="SOIL_M [mm/month]")
axis(1,at=as.POSIXct(c('2000-06-15','2030-06-15','2060-06-15','2085-06-15')), lab=dat.win.plt)

soil.min.remo <- numeric(4)
soil.min.remo[1] <- min(Future.Present$SOIL_M['1984/2014'])
for(tti in 1:length(dat.win)) soil.min.remo[tti+1] <-min(Future.remo$SOIL_M[dat.win[tti]],na.rm=T)
plot(xts(soil.min.remo,as.Date(ttpredict.time)),type="p",pch=18,main="", xaxt="n", ylab="Min. of SOIL_M [mm/month]")
axis(1,at=as.POSIXct(c('2000-06-15','2030-06-15','2060-06-15','2085-06-15')), lab=dat.win.plt)

###################################################################################################

et.sum.knmi <- numeric(4)
et.sum.knmi[1] <- mean(Future.Present$ET_M['1984/2014'])
for(tti in 1:length(dat.win)) et.sum.knmi[tti+1] <- mean(Future.knmi$ET_M[dat.win[tti]],na.rm=T)
ttpredict.time <- c('2000-06-05','2030-06-15','2060-06-15','2085-06-15')
plot(xts(et.sum.knmi,as.POSIXct(ttpredict.time)),type="p",pch=18,main="KNMIRACMO2 models evapotranspiration prediction", xaxt="n", ylab="ET_M [mm/month]")
axis(1,at=as.POSIXct(c('2000-06-15','2030-06-15','2060-06-15','2085-06-15')), lab=dat.win.plt)

soil.sum.knmi <- numeric(4)
soil.sum.knmi[1] <- mean(Future.Present$SOIL_M['1984/2014'])
for(tti in 1:length(dat.win)) soil.sum.knmi[tti+1] <- mean(Future.knmi$SOIL_M[dat.win[tti]],na.rm=T)
plot(xts(soil.sum.knmi,as.Date(ttpredict.time)),type="p",pch=18, main="KNMIRACMO2 models soil moisture prediction", xaxt="n", ylab="SOIL_M [mm/month]")
axis(1,at=as.POSIXct(c('2000-06-15','2030-06-15','2060-06-15','2085-06-15')), lab=dat.win.plt)

soil.min.knmi <- numeric(4)
soil.min.knmi[1] <- min(Future.Present$SOIL_M['1984/2014'])
for(tti in 1:length(dat.win)) soil.min.knmi[tti+1] <-min(Future.knmi$SOIL_M[dat.win[tti]],na.rm=T)
plot(xts(soil.min.knmi,as.Date(ttpredict.time)),type="p",pch=18,main="", xaxt="n", ylab="Min. of SOIL_M [mm/month]")
axis(1,at=as.POSIXct(c('2000-06-15','2030-06-15','2060-06-15','2085-06-15')), lab=dat.win.plt)

###################################################################################################
###################################################################################################

### Közös ábra ET

plot(xts(et.sum.dm,as.POSIXct(ttpredict.time)),type="p",pch=15, cex=1.4 ,main="", xaxt="n", ylab="ET_M",ylim=c(40,60))
axis(1,at=as.POSIXct(c('2000-06-15','2030-06-15','2060-06-15','2085-06-15')), lab=dat.win.plt)
points(xts(et.sum.dm,as.POSIXct(ttpredict.time)),pch=15, cex=1.4)
points(xts(et.sum.sm,as.POSIXct(ttpredict.time)),pch=17, cex=1.4, col="darkgreen")
points(xts(et.sum.remo,as.POSIXct(ttpredict.time)),pch=16, cex=1.4, col="darkblue")
points(xts(et.sum.knmi,as.POSIXct(ttpredict.time)),pch=8, cex=1.4, col="red")
lines(xts(et.sum.allavg,as.POSIXct(ttpredict.time)),pch=19,col="gold")

legend("topleft",c("remo","smhirca","dm", "knmiracmo2"),pch=c(16,17,15,8),col=c("darkblue","darkgreen","black", "red"), cex=0.8)

###################################################################################################
####################################################################################################

#### SOIL közös ábára

## Közös ábra
plot(xts(soil.sum.dm,as.POSIXct(ttpredict.time)),type="p",pch=15, cex=1.4, main="", xaxt="n", ylab="SOIL_M [%]",ylim=c(45,100))
axis(1,at=as.POSIXct(c('2000-06-15','2030-06-15','2060-06-15','2085-06-15')), lab=dat.win.plt)
points(xts(soil.sum.dm,as.POSIXct(ttpredict.time)),pch=15, cex=1.4)
points(xts(soil.sum.sm,as.POSIXct(ttpredict.time)),pch=17, cex=1.4, col="darkgreen")
points(xts(soil.sum.remo,as.POSIXct(ttpredict.time)),pch=16, cex=1.4, col="darkblue")
points(xts(soil.sum.knmi,as.POSIXct(ttpredict.time)),pch=8, cex=1.4, col="red")
legend("topleft",c("remo","smhirca","dm", "knmiracmo2"),pch=c(16,17,15,8),col=c("darkblue","darkgreen","black", "red"), cex=0.7)
lines(xts(soil.sum.allavg,as.POSIXct(ttpredict.time)),pch=19,col="gold")

####################################################################################################
####################################################################################################

#### SOIL_M !MIN! közös ábra

## Közös ábra
plot(xts(soil.min.dm,as.POSIXct(ttpredict.time)),type="p",pch=15, cex=1.4, main="", xaxt="n", ylab="SOIL_M_MIN [%]",ylim=c(0,6))
axis(1,at=as.POSIXct(c('2000-06-05','2030-06-15','2060-06-15','2085-06-15')), lab=dat.win.plt)
points(xts(soil.min.dm,as.POSIXct(ttpredict.time)),pch=15, cex=1.4)
points(xts(soil.min.sm,as.POSIXct(ttpredict.time)),pch=17, cex=1.4, col="darkgreen")
points(xts(soil.min.remo,as.POSIXct(ttpredict.time)),pch=16, cex=1.4, col="darkblue")
points(xts(soil.min.knmi,as.POSIXct(ttpredict.time)),pch=8, cex=1.4, col="red")

legend("topleft",c("remo","smhirca","dm", "knmiracmo2"),pch=c(16,17,15,8),col=c("darkblue","darkgreen","black", "red"), cex=0.6)
lines(xts(soil.min.allavg,as.POSIXct(ttpredict.time)),pch=19,col="gold")

######################################################################################
######################################################################################
###### SOIL_M MIN 10% PERCENTILE!###############################
##################################################################

qtl.dm.2070 = quantile(Future.dm$SOIL_M['2070/2099'], c(.10))
qtl.dm.2045 = quantile(Future.dm$SOIL_M['2045/2074'], c(.10))
qtl.dm.2015 = quantile(Future.dm$SOIL_M['2015/2044'], c(.10))

qtl.sm.2015 = quantile(Future.sm$SOIL_M['2015/2044'], c(.10))
qtl.sm.2045 = quantile(Future.sm$SOIL_M['2045/2074'], c(.10))
qtl.sm.2070 = quantile(Future.sm$SOIL_M['2070/2099'], c(.10))

qtl.remo.2015 = quantile(Future.remo$SOIL_M['2015/2044'], c(.10))
qtl.remo.2045 = quantile(Future.remo$SOIL_M['2045/2074'], c(.10))
qtl.remo.2070 = quantile(Future.remo$SOIL_M['2070/2099'], c(.10))

qtl.knmi.2015 = quantile(Future.knmi$SOIL_M['2015/2044'], c(.10))
qtl.knmi.2045 = quantile(Future.knmi$SOIL_M['2045/2074'], c(.10))
qtl.knmi.2070 = quantile(Future.knmi$SOIL_M['2070/2099'], c(.10))
#------------------------------------------------------------------
qtl.present.1985 = quantile(Future.Present$SOIL_M['1985/2014'], c(.10))
#-------------------------------------------------------------------

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

dm.future.2070 = Future.dm$SOIL_M['2070/2099']
dm.future.2045 = Future.dm$SOIL_M['2045/2074']
dm.future.2015 = Future.dm$SOIL_M['2015/2044']
# ----------------------------------------------
sm.future.2070 = Future.sm$SOIL_M['2070/2099']
sm.future.2045 = Future.sm$SOIL_M['2045/2074']
sm.future.2015 = Future.sm$SOIL_M['2015/2044']
# -----------------------------------------------
knmi.future.2070 = Future.knmi$SOIL_M['2070/2099']
knmi.future.2045 = Future.knmi$SOIL_M['2045/2074']
knmi.future.2015 = Future.knmi$SOIL_M['2015/2044']
# ----------------------------------------------
remo.future.2070 = Future.remo$SOIL_M['2070/2099']
remo.future.2045 = Future.remo$SOIL_M['2045/2074']
remo.future.2015 = Future.remo$SOIL_M['2015/2044']
#-------------------------------------------------------
present.1985 = Future.Present$SOIL_M['1985/2014']
#-------------------------------------------------------

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # #

dm.2070.10Percentile = dm.future.2070[dm.future.2070 < qtl.dm.2070]
dm.2045.10Percentile = dm.future.2045[dm.future.2045 < qtl.dm.2045]
dm.2015.10Percentile = dm.future.2015[dm.future.2015 < qtl.dm.2015]

dm.1980to2100.10Percentile = c(mean(present.1985.10Percentile), mean(dm.2015.10Percentile$SOIL_M, na.rm=TRUE),mean(dm.2045.10Percentile$SOIL_M, na.rm=TRUE), mean(dm.2070.10Percentile$SOIL_M, na.rm=TRUE))

sm.2070.10Percentile = sm.future.2070[sm.future.2070 < qtl.sm.2070]
sm.2045.10Percentile = sm.future.2045[sm.future.2045 < qtl.sm.2045]
sm.2015.10Percentile = sm.future.2015[sm.future.2015 < qtl.sm.2015]

sm.1980to2100.10Percentile = c(mean(present.1985.10Percentile), mean(sm.2015.10Percentile$SOIL_M, na.rm=TRUE),mean(sm.2045.10Percentile$SOIL_M, na.rm=TRUE), mean(sm.2070.10Percentile$SOIL_M, na.rm=TRUE))

knmi.2070.10Percentile = knmi.future.2070[knmi.future.2070 < qtl.knmi.2070]
knmi.2045.10Percentile = knmi.future.2045[knmi.future.2045 < qtl.knmi.2045]
knmi.2015.10Percentile = knmi.future.2015[knmi.future.2015 < qtl.knmi.2015]

knmi.1980to2100.10Percentile = c(mean(present.1985.10Percentile), mean(knmi.2015.10Percentile$SOIL_M, na.rm=TRUE),mean(knmi.2045.10Percentile$SOIL_M, na.rm=TRUE), mean(knmi.2070.10Percentile$SOIL_M, na.rm=TRUE))

remo.2070.10Percentile = remo.future.2070[remo.future.2070 < qtl.remo.2070]
remo.2045.10Percentile = remo.future.2045[remo.future.2045 < qtl.remo.2045]
remo.2015.10Percentile = remo.future.2015[remo.future.2015 < qtl.remo.2015]

remo.1980to2100.10Percentile = c(mean(present.1985.10Percentile), mean(remo.2015.10Percentile$SOIL_M, na.rm=TRUE),mean(remo.2045.10Percentile$SOIL_M, na.rm=TRUE), mean(remo.2070.10Percentile$SOIL_M, na.rm=TRUE))

#--------------------------------------------------------------------------
present.1985.10Percentile = present.1985[present.1985 < qtl.present.1985]
#--------------------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # #

## Közös ábra
plot(xts(remo.1980to2100.10Percentile,as.POSIXct(ttpredict.time)),type="p",pch=15, cex=1.4, main="", xaxt="n", ylab="SOIL_M_MIN [mm/month]",ylim=c(0,12))
axis(1,at=as.POSIXct(c('2000-06-05','2030-06-15','2060-06-15','2085-06-15')), lab=dat.win.plt)
points(xts(dm.1980to2100.10Percentile,as.POSIXct(ttpredict.time)),pch=17, cex=1.4, col="darkgreen")
points(xts(sm.1980to2100.10Percentile,as.POSIXct(ttpredict.time)),pch=16, cex=1.4, col="darkblue")
points(xts(knmi.1980to2100.10Percentile,as.POSIXct(ttpredict.time)),pch=8, cex=1.4, col="red")

legend("bottomleft",c("remo","smhirca","dm", "knmiracmo2"),pch=c(16,17,15,8),col=c("darkblue","darkgreen","black", "red"), cex=0.6)

all.1980to2100.10Percentile = (dm.1980to2100.10Percentile+sm.1980to2100.10Percentile+knmi.1980to2100.10Percentile+remo.1980to2100.10Percentile)/4

lines(xts(all.1980to2100.10Percentile,as.POSIXct(ttpredict.time)),pch=19,col="gold")
