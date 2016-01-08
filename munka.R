#Step-by-step work.
library(et.proj)
rawmovar = read.csv2("raw.csv", stringsAsFactors= FALSE) 
rawmovar.xts <- xts(rawmovar[-1] , order.by = as.Date(rawmovar$Index))

PETH.xts <- PETH.gen(rawmovar.xts$t)

forsegment.df <- df.segmentit.gen(rawmovar.xts[,1], PETH.xts, rawmovar.xts$P)

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

SOIL.MAX <- optimize(et.test, interval=c(100,10000), temp = rawmovar.xts$t, prec=rawmovar.xts$P, pet.real=PET.proj, cremap=rawmovar.xts[,1])$minimum

Present = et.calc(SOIL_MAX=SOIL.MAX,Temp = rawmovar.xts$t, Prec=rawmovar.xts$P, PET.real=PET.proj)
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

dmproj = read.csv2("DMI_HIRHAM5_A1B_ARPEGE_1951_2000.csv", stringsAsFactors= FALSE) 
dmproj.xts <- xts(dmproj[-1] , order.by = as.Date(dmproj$Date, format="%Y-%m-%d"))
plot(dmproj.xts$T)

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
