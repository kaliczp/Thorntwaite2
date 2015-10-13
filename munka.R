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

remoproj = read.csv2("remo.csv", stringsAsFactors= FALSE) 
remoproj.xts <- xts(remoproj[-1] , order.by = as.Date(remoproj$Index, format="%Y.%m.%d"))
