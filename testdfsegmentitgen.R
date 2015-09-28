forsegment.df <- df.segmentit.gen(test.meteo.xts[,1], PETH.xts, test.meteo.xts$P)

## Initial linear regression
lm.fit <- lm(CREMAP ~ PETH - 1 , data=forsegment.df)
## segmented regression based lm.fit
seg.result <- segmented(lm.fit, seg.Z= ~PETH, psi=40)

## A graphical test
plot(forsegment.df,xlim=c(0,max(forsegment.df$PETH)),ylim=c(0,max(forsegment.df$CREMAP)), type="n", xlab="PETH [mm]",ylab="ET CREMAP [mm/month]", xaxs="i", yaxs="i")
points(forsegment.df[forsegment.df$PETH < seg.result$psi[2],], col="red", pch=24, bg="red")
points(forsegment.df[forsegment.df$PETH >= seg.result$psi[2],], col="blue", pch=23, bg="blue")
plot(seg.result,add=T, rug=F, lwd=2)
# slope(seg.result)
curve(slope(seg.result)$PETH[1,1]*x,from=0,to=seg.result$psi[2],add=T, lwd=2)
axis(1,seg.result$psi[2], tck=1, lty="dotted", lab=F)
text(seg.result$psi[2], 0.5, lab=round(seg.result$psi[2],1), srt=90, adj=c(0, -0.3))    

