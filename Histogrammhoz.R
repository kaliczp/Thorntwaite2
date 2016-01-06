library(xts)

## ET histogram: mean

et_yearly_dm = apply.yearly (et.pred.dm$ET_M, mean)
et_yearly_knmi = apply.yearly (et.pred.knmi$ET_M, mean)
et_yearly_smhirca = apply.yearly (et.pred.smhirca$ET_M, mean)
et_yearly_remo = apply.yearly (et.pred.remo$ET_M, mean)

plot(et_yearly_smhirca)
lines(et_yearly_dm,col=2)

et_yearly_dm.10 = apply.yearly (et.pred.dm['2010/2040'], mean)$ET_M
et_yearly_dm.40 = apply.yearly (et.pred.dm['2040/2070'], mean)$ET_M
et_yearly_dm.70 = apply.yearly (et.pred.dm['2070/2100'], mean)$ET_M

plot(density(et_yearly_dm.40),lwd=2,col=2, main="", xlab="ET_M [mm]")
lines(density(et_yearly_dm.70),lwd=2,col=3)
lines(density(et_yearly_dm.10),lwd=2)
legend("topleft",c("2010-2040","2040-2070","2070-2100"),lwd=2,col=1:3)

#######################################################################
#######################################################################
#######################################################################

## ET histogram: max

et_yearly_dm_max = apply.yearly (et.pred.dm$ET_M, max)
et_yearly_knmi_max = apply.yearly (et.pred.knmi$ET_M, max)
et_yearly_smhirca_max = apply.yearly (et.pred.smhirca$ET_M, max)
et_yearly_remo_max = apply.yearly (et.pred.remo$ET_M, max)

plot(et_yearly_smhirca_max)
lines(et_yearly_dm_max,col=2)

et_yearly_dm_max.10 = apply.yearly (et.pred.dm$ET_M['2010/2040'], max)
et_yearly_dm_max.40 = apply.yearly (et.pred.dm$ET_M['2040/2070'], max)
et_yearly_dm_max.70 = apply.yearly (et.pred.dm$ET_M['2070/2100'], max)

plot(density(et_yearly_dm_max.40),lwd=2,col=2, main="", xlab="ET_M_max [mm]")
lines(density(et_yearly_dm_max.70),lwd=2,col=3)
lines(density(et_yearly_dm_max.10),lwd=2)
legend("topleft",c("2010-2040","2040-2070","2070-2100"),lwd=2,col=1:3)


#######################################################################


plot.density.timewindow <- function(x, time.window=c('2010/2040','2040/2070','2070/2100'), szorzo=1.04, legend.place="left", ...) {
  if(!is.xts(x)) {
    stop("Argument is not an xts!")
  }
  x.nona=na.omit(x)
  x.xlim = c(min(x),max(x))
  x.max <- max(density(x.nona)$y)*szorzo
  plot(density(x[time.window[1]]),lwd=2, ylim=c(0,x.max), col=3, ... )
  lines(density(x[time.window[2]]),lwd=2,col=2)
  lines(density(x[time.window[3]]),lwd=2)
  legend(paste0("top",legend.place),time.window,lwd=2,col=3:1)
}

plot.density.timewindow(apply.yearly (et.pred.remo$ET_M, max),main="",xlab="")

plot.density.timewindow(apply.yearly (et.pred.remo$SOIL_M, mean), szorzo=1.3, legend.place="right", main="", xlab="SOIL_M átlag [mm]", ylab="Rel. gyak.")
plot.density.timewindow(apply.yearly (et.pred.remo$ET_M, mean), szorzo=1.2, main="", ylab="Rel. gyak.", xlab="ET átlag [mm/hó]")


plot.density.timewindow(apply.yearly (et.pred.remo$SOIL_M, min), szorzo=1.2, legend.place="right", main="", xlab="SOIL_M min [mm]", ylab="Rel. gyak.")
plot.density.timewindow(apply.yearly (et.pred.remo$ET_M, max), szorzo=1.2, main="", ylab="Rel. gyak.", xlab="ET max [mm/hó]")


pdf(width=12,height=3.5)
par(mar=c(2.1,4.1,0.6,1.1))

plot.zoo(as.zoo(et.pred.remo['2010/2100',1]),xaxs = "i",ylab="ET_M [mm/hó]",main="",xaxt="n",xlab="", type="n")
grid(nx=NA,ny=NULL, lwd=2, col="grey")
lines(as.zoo(et.pred.remo['2010/2100',1]),lwd=2)
## MIÉRT csúszik el?
axis.POSIXct(1,at=seq(as.POSIXct("2010-01-31"),as.POSIXct("2100-12-31"),by="10 years"))

plot.zoo(as.zoo(et.pred.remo['2010/2100',2]),xaxs = "i",ylab="SOIL_M [mm]",main="",xaxt="n",xlab="")
grid(nx=NA,ny=NULL, lwd=2, col="grey")
lines(as.zoo(et.pred.remo['2010/2100',2]), lwd=2)
axis.POSIXct(1,at=seq(as.POSIXct("2010-01-31"),as.POSIXct("2100-12-31"),by="10 years"))

## Színes időszakok
plot.zoo(as.zoo(et.pred.remo['2010/2100',1]),xaxs = "i",ylab="ET_M [mm/hó]",main="",xaxt="n",xlab="",type="n")
grid(nx=NA,ny=NULL, lwd=2, col="grey")
lines(as.zoo(et.pred.remo['2010/2040',1]),col=3,lwd=2)
lines(as.zoo(et.pred.remo['2040/2070',1]),col=2,lwd=2)
lines(as.zoo(et.pred.remo['2070/2100',1]),col=1,lwd=2)
axis.POSIXct(1,at=seq(as.POSIXct("2010-01-31"),as.POSIXct("2100-12-31"),by="10 years"))
legend("topleft",c('2010/2040','2040/2070','2070/2100'),ncol=3,lwd=2,col=3:1)

plot.zoo(as.zoo(et.pred.remo['2010/2100',2]),xaxs = "i",ylab="SOIL_M [mm]",main="",xaxt="n",xlab="", type="n")
grid(nx=NA,ny=NULL, lwd=2, col="grey")
lines(as.zoo(et.pred.remo['2010/2040',2]), lwd=2, col=3)
lines(as.zoo(et.pred.remo['2040/2070',2]), lwd=2, col=2)
lines(as.zoo(et.pred.remo['2070/2100',2]), lwd=2, col=1)
axis.POSIXct(1,at=seq(as.POSIXct("2010-01-31"),as.POSIXct("2100-12-31"),by="10 years"))
legend("bottomleft",c('2010/2040','2040/2070','2070/2100'),ncol=3,lwd=2,col=3:1)
