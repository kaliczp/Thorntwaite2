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

## ET histogram: mean

et_yearly_dm_max = apply.yearly (et.pred.dm$ET_M, max)
et_yearly_knmi_max = apply.yearly (et.pred.knmi$ET_M, max)
et_yearly_smhirca_max = apply.yearly (et.pred.smhirca$ET_M, max)
et_yearly_remo_max = apply.yearly (et.pred.remo$ET_M, max)


plot.density.timewindow(et_yearly_dm_max,main="", xlab="ET_M [mm]")
