predict.PETH <- function(seg.obj=seg.fit, PETH=PETH.xts){
  require(xts)
  require(segmented)
  PETH.df <- as.data.frame(coredata(PETH))
  names(PETH.df) <- "PETH"
  ## Esetleg az idősorra számolni.
  real <- predict(seg.obj, newdata = PETH.df)
  xts(real,index(PETH))
}
