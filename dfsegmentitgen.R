df.segmentit.gen <- function(CREMAP, PETH, Prec, as.xts = FALSE){
    require(segmented)
    ## Remove xts and data.frame properties
    if(!as.xts){
        PETH <- as.vector(coredata(PETH))
        CREMAP <- as.vector(coredata(CREMAP))
    }
    Large.CREMAP <- CREMAP > PETH
    Large.Prec <- Prec > PETH
    Available.CREMAP <- !is.na(CREMAP)
    NonLimitedET.idx <- (Large.CREMAP | Large.Prec) & Available.CREMAP
    PETH.nolimit <- PETH[NonLimitedET.idx]
    CREMAP.nolimit <- CREMAP[NonLimitedET.idx]
    data.frame(PETH = PETH.nolimit, CREMAP=CREMAP.nolimit)
}
