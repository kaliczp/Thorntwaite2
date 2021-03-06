et.calc <- function(SOIL_MAX, Temp, Prec, PET.real) {
    ## convert xts data to ordinary vector
    
    ## Make empty data.frame
    TET.df <- data.frame(ET_M = numeric(nrow(Temp)),
                         SOIL_M = numeric(nrow(Temp))
                         )
    ## Set the initial value
    TET.df$SOIL_M[1] <- SOIL_MAX
    ## Set the first value of modelled ET to NA
    TET.df$ET_M[1] <- NA
    for(tti in 2:nrow(Temp)){
        curr.prec <- as.numeric(Prec[tti])
        curr.pet <- as.numeric(PET.real[tti])
        if (curr.prec > curr.pet) {
            TET.df$SOIL_M[tti] <- min(
                c(as.vector(curr.prec - curr.pet + TET.df$SOIL_M[tti-1]),
                  SOIL_MAX
                  )
            )
            TET.df$ET_M[tti] <- curr.pet
        } else {
            TET.df$SOIL_M[tti] <- TET.df$SOIL_M[tti-1] *
                exp(-(curr.pet - curr.prec) / SOIL_MAX)
            TET.df$ET_M[tti] <- curr.prec +
                TET.df$SOIL_M[tti-1] -
                    TET.df$SOIL_M[tti]
        }
    }
    xts(TET.df, index(Temp))
}
