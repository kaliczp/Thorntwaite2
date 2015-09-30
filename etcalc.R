et.calc <- function(Temp, Prec, PET.real, SOIL_MAX = 209.7) {
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
        if(Prec[tti] > PET.real[tti]){
            TET.df$SOIL_M[tti] <- min(
                c(as.vector(Prec[tti] - PET.real[tti] + TET.df$SOIL_M[tti-1]),
                  SOIL_MAX
                  )
            )
            TET.df$ET_M[tti] <- PET.real[tti]
        } else {
            TET.df$SOIL_M[tti] <- TET.df$SOIL_M[tti-1] *
                exp(-(PET.real[tti]-Prec[tti]) / SOIL_MAX)
            TET.df$ET_M[tti] <- Prec[tti] +
                TET.df$SOIL_M[tti-1] -
                    TET.df$SOIL_M[tti]
        }
    }
    xts(TET.df, index(Temp))
}
