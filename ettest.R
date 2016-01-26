et.test <- function(soil.max, temp, prec, pet.real, cremap) {
    
    et.pred.prelim <- et.calc(SOIL_MAX = soil.max, Temp = temp, Prec= prec, PET.real=pet.real)
    valid.difference <- na.omit(cremap - et.pred.prelim$ET_M)
    val.diff.sum <- sum(valid.difference^2)
    val.diff.sum +  val.diff.sum/1000 * soil.max^2/1000
}
