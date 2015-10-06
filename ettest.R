et.test <- function(soil.max, temp, prec, pet.real, cremap) {
    
    et.pred.prelim <- et.calc(SOIL_MAX = soil.max, Temp = temp, Prec= prec, PET.real=pet.real)
    valid.difference <- na.omit(cremap - et.pred.prelim$ET_M)
    sum(valid.difference^2)
}
