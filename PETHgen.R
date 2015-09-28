PETH.gen <- function(Temp, lat = 47.5){
    require(xts)
    ## Temp time series in deg C
    ## Next equation from Hamon_PET_equation.pdf
    SatVaporPress <- 0.6108*exp((17.27*Temp)/(Temp+273.3)) # [kPa]
    Day.length.hour <- daylength.calc(Latitude = lat)
    ## Temp begin in January!
    29.8*Day.length.hour*(SatVaporPress/(Temp+273.2)) # [mm/nap]
}
