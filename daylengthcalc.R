  daylength.calc <- function(Latitude = 47.5){
      ## Latitude In degree
      ## Declination is a given value for the month, see Dingman p. 601--602
      Declination <- c(-21.3, -13.3, -2.0, 9.8, 18.9, 23.3, 21.3, 13.7, 3.0, -9.0, -18.6, -23.3)
      ## Convert to radian
      Latitude.rad <- (Latitude/360) * 2 * pi
      Declination.rad <- (Declination/360) * 2*pi
      ## Length of the day,
      ## where omega is the angular velocity
      omega <- 0.2618
      ## doubled because it calculates the sunrise and sufall befor and after noon
      2*acos(-tan(Declination.rad)*tan(Latitude.rad))/omega
  }
