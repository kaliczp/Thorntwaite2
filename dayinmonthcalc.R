day.in.month.calc <- function(x) {
    ## The times necessary to endin in 31 days long month
    differences.in.days <- diff(x)
    ## Add the last value and impicitly convert to numeric vector
    c(differences.in.days, 31)
}
