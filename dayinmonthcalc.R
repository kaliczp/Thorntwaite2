day.in.month.calc <- function(x) {
    ## The times necessary to endin in 31 days long month
    differences.in.days <- diff(x)
    ## Convert to numeric vector
    differences.in.days <- as.numeric(differences.in.days)
    ## Add the last value the last december
    c(differences.in.days, 31)
}
