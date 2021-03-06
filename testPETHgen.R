## Generate the test data base
test.meteo <- matrix( c("1999-01-15",NA,11.87679973,-0.008273162,
                        "1999-02-15",NA,34.33935241,1.068492578,
                        "1999-03-15",NA,27.95640055,7.868492578,
                        "1999-04-15",NA,85.29000916,12.37679906,
                        "1999-05-15",NA,76.46199857,16.06679973,
                        "1999-06-15",NA,85.43238359,18.56679973,
                        "1999-07-15",NA,119.4625299,21.46679973,
                        "1999-08-15",NA,94.34917415,19.76849258,
                        "1999-09-15",NA,58.84877797,18.57679906,
                        "1999-10-15",NA,20.93917482,11.56849258,
                        "1999-11-15",11.06798644,67.80744731,3.568492578,
                        "1999-12-15",NA,58.79819569,0.451872615,
                        "2000-01-15",NA,28.61679973,-1.40992253,
                        "2000-02-15",NA,14.64320027,4.768492578,
                        "2000-03-15",23.9450339,78.43377105,6.968492578,
                        "2000-04-15",71.65518968,39.7844253,14.27679906,
                        "2000-05-15",124.7747025,31.66450244,17.65852656,
                        "2000-06-15",150.3342595,31.93992819,20.66849258,
                        "2000-07-15",95.32797794,100.5517268,19.66679973,
                        "2000-08-15",101.9414788,67.17704813,22.25852656,
                        "2000-09-15",47.96037019,52.91934575,16.26849258,
                        "2000-10-15",27.93670393,87.9430355,13.56679973,
                        "2000-11-15",12.14989404,57.09000916,8.666799727,
                        "2000-12-15",NA,47.79640055,1.868492578
                        ), ncol=4, byrow=TRUE)
test.meteo.df <- as.data.frame(test.meteo, stringsAsFactors= FALSE)
names(test.meteo.df) <- c("Index","ET-CREMAP","P","t")
for(tti in 2:4)
    test.meteo.df[,tti] <- as.numeric(test.meteo.df[,tti])
require(xts)
test.meteo.xts <- xts(test.meteo.df[-1] , order.by = as.Date(test.meteo.df$Index))

PETH.xts <- PETH.gen(test.meteo.xts$t)
