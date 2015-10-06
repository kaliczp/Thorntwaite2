package.skeleton(name="et.proj", list= c("day.in.month.calc", "daylength.calc",  "df.segmentit.gen",  "et.calc", "et.test", "PETH.gen", "predict.PETH"))

## R CMD build et.proj
## R CMD check et.proj_1.0.tar.gz
### Make zip for windows
## R CMD INSTALL -l localRlib et.proj_1.0.tar.gz
## cd localRlib
## zip -r et.proj_1.0.zip et.proj
