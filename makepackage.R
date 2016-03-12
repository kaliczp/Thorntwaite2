package.skeleton(name="et.proj", list= c("day.in.month.calc", "daylength.calc",  "df.segmentit.gen",  "et.calc", "et.test", "PETH.gen", "predict.PETH"))

## R CMD build et.proj
## R CMD check et.proj_1.2.tar.gz
### Make zip for windows
## R CMD INSTALL -l localRlib et.proj_1.2.tar.gz
## cd localRlib
## zip -r et.proj_1.2.zip et.proj

## Installation
 install.packages("path_to_file", repos = NULL, type="source")
## Substitute path_to_file_in Windows "C:\\et.proj_1.2.tar.gz"
