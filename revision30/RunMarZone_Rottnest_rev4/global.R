# rottnest4

library(shiny)
library(PBSmapping)
library(sp)
library(maptools)
require(foreign)
require(vegan)
require(labdsv)

setwd("./p")
Gfold <- sprintf("%s",round(runif(1)*1000000))

sPath <- "/mnt/shiny/"

for (ii in 1:100000){
  if(!file.exists(sprintf("%s%s",sPath,Gfold))) {
    system(paste("mkdir ",sprintf("%s%s",sPath,Gfold)))
    break()
  }
}
system(paste("cp -r files/* " , sprintf("%s%s",sPath,Gfold)))
cat(paste0(sPath,Gfold,"\n"))
setwd(sprintf("%s%s",sPath,Gfold))

sUserID <<- Gfold

sMarxanDir <- getwd()

# get NUMREPS from input.dat
inputdat <- readLines(paste(sMarxanDir,"/input.dat",sep=""))
iParam <- which(regexpr("NUMREPS",inputdat)==1)
iNUMREPS <<- as.integer(unlist(strsplit(inputdat[iParam], split=" "))[2])

iNumberOfZones <<- 3
ZoneColours <<- c("white","green","blue")
ZoneNames <<- c("Available","Partial","Reserved")

irefreshinput <<- 0
swhichzbc <<- "zones 1<->2"
isavetargetspf <<- 0


