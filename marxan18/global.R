# marxan.io

cat(paste0("hello\n"))
cat(paste0(getwd(),"\n"))

sAppDir <<- getwd()

sShinyPath <<- "/mnt/shiny/"
sShinyUserPath <<- paste0(sShinyPath,"users/")
sShinyDataPath <- paste0(sShinyPath,"data/")
sShinyTempPath <<- paste0(sShinyPath,"temp/")

sDatabase <<- ""
sSelectDb <<- "Tas2015"
sMarxanDir <<- ""
irefreshinput <<- 0
irefreshmrun <<- 0
isavetargetspf <<- 0
sSampleDataset <<- "Tas2015"
sBLM <<- 0.1
sProp <<- 0.3
sSPF <<- 1
iSpecDatRows <<- 0
iCores <<- 10
iRepsPerCore <<- 10

swhichparam <<- "BLM"
ruserblm <<- 0
ruserspf <<- 1
rusertarg <<- 0.3
irefreshptinput <<- 0

#itestinput <<- 0
sUploadApp <<- "upload17"

