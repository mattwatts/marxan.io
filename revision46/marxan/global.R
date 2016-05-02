# marxan.io Run Marxan

cat(paste0("hello\n"))
cat(paste0(getwd(),"\n"))

sAppDir <<- getwd()

sShinyPath <<- "/mnt/shiny/"
sShinyUserPath <<- paste0(sShinyPath,"users/")
sShinyDataPath <- paste0(sShinyPath,"data/")
sShinyTempPath <<- paste0(sShinyPath,"temp/")

iRevision <- 46
sUserGuide <<- "Marxan_io_rev39_user_guide.pdf"

sMarxanApp <<- paste0("revision",iRevision,"/marxan")

if (.Platform$pkgType == "source")
{
  sShinySourcePath <<- paste0(sShinyPath,"apps/revision",iRevision,"/source/")
} else {
  sShinySourcePath <<- paste0("/Users/matt/Documents/R/revision",iRevision,"/source/")
}

sDatabase <<- ""
sMarxanDir <<- ""
irefreshinput <<- 0
irefreshmrun <<- 0
isavetargetspf <<- 0
sSampleMarxanDataset <<- "Tasmania"
sSelectDb <<- sSampleMarxanDataset
sBLM <<- 0.1
sProp <<- 0.3
sSPF <<- 1
iSpecDatRows <<- 0
iCores <<- 10
iRepsPerCore <<- 10

irefreshmap <<- 0
irefreshcluster <<- 0
irefreshtable <<- 0

sdisplaywhat <<- "map"
iAspectX <<- 1
iAspectY <<- 1
fZoomToLimits <<- TRUE
fLeafletRdata <<- FALSE
fEnableLeaflet <<- FALSE
fEnableMap <<- FALSE

