# marxan.io Run Marxan

cat(paste0("hello\n"))
cat(paste0(getwd(),"\n"))

sAppDir <<- getwd()

sShinyPath <<- "/mnt/shiny/"
sShinyUserPath <<- paste0(sShinyPath,"users/")
sShinyDataPath <- paste0(sShinyPath,"data/")
sShinyTempPath <<- paste0(sShinyPath,"temp/")
sAppServer <<- "marxan.io"

iRevision <- 44
sUserGuide <<- "Marxan_io_rev39_user_guide.pdf"

sMarZoneApp <<- paste0("revision",iRevision,"/marzone")

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
sSampleMarZoneDataset <<- "RottnestIsland_Scenario4"
sSelectDb <<- sSampleMarZoneDataset
iSpecDatRows <<- 0
iCores <<- 10
iRepsPerCore <<- 10

swhichparam <<- "BLM"
irefreshmap <<- 0
irefreshtable <<- 0
irefreshcluster <<- 0

iZones <<- 2
ZoneNames <<- c("Available","Reserved")
iAspectX <<- 1
iAspectY <<- 1
fDatabaseChange <<- TRUE
fLeafletRdata <<- FALSE


