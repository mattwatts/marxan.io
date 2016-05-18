# marxan.io Run Marxan

cat(paste0("hello\n"))
cat(paste0(getwd(),"\n"))

sAppDir <<- getwd()

sShinyPath <<- "/mnt/shiny/"
sShinyUserPath <<- paste0(sShinyPath,"users/")
sShinyDataPath <- paste0(sShinyPath,"data/")
sShinyTempPath <<- paste0(sShinyPath,"temp/")

iRevision <- 48
sUserGuide <<- "Marxan_io_rev39_user_guide.pdf"

sUploadApp <<- paste0("revision",iRevision,"/upload")
sDownloadApp <<- paste0("revision",iRevision,"/download")
sMarxanApp <<- paste0("revision",iRevision,"/marxan")
sMarZoneApp <<- paste0("revision",iRevision,"/marzone")

if (.Platform$pkgType == "source")
{
  sShinySourcePath <<- paste0(sShinyPath,"apps/revision",iRevision,"/source/")
} else {
  sShinySourcePath <<- paste0("/Users/matt/Documents/R/revision",iRevision,"/source/")
}

sDatabase <<- ""
sSelectDb <<- "Tasmania"
sMarxanDir <<- ""
sSampleMarxanDataset <<- "Tasmania"
sSampleMarZoneDataset <<- "RottnestIsland_Scenario4"
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
irefreshtable <<- 0

iParamTestReps <<- 10

sdisplaywhat <<- "map"
iAspectX <<- 1
iAspectY <<- 1
fWindowsEOLN <<- FALSE
fWindowsEOLNPublic <<- FALSE

