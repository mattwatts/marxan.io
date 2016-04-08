# marxan.io Login

cat(paste0("hello\n"))
cat(paste0(getwd(),"\n"))

sAppDir <<- getwd()

sShinyPath <<- "/mnt/shiny/"
sShinyUserPath <<- paste0(sShinyPath,"users/")
sShinyDataPath <- paste0(sShinyPath,"data/")
sShinyTempPath <<- paste0(sShinyPath,"temp/")

iRevision <- 37
sUserGuide <<- "Marxan_io_rev34_user_guide.pdf"

sAllApps <<- paste0("revision",iRevision)
sUploadApp <<- paste0("revision",iRevision,"/upload")
sMarxanApp <<- paste0("revision",iRevision,"/marxan")
sMarxanParamTestApp <<- paste0("revision",iRevision,"/mxptest")
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
irefreshinput <<- 0
irefreshmrun <<- 0
isavetargetspf <<- 0
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
irefreshptinput <<- 0

iLogin <<- 0
iLogout <<- 0
iLoginClick <<- 0
iWrongPassword <<- 0

