# marxan.io

# input is a zip file provided by user
# unzip to a temporary location
# detect pulayer
#   simplify geometry
#   detect field PUID or PU_ID
#   rename as PUID
#   drop all fields except PUID
#   save as pulayer.shp
# detect puoutline if exists
#   simplify geometry
#   save as puoutline.shp
# scan input.dat
#   detect pu.dat
#   detect spec.dat
#   detect bound.dat
#   detect puvsp.dat
# identify common errors
# convert matrix to sparse
# create puorder.dat and sporder.dat
# create pulayer.Rdata
# run Marxan
# create cluster.Rdata

# return error condition to Marxan web app
# if stop error condition, return useful error string
# return list of warning messages to Marxan web app

library(foreign)
library(rgdal)
library(PBSmapping)
library(sqldf)
library(sp)
library(tools)
library(maptools)
library(foreach)
library(doMC)
require(vegan)
require(labdsv)

PadInt <- function(iRunNumber)
{
  iPad <- 5 - nchar(as.character(iRunNumber))
  return(paste0(paste0(rep("0",iPad),collapse=""),iRunNumber))
}

GetOutputFileext <- function(sMarxanDir,sParam)
{
  inputdat <- readLines(paste(sMarxanDir,"/input.dat",sep=""))
  iParam <- which(regexpr(sParam,inputdat)==1)

  iValue <- as.integer(unlist(strsplit(inputdat[iParam], split=" "))[2])

  if (iValue == 1) { return(".dat") }
  if (iValue == 2) { return(".txt") }
  if (iValue == 3) { return(".csv") }
}

# create a temporary directory in a specified directory
CreateTempDir <- function(sTempPath)
{
  Gfold <- sprintf("%s",round(runif(1)*1000000))
  for (ii in 1:100000)
  {
    sPath <- sprintf("%s/%s",sTempPath,Gfold)
    if(!file.exists(sPath))
    {
      system(paste("mkdir ",sPath))
      break()
    }
  }
  return(sPath)
}

GetParamValue <- function(inputdat,sParam)
{
  iParam <- which(regexpr(sParam,inputdat)==1)
  if (length(iParam) > 0)
  {
    return(sapply(strsplit(inputdat[iParam]," "),"[",2))
  } else {
    return("")
  }
}

smart_read <- function(sInFile)
{
  # are the files CSV, Tab, or comma delimeted?
  cat(paste0("smart_read reading file ",sInFile,"\n"))

  # automatically detect the delimeter type: comma, tab, or space
  sLine <- readLines(sInFile,1)

  if (grepl(",",sLine))
  {
    InTable <- read.csv(sInFile,stringsAsFactors=FALSE)
  }
  if (grepl("\t",sLine))
  {
    InTable <- read.delim(sInFile,stringsAsFactors=FALSE)
  }
  if (grepl(" ",sLine))
  {
    InTable <- read.table(sInFile,stringsAsFactors=FALSE,sep=" ")
  }

  cat(paste0("smart_read file read ",sInFile,"\n"))

  return(InTable)
}

ParseMarxanZip <- function(sInputZipFile,sTempPath,sShinyUserPath,sDataPath,sUserName)
{
  withProgress(message = "Please wait: processing dataset",value=0,
  {

      sPath <- sTempPath

      sLogFile <- paste0(sPath,"/ParseMarxanZip.log")
      WarningMsg <- c()
      ErrorMsg <- c()

      write(paste0("ParseMarxanZip log start ",date()),file=sLogFile)
      write(paste0("temp path ",sPath," ",date()),file=sLogFile,append=TRUE)

      cat(paste0("temp path ",sPath,"\n"))

      withProgress(message="Reading zip",value=0,
      {
          # sPath is the temporary directory
          cat(paste0(">",sInputZipFile,"< >",paste0(sPath,"/data.zip"),"<\n"))
          
          incProgress(0.1,detail="Copying")
          
          file.copy(sInputZipFile,paste0(sPath,"/data.zip"),overwrite=TRUE)
          cat(paste0("copy done\n"))
          
          incProgress(0.5,detail="Unzipping")
          
          system(paste0("unzip ",sInputZipFile," -d ",sPath))
          
          incProgress(1)
          
          cat(paste0("unzip done\n"))
      
      }) # with Progress Reading zip 
  

      withProgress(message="Input files",value=0,
      {
          # find input.dat
          sInputDat <- list.files(path = sPath, recursive = TRUE, pattern = "^input\\.dat$", ignore.case = TRUE, full.names = TRUE)
          if (length(sInputDat) == 0)
          {
            # STOP ERROR
            sErrorMsg <- "ERROR: input.dat not found"
            write(paste0("STOP ",sErrorMsg," ",date()),file=sLogFile,append=TRUE)
            ErrorMsg <- c(ErrorMsg,sErrorMsg)
            cat(paste0(sErrorMsg,"\n"))
            return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
            stop(sErrorMsg)
          }
          if (length(sInputDat) > 1)
          {
            # STOP ERROR
            sErrorMsg <- paste0("ERROR: more than 1 input.dat found: ",paste0(sInputDat,collapse=" "))
            write(paste0("STOP ",sErrorMsg," ",date()),file=sLogFile,append=TRUE)
            ErrorMsg <- c(ErrorMsg,sErrorMsg)
            cat(paste0(sErrorMsg,"\n"))
            return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
            stop(sErrorMsg)
          }

          if (.Platform$pkgType == "source")
          {
            system(paste0("dos2unix ",sInputDat))
          }

          cat(paste0("reading input.dat ",sInputDat,"\n"))

          inputdat <- readLines(sInputDat)

          write(paste0("input.dat read ",sInputDat," ",date()),file=sLogFile,append=TRUE)
          cat(paste0("input.dat read ",sInputDat,"\n"))

          sPUNAME <- GetParamValue(inputdat,"PUNAME")
          sSPECNAME <- GetParamValue(inputdat,"SPECNAME")
          sPUVSPRNAME <- GetParamValue(inputdat,"PUVSPRNAME")
          sBOUNDNAME <- GetParamValue(inputdat,"BOUNDNAME")
  
          sZONESNAME <- GetParamValue(inputdat,"ZONESNAME")
  
          fMarZone <<- FALSE

          if (sZONESNAME != "")
          {
            fMarZone <<- TRUE
    
            # this is a MarZone dataset
            sCOSTSNAME <- GetParamValue(inputdat,"COSTSNAME")
            sZONECOSTNAME <- GetParamValue(inputdat,"ZONECOSTNAME")
            sZONEBOUNDCOSTNAME <- GetParamValue(inputdat,"ZONEBOUNDCOSTNAME")
            sZONECONTRIBNAME <- GetParamValue(inputdat,"ZONECONTRIBNAME")
            sZONECONTRIB2NAME <- GetParamValue(inputdat,"ZONECONTRIB2NAME")
            sZONECONTRIB3NAME <- GetParamValue(inputdat,"ZONECONTRIB3NAME")
            sZONETARGETNAME <- GetParamValue(inputdat,"ZONETARGETNAME")
            sZONETARGET2NAME <- GetParamValue(inputdat,"ZONETARGET2NAME")
            sPULOCKNAME <- GetParamValue(inputdat,"PULOCKNAME")
            sPUZONENAME <- GetParamValue(inputdat,"PUZONENAME")
          }

          if (fMarZone)
          {
            sOutDir <- paste0(sPath,"/marzone")
          } else {
            sOutDir <- paste0(sPath,"/marxan")
          }
  
          # create Marxan directories
          dir.create(sOutDir)
          dir.create(paste0(sOutDir,"/input"))
          dir.create(paste0(sOutDir,"/output"))
          dir.create(paste0(sOutDir,"/pulayer"))

          # parse the input file parameters
          if (sPUNAME == "")
          {
            # STOP ERROR
            sErrorMsg <- "ERROR: PUNAME not found in input.dat"
            write(paste0("STOP ",sErrorMsg," ",date()),file=sLogFile,append=TRUE)
            ErrorMsg <- c(ErrorMsg,sErrorMsg)
            cat(paste0(sErrorMsg,"\n"))
            return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
            stop(sErrorMsg)
          }
          if (sSPECNAME == "")
          {
            # STOP ERROR
            sErrorMsg <- "ERROR: SPECNAME not found in input.dat"
            write(paste0("STOP ",sErrorMsg," ",date()),file=sLogFile,append=TRUE)
            ErrorMsg <- c(ErrorMsg,sErrorMsg)
            cat(paste0(sErrorMsg,"\n"))
            return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
            stop(sErrorMsg)
          }
          if (sPUVSPRNAME == "")
          {
            # STOP ERROR
            sErrorMsg <- "ERROR: PUVSPRNAME not found in input.dat"
            write(paste0("STOP ",sErrorMsg," ",date()),file=sLogFile,append=TRUE)
            ErrorMsg <- c(ErrorMsg,sErrorMsg)
            cat(paste0(sErrorMsg,"\n"))
            return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
            stop(sErrorMsg)
          }
          if (sBOUNDNAME == "")
          {
            # warning message but not critical
            sWarningMsg <- "Warning: BOUNDNAME not found in input.dat"
            write(paste0(sWarningMsg," ",date()),file=sLogFile,append=TRUE)
            WarningMsg <- c(WarningMsg,sWarningMsg)
            return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
            cat(paste0(sWarningMsg,"\n"))
          }
  
          if (fMarZone)
          {
            if (sCOSTSNAME == "")
            {
              # STOP ERROR
              sErrorMsg <- "ERROR: COSTSNAME not found in input.dat"
              write(paste0("STOP ",sErrorMsg," ",date()),file=sLogFile,append=TRUE)
              ErrorMsg <- c(ErrorMsg,sErrorMsg)
              cat(paste0(sErrorMsg,"\n"))
              return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
              stop(sErrorMsg)
            }
  
            if (sZONECOSTNAME == "")
            {
              # STOP ERROR
              sErrorMsg <- "ERROR: ZONECOSTNAME not found in input.dat"
              write(paste0("STOP ",sErrorMsg," ",date()),file=sLogFile,append=TRUE)
              ErrorMsg <- c(ErrorMsg,sErrorMsg)
              cat(paste0(sErrorMsg,"\n"))
              return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
              stop(sErrorMsg)
            }
          }

          # scan for the input files
          sPuDat <- list.files(path = sPath, recursive = TRUE, pattern = paste0(sPUNAME,"$"), ignore.case = TRUE, full.names = TRUE)
          sSpecDat <- list.files(path = sPath, recursive = TRUE, pattern = paste0(sSPECNAME,"$"), ignore.case = TRUE, full.names = TRUE)
          sPuvsprDat <- list.files(path = sPath, recursive = TRUE, pattern = paste0(sPUVSPRNAME,"$"), ignore.case = TRUE, full.names = TRUE)
          sBoundDat <- list.files(path = sPath, recursive = TRUE, pattern = paste0(sBOUNDNAME,"$"), ignore.case = TRUE, full.names = TRUE)

          if (fMarZone)
          {
            fZoneBoundCost <- FALSE
            fZoneContrib <- FALSE
            fZoneContrib2 <- FALSE
            fZoneContrib3 <- FALSE
            fZoneTarget <- FALSE
            fZoneTarget2 <- FALSE
            fPuLock <- FALSE
            fPuZone <- FALSE
  
            sZonesDat <- list.files(path = sPath, recursive = TRUE, pattern = paste0(sZONESNAME,"$"), ignore.case = TRUE, full.names = TRUE)
            sCostsDat <- list.files(path = sPath, recursive = TRUE, pattern = paste0(sCOSTSNAME,"$"), ignore.case = TRUE, full.names = TRUE)
            sZoneCostDat <- list.files(path = sPath, recursive = TRUE, pattern = paste0(sZONECOSTNAME,"$"), ignore.case = TRUE, full.names = TRUE)
            if (sZONEBOUNDCOSTNAME != "")
            {
              sZoneBoundCostDat <- list.files(path = sPath, recursive = TRUE, pattern = paste0(sZONEBOUNDCOSTNAME,"$"), ignore.case = TRUE, full.names = TRUE)
              if (length(sZoneBoundCostDat) == 0)
              {
                # STOP ERROR
                sErrorMsg <- paste0("ERROR: zoneboundcost.dat file ",sZONEBOUNDCOSTNAME," not found ")
                write(paste0("STOP ",sErrorMsg," ",date()),file=sLogFile,append=TRUE)
                ErrorMsg <- c(ErrorMsg,sErrorMsg)
                cat(paste0(sErrorMsg,"\n"))
                return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
                stop(sErrorMsg)
              }
    
              if (length(sZoneBoundCostDat) > 1)
              {
                # STOP ERROR
                sErrorMsg <- paste0("ERROR: more than 1 zoneboundcost.dat found: ",paste0(sZoneBoundCostDat,collapse=" "))
                write(paste0("STOP ",sErrorMsg," ",date()),file=sLogFile,append=TRUE)
                ErrorMsg <- c(ErrorMsg,sErrorMsg)
                cat(paste0(sErrorMsg,"\n"))
                return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
                stop(sErrorMsg)
              }
    
              fZoneBoundCost <- TRUE
            }
            if (sZONECONTRIBNAME != "")
            {
              sZoneContribDat <- list.files(path = sPath, recursive = TRUE, pattern = paste0(sZONECONTRIBNAME,"$"), ignore.case = TRUE, full.names = TRUE)
    
              if (length(sZoneContribDat) == 0)
              {
                # STOP ERROR
                sErrorMsg <- paste0("ERROR: zonecontrib.dat file ",sZONECONTRIBNAME," not found ")
                write(paste0("STOP ",sErrorMsg," ",date()),file=sLogFile,append=TRUE)
                ErrorMsg <- c(ErrorMsg,sErrorMsg)
                cat(paste0(sErrorMsg,"\n"))
                return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
                stop(sErrorMsg)
              }
    
              if (length(sZoneContribDat) > 1)
              {
                # STOP ERROR
                sErrorMsg <- paste0("ERROR: more than 1 zonecontrib.dat found: ",paste0(sZoneContribDat,collapse=" "))
                write(paste0("STOP ",sErrorMsg," ",date()),file=sLogFile,append=TRUE)
                ErrorMsg <- c(ErrorMsg,sErrorMsg)
                cat(paste0(sErrorMsg,"\n"))
                return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
                stop(sErrorMsg)
              }
    
              fZoneContrib <- TRUE
            }
  
            if (sZONECONTRIB2NAME != "")
            {
              sZoneContrib2Dat <- list.files(path = sPath, recursive = TRUE, pattern = paste0(sZONECONTRIB2NAME,"$"), ignore.case = TRUE, full.names = TRUE)
    
              if (length(sZoneContrib2Dat) == 0)
              {
                # STOP ERROR
                sErrorMsg <- paste0("ERROR: zonecontrib2.dat file ",sZONECONTRIB2NAME," not found ")
                write(paste0("STOP ",sErrorMsg," ",date()),file=sLogFile,append=TRUE)
                ErrorMsg <- c(ErrorMsg,sErrorMsg)
                cat(paste0(sErrorMsg,"\n"))
                return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
                stop(sErrorMsg)
              }
    
              if (length(sZoneContrib2Dat) > 1)
              {
                # STOP ERROR
                sErrorMsg <- paste0("ERROR: more than 1 zonecontrib2.dat found: ",paste0(sZoneContrib2Dat,collapse=" "))
                write(paste0("STOP ",sErrorMsg," ",date()),file=sLogFile,append=TRUE)
                ErrorMsg <- c(ErrorMsg,sErrorMsg)
                cat(paste0(sErrorMsg,"\n"))
                return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
                stop(sErrorMsg)
              }
    
              fZoneContrib2 <- TRUE
            }
  
            if (sZONECONTRIB3NAME != "")
            {
              sZoneContrib3Dat <- list.files(path = sPath, recursive = TRUE, pattern = paste0(sZONECONTRIB3NAME,"$"), ignore.case = TRUE, full.names = TRUE)
    
              if (length(sZoneContrib3Dat) == 0)
              {
                # STOP ERROR
                sErrorMsg <- paste0("ERROR: zonecontrib3.dat file ",sZONECONTRIB3NAME," not found ")
                write(paste0("STOP ",sErrorMsg," ",date()),file=sLogFile,append=TRUE)
                ErrorMsg <- c(ErrorMsg,sErrorMsg)
                cat(paste0(sErrorMsg,"\n"))
                return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
                stop(sErrorMsg)
              }
    
              if (length(sZoneContrib3Dat) > 1)
              {
                # STOP ERROR
                sErrorMsg <- paste0("ERROR: more than 1 zonecontrib3.dat found: ",paste0(sZoneContrib3Dat,collapse=" "))
                write(paste0("STOP ",sErrorMsg," ",date()),file=sLogFile,append=TRUE)
                ErrorMsg <- c(ErrorMsg,sErrorMsg)
                cat(paste0(sErrorMsg,"\n"))
                return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
                stop(sErrorMsg)
              }
    
              fZoneContrib3 <- TRUE
            }
  
            if (sZONETARGETNAME != "")
            {
              sZoneTargetDat <- list.files(path = sPath, recursive = TRUE, pattern = paste0(sZONETARGETNAME,"$"), ignore.case = TRUE, full.names = TRUE)
    
              if (length(sZoneTargetDat) == 0)
              {
                # STOP ERROR
                sErrorMsg <- paste0("ERROR: zonetarget.dat file ",sZONETARGETNAME," not found ")
                write(paste0("STOP ",sErrorMsg," ",date()),file=sLogFile,append=TRUE)
                ErrorMsg <- c(ErrorMsg,sErrorMsg)
                cat(paste0(sErrorMsg,"\n"))
                return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
                stop(sErrorMsg)
              }
    
              if (length(sZoneTargetDat) > 1)
              {
                # STOP ERROR
                sErrorMsg <- paste0("ERROR: more than 1 zonetarget.dat found: ",paste0(sZoneTargetDat,collapse=" "))
                write(paste0("STOP ",sErrorMsg," ",date()),file=sLogFile,append=TRUE)
                ErrorMsg <- c(ErrorMsg,sErrorMsg)
                cat(paste0(sErrorMsg,"\n"))
                return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
                stop(sErrorMsg)
              }
    
              fZoneTarget <- TRUE
            }
  
            if (sZONETARGET2NAME != "")
            {
              sZoneTarget2Dat <- list.files(path = sPath, recursive = TRUE, pattern = paste0(sZONETARGET2NAME,"$"), ignore.case = TRUE, full.names = TRUE)
    
              if (length(sZoneTarget2Dat) == 0)
              {
                # STOP ERROR
                sErrorMsg <- paste0("ERROR: zonetarget2.dat file ",sZONETARGET2NAME," not found ")
                write(paste0("STOP ",sErrorMsg," ",date()),file=sLogFile,append=TRUE)
                ErrorMsg <- c(ErrorMsg,sErrorMsg)
                cat(paste0(sErrorMsg,"\n"))
                return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
                stop(sErrorMsg)
              }
    
              if (length(sZoneTarget2Dat) > 1)
              {
                # STOP ERROR
                sErrorMsg <- paste0("ERROR: more than 1 zonetarget2.dat found: ",paste0(sZoneTarget2Dat,collapse=" "))
                write(paste0("STOP ",sErrorMsg," ",date()),file=sLogFile,append=TRUE)
                ErrorMsg <- c(ErrorMsg,sErrorMsg)
                cat(paste0(sErrorMsg,"\n"))
                return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
                stop(sErrorMsg)
              }
    
              fZoneTarget2 <- TRUE
            }
  
            if (sPULOCKNAME != "")
            {
              sPuLockDat <- list.files(path = sPath, recursive = TRUE, pattern = paste0(sPULOCKNAME,"$"), ignore.case = TRUE, full.names = TRUE)
    
              if (length(sPuLockDat) == 0)
              {
                # STOP ERROR
                sErrorMsg <- paste0("ERROR: pulock.dat file ",sPULOCKNAME," not found ")
                write(paste0("STOP ",sErrorMsg," ",date()),file=sLogFile,append=TRUE)
                ErrorMsg <- c(ErrorMsg,sErrorMsg)
                cat(paste0(sErrorMsg,"\n"))
                return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
                stop(sErrorMsg)
              }
    
              if (length(sPuLockDat) > 1)
              {
                # STOP ERROR
                sErrorMsg <- paste0("ERROR: more than 1 pulock.dat found: ",paste0(sPuLockDat,collapse=" "))
                write(paste0("STOP ",sErrorMsg," ",date()),file=sLogFile,append=TRUE)
                ErrorMsg <- c(ErrorMsg,sErrorMsg)
                cat(paste0(sErrorMsg,"\n"))
                return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
                stop(sErrorMsg)
              }
    
              fPuLock <- TRUE
            }
  
            if (sPUZONENAME != "")
            {
              sPuZoneDat <- list.files(path = sPath, recursive = TRUE, pattern = paste0(sPUZONENAME,"$"), ignore.case = TRUE, full.names = TRUE)
    
              if (length(sPuZoneDat) == 0)
              {
                # STOP ERROR
                sErrorMsg <- paste0("ERROR: puzone.dat file ",sPUZONENAME," not found ")
                write(paste0("STOP ",sErrorMsg," ",date()),file=sLogFile,append=TRUE)
                ErrorMsg <- c(ErrorMsg,sErrorMsg)
                cat(paste0(sErrorMsg,"\n"))
                return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
                stop(sErrorMsg)
              }
    
              if (length(sPuZoneDat) > 1)
              {
                # STOP ERROR
                sErrorMsg <- paste0("ERROR: more than 1 puzone.dat found: ",paste0(sPuZoneDat,collapse=" "))
                write(paste0("STOP ",sErrorMsg," ",date()),file=sLogFile,append=TRUE)
                ErrorMsg <- c(ErrorMsg,sErrorMsg)
                cat(paste0(sErrorMsg,"\n"))
                return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
                stop(sErrorMsg)
              }
    
              fPuZone <- TRUE
            }
          }

          write(paste0("paths searched ",date()),file=sLogFile,append=TRUE)
          cat("paths searched\n")

          if (length(sPuDat) == 0)
          {
            # STOP ERROR
            sErrorMsg <- paste0("ERROR: pu.dat file ",sPUNAME," not found ")
            write(paste0("STOP ",sErrorMsg," ",date()),file=sLogFile,append=TRUE)
            ErrorMsg <- c(ErrorMsg,sErrorMsg)
            cat(paste0(sErrorMsg,"\n"))
            return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
            stop(sErrorMsg)
          }
          if (length(sSpecDat) == 0)
          {
            # STOP ERROR
            sErrorMsg <- paste0("ERROR: spec.dat file ",sSPECNAME," not found ")
            write(paste0("STOP ",sErrorMsg," ",date()),file=sLogFile,append=TRUE)
            ErrorMsg <- c(ErrorMsg,sErrorMsg)
            cat(paste0(sErrorMsg,"\n"))
            return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
            stop(sErrorMsg)
          }
          if (length(sPuvsprDat) == 0)
          {
            # STOP ERROR
            sErrorMsg <- paste0("ERROR: puvspr.dat file ",sPUVSPRNAME," not found ")
            write(paste0("STOP ",sErrorMsg," ",date()),file=sLogFile,append=TRUE)
            ErrorMsg <- c(ErrorMsg,sErrorMsg)
            cat(paste0(sErrorMsg,"\n"))
            return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
            stop(sErrorMsg)
          }
          if (length(sBoundDat) == 0)
          {
            # warning message but not critical
            sWarningMsg <- paste0("Warning: bound.dat file ",sBOUNDNAME," not found ")
            write(paste0(sWarningMsg," ",date()),file=sLogFile,append=TRUE)
            WarningMsg <- c(WarningMsg,sWarningMsg)
            cat(paste0(sWarningMsg,"\n"))
          }

          if (length(sPuDat) > 1)
          {
            # STOP ERROR
            sErrorMsg <- paste0("ERROR: more than 1 pu.dat file ",sPUNAME," found ")
            write(paste0("STOP ",sErrorMsg," ",date()),file=sLogFile,append=TRUE)
            ErrorMsg <- c(ErrorMsg,sErrorMsg)
            cat(paste0(sErrorMsg,"\n"))
            return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
            stop(sErrorMsg)
          }
          if (length(sSpecDat) > 1)
          {
            # STOP ERROR
            sErrorMsg <- paste0("ERROR: more than 1 spec.dat file ",sSPECNAME," found ")
            write(paste0("STOP ",sErrorMsg," ",date()),file=sLogFile,append=TRUE)
            ErrorMsg <- c(ErrorMsg,sErrorMsg)
            cat(paste0(sErrorMsg,"\n"))
            return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
            stop(sErrorMsg)
          }
          if (length(sPuvsprDat) > 1)
          {
            # STOP ERROR
            sErrorMsg <- paste0("ERROR: more than 1 puvspr.dat file ",sPUVSPRNAME," found ")
            write(paste0("STOP ",sErrorMsg," ",date()),file=sLogFile,append=TRUE)
            ErrorMsg <- c(ErrorMsg,sErrorMsg)
            cat(paste0(sErrorMsg,"\n"))
            return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
            stop(sErrorMsg)
          }
          if (length(sBoundDat) > 1)
          {
            # STOP ERROR
            sErrorMsg <- paste0("ERROR: more than 1 bound.dat file ",sBOUNDNAME," found ")
            write(paste0("STOP ",sErrorMsg," ",date()),file=sLogFile,append=TRUE)
            ErrorMsg <- c(ErrorMsg,sErrorMsg)
            cat(paste0(sErrorMsg,"\n"))
            return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
            stop(sErrorMsg)
          }

          if (.Platform$pkgType == "source")
          {
            incProgress(0,detail="converting Marxan files")
          
            system(paste0("dos2unix ",sPuDat))
            system(paste0("dos2unix ",sSpecDat))
            system(paste0("dos2unix ",sPuvsprDat))
            if (length(sBoundDat) == 1)
            {
              system(paste0("dos2unix ",sBoundDat))
            }
            if (fMarZone)
            {
              incProgress(0,detail="converting MarZone files")
          
              system(paste0("dos2unix ",sZonesDat))
              system(paste0("dos2unix ",sCostsDat))
              system(paste0("dos2unix ",sZoneCostDat))
              if (fZoneBoundCost) { system(paste0("dos2unix ",sZoneBoundCostDat)) }
              if (fZoneContrib)   { system(paste0("dos2unix ",sZoneContribDat)) }
              if (fZoneContrib2)  { system(paste0("dos2unix ",sZoneContrib2)) }
              if (fZoneContrib3)  { system(paste0("dos2unix ",sZoneContrib3Dat)) }
              if (fZoneTarget)    { system(paste0("dos2unix ",sZoneTargetDat)) }
              if (fZoneTarget2)   { system(paste0("dos2unix ",sZoneTarget2Dat)) }
              if (fPuLock)        { system(paste0("dos2unix ",sPuLockDat)) }
              if (fPuZone)        { system(paste0("dos2unix ",sPuZoneDat)) }
            }
          }

          write(paste0("reading input files ",date()),file=sLogFile,append=TRUE)
          cat("reading input files\n")

          # read the default input.dat
          inputdat <- readLines(paste0(sDataPath,"/no_bound_input.dat"))

  
          # read the input files
          incProgress(0,detail="reading Marxan files")
          
          pu.dat <- smart_read(sPuDat)
          spec.dat <- smart_read(sSpecDat)
          puvspr.dat <- smart_read(sPuvsprDat)
          fBoundDat <- FALSE
          if (length(sBoundDat) == 1)
          {
            fBoundDat <- TRUE
            bound.dat <- smart_read(sBoundDat)
          }

          # add the required input parameters
          if (fBoundDat)
          {
            inputdat <- c(inputdat,"")
            inputdat <- c(inputdat,"BOUNDNAME bound.dat")
          }
          if (fMarZone)
          {
            # read and save the MarZone input files, adding required input parameters as we go
            if (! fBoundDat)
            {
              inputdat <- c(inputdat,"")
            }
            inputdat <- c(inputdat,"ZONESNAME zones.dat")
            inputdat <- c(inputdat,"COSTSNAME costs.dat")
            inputdat <- c(inputdat,"ZONECOSTNAME zonecost.dat")
    
            incProgress(0,detail="reading MarZone files")
          
            zones.dat <- smart_read(sZonesDat)
            costs.dat <- smart_read(sCostsDat)
            zonecost.dat <- smart_read(sZoneCostDat)
            write.csv(zones.dat,paste0(sOutDir,"/input/zones.dat"),quote=FALSE,row.names=FALSE)
            write.csv(costs.dat,paste0(sOutDir,"/input/costs.dat"),quote=FALSE,row.names=FALSE)
            write.csv(zonecost.dat,paste0(sOutDir,"/input/zonecost.dat"),quote=FALSE,row.names=FALSE)
    
            if (fZoneBoundCost)
            {
              zoneboundcost.dat <- smart_read(sZoneBoundCostDat)
              write.csv(zoneboundcost.dat,paste0(sOutDir,"/input/zoneboundcost.dat"),quote=FALSE,row.names=FALSE)
              inputdat <- c(inputdat,"ZONEBOUNDCOSTNAME zoneboundcost.dat")
            }
            if (fZoneContrib)
            {
              zonecontrib.dat <- smart_read(sZoneContribDat)
              write.csv(zonecontrib.dat,paste0(sOutDir,"/input/zonecontrib.dat"),quote=FALSE,row.names=FALSE)
              inputdat <- c(inputdat,"ZONECONTRIBNAME zonecontrib.dat")
            }
            if (fZoneContrib2)
            {
              zonecontrib2.dat <- smart_read(sZoneContrib2Dat)
              write.csv(zonecontrib2.dat,paste0(sOutDir,"/input/zonecontrib2.dat"),quote=FALSE,row.names=FALSE)
              inputdat <- c(inputdat,"ZONECONTRIB2NAME zonecontrib2.dat")
            }
            if (fZoneContrib3)
            {
              zonecontrib3.dat <- smart_read(sZoneContrib3Dat)
              write.csv(zonecontrib3.dat,paste0(sOutDir,"/input/zonecontrib3.dat"),quote=FALSE,row.names=FALSE)
              inputdat <- c(inputdat,"ZONECONTRIB3NAME zonecontrib3.dat")
            }
            if (fZoneTarget)
            {
              zonetarget.dat <- smart_read(sZoneTargetDat)
              write.csv(zonetarget.dat,paste0(sOutDir,"/input/zonetarget.dat"),quote=FALSE,row.names=FALSE)
              inputdat <- c(inputdat,"ZONETARGETNAME zonetarget.dat")
            }
            if (fZoneTarget2)
            {
              zonetarget2.dat <- smart_read(sZoneTarget2Dat)
              write.csv(zonetarget2.dat,paste0(sOutDir,"/input/zonetarget2.dat"),quote=FALSE,row.names=FALSE)
              inputdat <- c(inputdat,"ZONETARGET2NAME zonetarget2.dat")
            }
            if (fPuLock)
            {
              pulock.dat <- smart_read(sPuLockDat)
              write.csv(pulock.dat,paste0(sOutDir,"/input/pulock.dat"),quote=FALSE,row.names=FALSE)
              inputdat <- c(inputdat,"PULOCKNAME pulock.dat")
            }
            if (fPuZone)
            {
              puzone.dat <- smart_read(sPuZoneDat)
              write.csv(puzone.dat,paste0(sOutDir,"/input/puzone.dat"),quote=FALSE,row.names=FALSE)
              inputdat <- c(inputdat,"PUZONENAME puzone.dat")
            }
          }

          # save the input.dat
          writeLines(inputdat,con=paste0(sOutDir,"/input.dat"))

          write(paste0("input files read ",date()),file=sLogFile,append=TRUE)
          cat("input files read\n")

          incProgress(0,detail="writing Marxan files")
          
          # save the Marxan input files
          write.csv(pu.dat,paste0(sOutDir,"/input/pu.dat"),quote=FALSE,row.names=FALSE)
          # if spec.dat has a name field, make the name field the last field
          # this will reduce errors where users include delimeter characters in the feature names
          cnames <- colnames(spec.dat)
          iName <- which(grepl("name",cnames))
          if (length(iName) > 0)
          {
            spec.dat <- spec.dat[c(cnames[-iName],"name")]
          }
          write.csv(spec.dat,paste0(sOutDir,"/input/spec.dat"),quote=FALSE,row.names=FALSE)
          if (fBoundDat)
          {
            write.csv(bound.dat,paste0(sOutDir,"/input/bound.dat"),quote=FALSE,row.names=FALSE)
          }

          incProgress(0,detail="converting matrix file")
          
          if (ncol(puvspr.dat) == 3)
          {
            # This is a sparse matrix. Ensure it is sorted in the correct order.
            puorder.dat <- puvspr.dat[order(puvspr.dat$pu),]
            write.csv(puorder.dat,paste0(sOutDir,"/input/puorder.dat"),quote=FALSE,row.names=FALSE)
            sporder.dat <- puvspr.dat[order(puvspr.dat$species),]
            write.csv(sporder.dat,paste0(sOutDir,"/input/sporder.dat"),quote=FALSE,row.names=FALSE)

            # If puorder.dat and puvspr.dat do not have pu in the same order, warn user their matrix wasn't sorted ok.
            if (sum(order(puvspr.dat$pu) != order(puorder.dat$pu)) > 0)
            {
              # warning message but not critical
              sWarningMsg <- paste0("Warning: puvspr.dat file ",sPuvsprDat," was not ordered from lowest to highest pu ")
              write(paste0(sWarningMsg," ",date()),file=sLogFile,append=TRUE)
              WarningMsg <- c(WarningMsg,sWarningMsg)
              cat(paste0(sWarningMsg,"\n"))
            }
          } else {
            # we need to convert this to a sparse matrix
            # generate puorder.dat
            sPuOrder <- paste0(sOutDir,"/input/puorder.dat")
            write('species,pu,amount',file=sPuOrder)
            for (j in 1:nrow(matrix))
            {
              for (i in 2:ncol(matrix))
              {
                rAmount <- matrix[j,i]

                if (rAmount > 0)
                {
                  iPUID <- matrix[j,1]
                  iSPID <- substring(colnames(matrix)[i],2)

                  write(paste(iSPID,iPUID,rAmount,sep=","),
                        file=sPuOrder,append=TRUE)
                }
              }
            }
            # generate sporder.dat
            sSpOrder <- paste0(sOutDir,"/input/sporder.dat")
            write('species,pu,amount',file=sSpOrder)
            for (i in 2:ncol(matrix))
            {
              for (j in 1:nrow(matrix))
              {
                rAmount <- matrix[j,i]

                if (rAmount > 0)
                {
                  iPUID <- matrix[j,1]
                  iSPID <- substring(colnames(matrix)[i],2)

                  write(paste(iSPID,iPUID,rAmount,sep=","),
                        file=sSpOrder,append=TRUE)
                }
              }
            }
            # Warn user their matrix wasn't in the correct format. warning message but not critical
            sWarningMsg <- paste0("puvspr.dat file ",sPuvsprDat," was not in sparse matrix format ")
            write(paste0(sWarningMsg," ",date()),file=sLogFile,append=TRUE)
            WarningMsg <- c(WarningMsg,sWarningMsg)
            cat(paste0(sWarningMsg,"\n"))
          }

          write(paste0("marxan files processed ",date()),file=sLogFile,append=TRUE)
          cat("marxan files processed\n")
          
      }) # withProgress Reading marxan files

      withProgress(message="Planning units",value=0,
      {
          # find shapefiles
          ShapeFiles <- list.files(path = sPath, recursive = TRUE, pattern = "*.shp$", ignore.case = TRUE, full.names = TRUE)
          # ignore any .xlm files if they are present
          WhichXml <- regexpr(".xml",ShapeFiles) > 0
          FilterShapes <- c()
          for (i in 1:length(ShapeFiles))
          {
            if (WhichXml[i] == FALSE)
            {
              FilterShapes <- c(FilterShapes,ShapeFiles[i])
            }
          }
          if (length(FilterShapes) > 0)
          {
            if (length(FilterShapes) == 1)
            {
              # we have 1 shapefile and assume this is the planning unit layer
              sPuLayer <- FilterShapes[1]
              sOutlineLayer <- ""
            } else {
              if (length(FilterShapes) == 2)
              {
                # we have 2 shapefiles: assume one is planning unit layer and other is outline
                # guess which is which
                WhichShapefile <- regexpr("pulayer.shp",FilterShapes) > 0
                if (sum(WhichShapefile) != 1)
                {
                  # Error, can't guess which shapefile is the pulayer
                  # STOP ERROR
                  sErrorMsg <- "ERROR: 2 shapefiles found, can't guess which one is the pulayer"
                  write(paste0("STOP ",sErrorMsg," ",date()),file=sLogFile,append=TRUE)
                  ErrorMsg <- c(ErrorMsg,sErrorMsg)
                  cat(paste0(sErrorMsg,"\n"))
                  return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
                  stop(sErrorMsg)
                }
                if (WhichShapefile[1] == TRUE)
                {
                  sPuLayer <- FilterShapes[1]
                  sOutlineLayer <- FilterShapes[2]
                } else {
                  sPuLayer <- FilterShapes[2]
                  sOutlineLayer <- FilterShapes[1]
                }
              } else {
                # We have more than 2 shapefiles. This is an error condition.
                # STOP ERROR
                sErrorMsg <- "ERROR: more than 2 shapefiles found"
                write(paste0("STOP ",sErrorMsg," ",date()),file=sLogFile,append=TRUE)
                ErrorMsg <- c(ErrorMsg,sErrorMsg)
                cat(paste0(sErrorMsg,"\n"))
                return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
                stop(sErrorMsg)
              }
            }
          }

          incProgress(0,detail="reading shapefile")
          
          tryCatch(
          {
              # load the planning unit shapefile
              pushapefile <- readOGR(dirname(sPuLayer),file_path_sans_ext(basename(sPuLayer)))
          },
          error=function(cond)
          {
              # STOP ERROR
              sErrorMsg <- paste0("ERROR: can't read shapefile >",cond,"<")
              write(paste0("STOP ",sErrorMsg," ",date()),file=sLogFile,append=TRUE)
              ErrorMsg <- c(ErrorMsg,sErrorMsg)
              cat(paste0(sErrorMsg,"\n"))
              return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
              stop(sErrorMsg)
          }
          )

          incProgress(0,detail="writing shapefile")
          
          tryCatch(
          {
              writeOGR(pushapefile,paste0(sOutDir,"/pulayer"),"pulayer",driver="ESRI Shapefile",overwrite_layer=TRUE)
          },
          error=function(cond)
          {
              # STOP ERROR
              sErrorMsg <- paste0("ERROR: can't write shapefile >",cond,"<")
              write(paste0("STOP ",sErrorMsg," ",date()),file=sLogFile,append=TRUE)
              ErrorMsg <- c(ErrorMsg,sErrorMsg)
              cat(paste0(sErrorMsg,"\n"))
              return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
              stop(sErrorMsg)
          }
          )

          write(paste0("pulayer read ",sPuLayer," ",date()),file=sLogFile,append=TRUE)
          cat(paste0("pulayer read ",sPuLayer,"\n"))

          incProgress(0,detail="reading dbf table")
          
          tryCatch(
          {
              sDbfTable <- paste0(dirname(sPuLayer),"/",file_path_sans_ext(basename(sPuLayer)),".dbf")
              if (!file.exists(sDbfTable))
              {
                  sDbfTable <- paste0(dirname(sPuLayer),"/",file_path_sans_ext(basename(sPuLayer)),".DBF")
              }
              putable <- read.dbf(sDbfTable)
          },
          error=function(cond)
          {
              # STOP ERROR
              sErrorMsg <- paste0("ERROR: can't read dbf table >",cond,"<")
              write(paste0("STOP ",sErrorMsg," ",date()),file=sLogFile,append=TRUE)
              ErrorMsg <- c(ErrorMsg,sErrorMsg)
              cat(paste0(sErrorMsg,"\n"))
              return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
              stop(sErrorMsg)
          }
          )

          # guess which field is PUID
          iPUIDfield <- which(colnames(putable) == "PUID")
          if (length(iPUIDfield) == 0)
          {
            iPUIDfield <- which(colnames(putable) == "PU_ID")
          }
          if (length(iPUIDfield) == 0)
          {
            iPUIDfield <- which(colnames(putable) == "puid")
          }
          if (length(iPUIDfield) == 0)
          {
            iPUIDfield <- which(colnames(putable) == "pu_id")
          }

          if (length(iPUIDfield) == 0)
          {
            # We can't find PUID field in the planning unit shapefile.
            # STOP ERROR
            sErrorMsg <- "ERROR: can't find PUID field in the planning unit shapefile "
            write(paste0("STOP ",sErrorMsg," ",date()),file=sLogFile,append=TRUE)
            ErrorMsg <- c(ErrorMsg,sErrorMsg)
            cat(paste0(sErrorMsg,"\n"))
            return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
            stop(sErrorMsg)
          }
          colnames(putable)[iPUIDfield] <- "PUID"

          incProgress(0,detail="querying dbf table")
          
          putable <- sqldf("SELECT PUID FROM putable")
          dim(putable)

          incProgress(0,detail="writing dbf table")
          
          write.dbf(putable,paste0(sOutDir,"/pulayer/pulayer.dbf"))
  
          incProgress(0,detail="simplifying shapefile")
          
          tryCatch(
          {
              # simplify the planning unit layer
              pushapefile_simp10 <- gSimplify(pushapefile,tol=10,topologyPreserve=TRUE)
          },
          error=function(cond)
          {
              # STOP ERROR
              sErrorMsg <- paste0("ERROR: can't simplify shapefile >",cond,"<")
              write(paste0("STOP ",sErrorMsg," ",date()),file=sLogFile,append=TRUE)
              ErrorMsg <- c(ErrorMsg,sErrorMsg)
              cat(paste0(sErrorMsg,"\n"))
              return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
              stop(sErrorMsg)
          }
          )
  
          write(paste0("planning units simplified ",date()),file=sLogFile,append=TRUE)
          cat("planning units simplified\n")

          incProgress(0,detail="dissolving shapefile")
          
          tryCatch(
          {
              # dissolve the planning unit layer
              pushapefile_dissolve <- gUnaryUnion(pushapefile_simp10)
          },
          error=function(cond)
          {
              # STOP ERROR
              sErrorMsg <- paste0("ERROR: can't dissolve shapefile >",cond,"<")
              write(paste0("STOP ",sErrorMsg," ",date()),file=sLogFile,append=TRUE)
              ErrorMsg <- c(ErrorMsg,sErrorMsg)
              cat(paste0(sErrorMsg,"\n"))
              return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
              stop(sErrorMsg)
          }
          )
  
          write(paste0("planning units dissolved ",date()),file=sLogFile,append=TRUE)
          cat("planning units dissolved\n")

          incProgress(0,detail="creating shapefile outlines")
          
          tryCatch(
          {
              # remove the holes from dissolved polygon
              outerRings <- Filter(function(f){f@ringDir==1},pushapefile_dissolve@polygons[[1]]@Polygons)
              outerBounds <- SpatialPolygons(list(Polygons(outerRings,ID=1)))
              puoutline <- SpatialPolygons2PolySet(outerBounds)
              fOutline <- TRUE
          },
          error=function(cond)
          {
              # STOP ERROR
              sErrorMsg <- paste0("ERROR: can't create shapefile outlines >",cond,"<")
              write(paste0("STOP ",sErrorMsg," ",date()),file=sLogFile,append=TRUE)
              ErrorMsg <- c(ErrorMsg,sErrorMsg)
              cat(paste0(sErrorMsg,"\n"))
              return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
              stop(sErrorMsg)
          }
          )

          incProgress(0,detail="converting shapefile")
          
          tryCatch(
          {
              # create the pulayer.Rdata file
              pustatus_ <- unlist(pu.dat$status)
              pulayer_ <<- SpatialPolygons2PolySet(pushapefile_simp10)
              y_ <- bbox(pushapefile_simp10)[2,2] - bbox(pushapefile_simp10)[2,1]
              x_ <- bbox(pushapefile_simp10)[1,2] - bbox(pushapefile_simp10)[1,1]
          },
          error=function(cond)
          {
              # STOP ERROR
              sErrorMsg <- paste0("ERROR: can't convert shapefile >",cond,"<")
              write(paste0("STOP ",sErrorMsg," ",date()),file=sLogFile,append=TRUE)
              ErrorMsg <- c(ErrorMsg,sErrorMsg)
              cat(paste0(sErrorMsg,"\n"))
              return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
              stop(sErrorMsg)
          }
          )

          incProgress(0,detail="saving pulayer.Rdata")
          
          tryCatch(
          {
              sRdata <- paste0(sOutDir,"/pulayer/pulayer.Rdata")
              save(fOutline,puoutline,pulayer_,pustatus_,x_,y_,file=sRdata)
          },
          error=function(cond)
          {
              # STOP ERROR
              sErrorMsg <- paste0("ERROR: can't save pulayer.Rdata >",cond,"<")
              write(paste0("STOP ",sErrorMsg," ",date()),file=sLogFile,append=TRUE)
              ErrorMsg <- c(ErrorMsg,sErrorMsg)
              cat(paste0(sErrorMsg,"\n"))
              return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
              stop(sErrorMsg)
          }
          )

          write(paste0("pulayer.Rdata created ",sRdata," ",date()),file=sLogFile,append=TRUE)
          cat(paste0("pulayer.Rdata created ",sRdata,"\n"))
          
          write(paste0("shapefiles processed ",date()),file=sLogFile,append=TRUE)
          cat("shapefiles processed\n")

          write(paste0("ParseMarxanZip log end ",date()),file=sLogFile,append=TRUE)
          cat(paste0("ParseMarxanZip end\n"))

          return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
          
      }) # withProgress Reading planning unit shapes
      
  }) # withProgress
}

ReadParseErrors <- function(ParseResult)
{
  Warnings <<- c()
  Errors <<- c()
  if (length(ParseResult) > 2)
  {
    for (i in 3:length(ParseResult))
    {
      if (grepl("Warning",ParseResult[i]))
      {
        Warnings <<- c(Warnings,ParseResult[i])
      }
      if (grepl("ERROR",ParseResult[i]))
      {
        Errors <<- c(Errors,ParseResult[i])
      }
    }
  }
}

IngestMarxanDatabase <- function(sPath,sShinyUserPath,sUserName,sDatabaseName)
{
  # copy the ingested Marxan dataset to the users home directory
  dir.create(paste0(sShinyUserPath,"/",sUserName))
  if (fMarZone)
  {
    dir.create(paste0(sShinyUserPath,"/",sUserName,"/marzone/",sDatabaseName))
    system(paste0("cp -r ",sPath,"/marzone/ ",sShinyUserPath,"/",sUserName,"/marzone/",sDatabaseName))
  } else {
    dir.create(paste0(sShinyUserPath,"/",sUserName,"/marxan/",sDatabaseName))
    system(paste0("cp -r ",sPath,"/marxan/ ",sShinyUserPath,"/",sUserName,"/marxan/",sDatabaseName))
  }
}

RunMarxan <- function(sMarxanDir,sExecutable,iCores,iRepsPerCore)
{
  # BLM parameter
  inputdat <- readLines(paste0(sMarxanDir,"/input.dat"))
  randomseeds <- round(runif(10)*100000)

  # run Marxan
  foreach(i=1:iCores) %dopar%
  {
    dir.create(paste0(sMarxanDir,"/core",i))
    file.copy(paste0(sShinyDataPath,"/",sExecutable),paste0(sMarxanDir,"/core",i,"/",sExecutable))
    system(paste0("chmod +x ",sMarxanDir,"/core",i,"/",sExecutable))

    # set parameters for multi core
    iINPUTDIRparam <- which(regexpr("INPUTDIR",inputdat)==1)
    iOUTPUTDIRparam <- which(regexpr("OUTPUTDIR",inputdat)==1)
    iSCENNAMEparam <- which(regexpr("SCENNAME",inputdat)==1)
    iNUMREPSparam <- which(regexpr("NUMREPS",inputdat)==1)
    iRANDSEEDparam <- which(regexpr("RANDSEED",inputdat)==1)
    inputdat[iINPUTDIRparam] <- paste0("INPUTDIR ",sMarxanDir,"/input")
    inputdat[iOUTPUTDIRparam] <- paste0("OUTPUTDIR ",sMarxanDir,"/output")
    inputdat[iSCENNAMEparam] <- paste0("SCENNAME output",i)
    inputdat[iNUMREPSparam] <- paste0("NUMREPS ",iRepsPerCore)
    inputdat[iRANDSEEDparam] <- paste0("RANDSEED ",randomseeds[i])
    writeLines(inputdat,paste0(sMarxanDir,"/core",i,"/input.dat"))

    setwd(paste0(sMarxanDir,"/core",i))
    system(paste0("./",sExecutable," -s"))
  }
  for (i in 1:iCores)
  {
    file.remove(paste0(sMarxanDir,"/core",i,"/input.dat"))
  }
}

JoinParallelResults_MarZone <- function(sMarxanDir,iCores,iRepsPerCore,iZones)
{
    iSolutions <- round(iCores*iRepsPerCore)
    # combine the summary tables
    sumtable <- c()
    for (i in 1:iCores)
    {
        sumtable_ <- read.csv(paste0(sMarxanDir,"/output/output",i,"_sum.csv"))
        sumtable <- rbind(sumtable,sumtable_)
    }
    for (i in 1:iSolutions)
    {
        sumtable[i,1] <- i
    }
    write.csv(sumtable,
              paste0(sMarxanDir,"/output/output_sum.csv"),
              quote=FALSE,row.names=FALSE)

    # detect best solution
    iBest <- which(sumtable[,2]==min(sumtable[,2]))
    if (length(iBest) > 0)
    {
        iBest <- iBest[1]
    }
    
    # rename mv files and solution files
    iSol <- 0
    for (i in 1:iCores)
    {
        for (j in 1:iRepsPerCore)
        {
            iSol <- iSol + 1
        
            file.rename(paste0(sMarxanDir,"/output/output",i,"_mv",PadInt(j),".csv"),
                        paste0(sMarxanDir,"/output/output_mv",PadInt(iSol),".csv"))
        
            file.rename(paste0(sMarxanDir,"/output/output",i,"_r",PadInt(j),".csv"),
                        paste0(sMarxanDir,"/output/output_r",PadInt(iSol),".csv"))
        }
    }

    # copy _mvbest and _best files
    file.copy(paste0(sMarxanDir,"/output/output_mv",PadInt(iBest),".csv"),
              paste0(sMarxanDir,"/output/output_mvbest.csv"),
              overwrite=TRUE)
    file.copy(paste0(sMarxanDir,"/output/output_r",PadInt(iBest),".csv"),
              paste0(sMarxanDir,"/output/output_best.csv"),
              overwrite=TRUE)
    
    # join ssoln files
    ssolntable <- read.csv(paste0(sMarxanDir,"/output/output",i,"_ssoln.csv"))
    sPUName <- colnames(ssolntable)[1]
    ZoneNames <- c()
    for (k in 1:iZones)
    {
        sZoneName <- colnames(ssolntable)[2+k]
        ZoneNames <- c(ZoneNames,sZoneName)
        
        colnames(ssolntable)[2+k] <- paste0("R1Z",k)
    }
    #sZ1Name <- colnames(ssolntable)[3]
    #sZ2Name <- colnames(ssolntable)[4]
    #sZ3Name <- colnames(ssolntable)[5]
    colnames(ssolntable)[1] <- "PUID"
    colnames(ssolntable)[2] <- "R1N"
    #colnames(ssolntable)[3] <- "R1Z1"
    #colnames(ssolntable)[4] <- "R1Z2"
    #colnames(ssolntable)[5] <- "R1Z3"
    for (i in 2:iCores)
    {
        ssolntable_ <- read.csv(paste0(sMarxanDir,"/output/output",i,"_ssoln.csv"))
        colnames(ssolntable_)[1] <- "PUID"
        ssolntable <- sqldf("SELECT * from ssolntable LEFT JOIN ssolntable_ USING(PUID)")
        colnames(ssolntable)[ncol(ssolntable)] <- paste0("SS",i)
        for (k in 1:iZones)
        {
            colnames(ssolntable)[ncol(ssolntable)-iZones+k] <- paste0("R",i,"Z",k)
        }
        colnames(ssolntable)[ncol(ssolntable)-iZones] <- paste0("R",i,"N")
        #colnames(ssolntable)[ncol(ssolntable)-3] <- paste0("R",i,"N")
        #colnames(ssolntable)[ncol(ssolntable)-2] <- paste0("R",i,"Z1")
        #colnames(ssolntable)[ncol(ssolntable)-1] <- paste0("R",i,"Z2")
        #colnames(ssolntable)[ncol(ssolntable)] <- paste0("R",i,"Z3")
    }
    # NOTE: these 5 lines are complicated: adding 10 columns with indeterminate names...
    ssolntable$number <- ssolntable$R1N + ssolntable$R2N + ssolntable$R3N + ssolntable$R4N + ssolntable$R5N + ssolntable$R6N + ssolntable$R7N + ssolntable$R8N + ssolntable$R9N + ssolntable$R10N
    ssolntable$Z1 <- ssolntable$R1Z1 + ssolntable$R2Z1 + ssolntable$R3Z1 + ssolntable$R4Z1 + ssolntable$R5Z1 + ssolntable$R6Z1 + ssolntable$R7Z1 + ssolntable$R8Z1 + ssolntable$R9Z1 + ssolntable$R10Z1
    ssolntable$Z2 <- ssolntable$R1Z2 + ssolntable$R2Z2 + ssolntable$R3Z2 + ssolntable$R4Z2 + ssolntable$R5Z2 + ssolntable$R6Z2 + ssolntable$R7Z2 + ssolntable$R8Z2 + ssolntable$R9Z2 + ssolntable$R10Z2
    if (iZones > 2) { ssolntable$Z3 <- ssolntable$R1Z3 + ssolntable$R2Z3 + ssolntable$R3Z3 + ssolntable$R4Z3 + ssolntable$R5Z3 + ssolntable$R6Z3 + ssolntable$R7Z3 + ssolntable$R8Z3 + ssolntable$R9Z3 + ssolntable$R10Z3 }
    if (iZones > 3) { ssolntable$Z4 <- ssolntable$R1Z4 + ssolntable$R2Z4 + ssolntable$R3Z4 + ssolntable$R4Z4 + ssolntable$R5Z4 + ssolntable$R6Z4 + ssolntable$R7Z4 + ssolntable$R8Z4 + ssolntable$R9Z4 + ssolntable$R10Z4 }
    if (iZones > 4) { ssolntable$Z5 <- ssolntable$R1Z5 + ssolntable$R2Z5 + ssolntable$R3Z5 + ssolntable$R4Z5 + ssolntable$R5Z5 + ssolntable$R6Z5 + ssolntable$R7Z5 + ssolntable$R8Z5 + ssolntable$R9Z5 + ssolntable$R10Z5 }
    if (iZones > 5) { ssolntable$Z6 <- ssolntable$R1Z6 + ssolntable$R2Z6 + ssolntable$R3Z6 + ssolntable$R4Z6 + ssolntable$R5Z6 + ssolntable$R6Z6 + ssolntable$R7Z6 + ssolntable$R8Z6 + ssolntable$R9Z6 + ssolntable$R10Z6 }
    if (iZones > 6) { ssolntable$Z7 <- ssolntable$R1Z7 + ssolntable$R2Z7 + ssolntable$R3Z7 + ssolntable$R4Z7 + ssolntable$R5Z7 + ssolntable$R6Z7 + ssolntable$R7Z7 + ssolntable$R8Z7 + ssolntable$R9Z7 + ssolntable$R10Z7 }
    if (iZones > 7) { ssolntable$Z8 <- ssolntable$R1Z8 + ssolntable$R2Z8 + ssolntable$R3Z8 + ssolntable$R4Z8 + ssolntable$R5Z8 + ssolntable$R6Z8 + ssolntable$R7Z8 + ssolntable$R8Z8 + ssolntable$R9Z8 + ssolntable$R10Z8 }
    if (iZones > 8) { ssolntable$Z9 <- ssolntable$R1Z9 + ssolntable$R2Z9 + ssolntable$R3Z9 + ssolntable$R4Z9 + ssolntable$R5Z9 + ssolntable$R6Z9 + ssolntable$R7Z9 + ssolntable$R8Z9 + ssolntable$R9Z9 + ssolntable$R10Z9 }
    if (iZones > 9) { ssolntable$Z10 <- ssolntable$R1Z10 + ssolntable$R2Z10 + ssolntable$R3Z10 + ssolntable$R4Z10 + ssolntable$R5Z10 + ssolntable$R6Z10 + ssolntable$R7Z10 + ssolntable$R8Z10 + ssolntable$R9Z10 + ssolntable$R10Z10 }
    if (iZones > 10) { ssolntable$Z11 <- ssolntable$R1Z11 + ssolntable$R2Z11 + ssolntable$R3Z11 + ssolntable$R4Z11 + ssolntable$R5Z11 + ssolntable$R6Z11 + ssolntable$R7Z11 + ssolntable$R8Z11 + ssolntable$R9Z11 + ssolntable$R10Z11 }
    if (iZones > 11) { ssolntable$Z12 <- ssolntable$R1Z12 + ssolntable$R2Z12 + ssolntable$R3Z12 + ssolntable$R4Z12 + ssolntable$R5Z12 + ssolntable$R6Z12 + ssolntable$R7Z12 + ssolntable$R8Z12 + ssolntable$R9Z12 + ssolntable$R10Z12 }
    if (iZones > 12) { ssolntable$Z13 <- ssolntable$R1Z13 + ssolntable$R2Z13 + ssolntable$R3Z13 + ssolntable$R4Z13 + ssolntable$R5Z13 + ssolntable$R6Z13 + ssolntable$R7Z13 + ssolntable$R8Z13 + ssolntable$R9Z13 + ssolntable$R10Z13 }
    if (iZones > 13) { ssolntable$Z14 <- ssolntable$R1Z14 + ssolntable$R2Z14 + ssolntable$R3Z14 + ssolntable$R4Z14 + ssolntable$R5Z14 + ssolntable$R6Z14 + ssolntable$R7Z14 + ssolntable$R8Z14 + ssolntable$R9Z14 + ssolntable$R10Z14 }
    if (iZones > 14) { ssolntable$Z15 <- ssolntable$R1Z15 + ssolntable$R2Z15 + ssolntable$R3Z15 + ssolntable$R4Z15 + ssolntable$R5Z15 + ssolntable$R6Z15 + ssolntable$R7Z15 + ssolntable$R8Z15 + ssolntable$R9Z15 + ssolntable$R10Z15 }
    if (iZones > 15) { ssolntable$Z16 <- ssolntable$R1Z16 + ssolntable$R2Z16 + ssolntable$R3Z16 + ssolntable$R4Z16 + ssolntable$R5Z16 + ssolntable$R6Z16 + ssolntable$R7Z16 + ssolntable$R8Z16 + ssolntable$R9Z16 + ssolntable$R10Z16 }
    if (iZones > 16) { ssolntable$Z17 <- ssolntable$R1Z17 + ssolntable$R2Z17 + ssolntable$R3Z17 + ssolntable$R4Z17 + ssolntable$R5Z17 + ssolntable$R6Z17 + ssolntable$R7Z17 + ssolntable$R8Z17 + ssolntable$R9Z17 + ssolntable$R10Z17 }
    if (iZones > 17) { ssolntable$Z18 <- ssolntable$R1Z18 + ssolntable$R2Z18 + ssolntable$R3Z18 + ssolntable$R4Z18 + ssolntable$R5Z18 + ssolntable$R6Z18 + ssolntable$R7Z18 + ssolntable$R8Z18 + ssolntable$R9Z18 + ssolntable$R10Z18 }
    if (iZones > 18) { ssolntable$Z19 <- ssolntable$R1Z19 + ssolntable$R2Z19 + ssolntable$R3Z19 + ssolntable$R4Z19 + ssolntable$R5Z19 + ssolntable$R6Z19 + ssolntable$R7Z19 + ssolntable$R8Z19 + ssolntable$R9Z19 + ssolntable$R10Z19 }
    if (iZones > 19) { ssolntable$Z20 <- ssolntable$R1Z20 + ssolntable$R2Z20 + ssolntable$R3Z20 + ssolntable$R4Z20 + ssolntable$R5Z20 + ssolntable$R6Z20 + ssolntable$R7Z20 + ssolntable$R8Z20 + ssolntable$R9Z20 + ssolntable$R10Z20 }
    ssolntable <- sqldf("SELECT PUID, number, Z1, Z2, Z3 from ssolntable")
    colnames(ssolntable)[1] <- sPUName
    for (k in 1:iZones)
    {
        colnames(ssolntable)[2+k] <- ZoneNames[k]
    }
    #colnames(ssolntable)[3] <- sZ1Name
    #colnames(ssolntable)[4] <- sZ2Name
    #colnames(ssolntable)[5] <- sZ3Name
    write.csv(ssolntable,
                  paste0(sMarxanDir,"/output/output_ssoln.csv"),
                  quote=FALSE,row.names=FALSE)

    # join cluster files: text parse
    outfile <- file(paste0(sMarxanDir,"/output/output_solutionsmatrix.csv"),"w")
    iRow <- 0
    for (i in 1:iCores)
    {
        infile <- file(paste0(sMarxanDir,"/output/output",i,"_solutionsmatrix.csv"),"r")
        # read header row
        sLine <- readLines(con=infile,n=1)
  
        # write header row if i == 1
        if (i == 1)
        {
            write(sLine,file=outfile)
        }
    
        for (j in 1:(iZones*iRepsPerCore))
        {
            sLine <- readLines(con=infile,n=1)
            write(sLine,file=outfile,append=TRUE)
        }
        close(infile)
    }
    close(outfile)
    # load the joined cluster file and fix the row names
    solutionsmatrix <- read.csv(paste0(sMarxanDir,"/output/output_solutionsmatrix.csv"))
    solutionsmatrix$SolutionsMatrix <- as.character(solutionsmatrix$SolutionsMatrix)
    iInc <- 0
    for (i in 1:iSolutions)
    {
      for (j in 1:iZones)
      {
         iInc <- iInc + 1
         solutionsmatrix$SolutionsMatrix[iInc] <- paste0("Z",j,"S",i)
      }
    }
    write.csv(solutionsmatrix,paste0(sMarxanDir,"/output/output_solutionsmatrix.csv"),quote=FALSE,row.names=FALSE)
}

JoinParallelResults <- function(sMarxanDir,iCores,iRepsPerCore)
{
  iSolutions <- round(iCores*iRepsPerCore)
  # combine the summary tables
  sumtable <- c()
  for (i in 1:iCores)
  {
    sumtable_ <- read.csv(paste0(sMarxanDir,"/output/output",i,"_sum.csv"))
    sumtable <- rbind(sumtable,sumtable_)
  }
  for (i in 1:iSolutions)
  {
    sumtable[i,1] <- i
  }
  write.csv(sumtable,
            paste0(sMarxanDir,"/output/output_sum.csv"),
            quote=FALSE,row.names=FALSE)

  # detect best solution
  iBest <- which(sumtable$Score==min(sumtable$Score))
  if (length(iBest) > 0)
  {
    iBest <- iBest[1]
  }

  # rename mv files and solution files
  iSol <- 0
  for (i in 1:iCores)
  {
    for (j in 1:iRepsPerCore)
    {
      iSol <- iSol + 1

      file.rename(paste0(sMarxanDir,"/output/output",i,"_mv",PadInt(j),".csv"),
                  paste0(sMarxanDir,"/output/output_mv",PadInt(iSol),".csv"))

      file.rename(paste0(sMarxanDir,"/output/output",i,"_r",PadInt(j),".csv"),
                  paste0(sMarxanDir,"/output/output_r",PadInt(iSol),".csv"))
    }
  }

  # copy _mvbest and _best files
  file.copy(paste0(sMarxanDir,"/output/output_mv",PadInt(iBest),".csv"),
            paste0(sMarxanDir,"/output/output_mvbest.csv"),
            overwrite=TRUE)
  file.copy(paste0(sMarxanDir,"/output/output_r",PadInt(iBest),".csv"),
            paste0(sMarxanDir,"/output/output_best.csv"),
            overwrite=TRUE)

  # join ssoln files
  ssolntable <- read.csv(paste0(sMarxanDir,"/output/output",i,"_ssoln.csv"))
  colnames(ssolntable)[2] <- "numberX"
  for (i in 2:iCores)
  {
    ssolntable_ <- read.csv(paste0(sMarxanDir,"/output/output",i,"_ssoln.csv"))
    ssolntable <- sqldf("SELECT * from ssolntable LEFT JOIN ssolntable_ USING(planning_unit)")
    ssolntable$numberX <- ssolntable$numberX + ssolntable$number
    ssolntable <- sqldf("SELECT planning_unit, numberX from ssolntable")
  }
  colnames(ssolntable)[2] <- "number"
  write.csv(ssolntable,
            paste0(sMarxanDir,"/output/output_ssoln.csv"),
            quote=FALSE,row.names=FALSE)

  # join cluster files: text parse
  outfile <- file(paste0(sMarxanDir,"/output/output_solutionsmatrix.csv"),"w")
  iRow <- 0
  for (i in 1:iCores)
  {
    infile <- file(paste0(sMarxanDir,"/output/output",i,"_solutionsmatrix.csv"),"r")
    # read header row
    sLine <- readLines(con=infile,n=1)

    # write header row if i == 1
    if (i == 1)
    {
      write(sLine,file=outfile)
    }

    for (j in 1:iRepsPerCore)
    {
      sLine <- readLines(con=infile,n=1)
      iLen <- nchar(sLine)
      if (j < iRepsPerCore)
      {
        # S1..S9 : remove 3 chars
        sLine <- substr(sLine, 4, iLen)
      } else {
        # S10 : remove 4 chars
        sLine <- substr(sLine, 5, iLen)
      }
      iRow <- iRow + 1
      write(paste0("S",iRow,",",sLine),file=outfile,append=TRUE)
    }
    close(infile)
  }
  close(outfile)
}

ImportOutputsCsvToShpDbf_MarZone <- function(sPuShapeFileDbf, sMarxanDir, iNumberOfRuns, iNumberOfZones, sPUID)
# Imports the relevant contents of output files to the planning unit shape file dbf.
{
  # load and prepare pu_table
  pu_table <- read.dbf(sPuShapeFileDbf)
  pu_table <- sqldf(paste0("SELECT ", sPUID, " from pu_table"))
  colnames(pu_table)[1] <- "PUID"
                    
  pu_table$PUID <- as.integer(pu_table$PUID)
  
  # load and prepare ssoln_table
  ssoln_table <- read.csv(paste0(sMarxanDir,"/output/output_ssoln",GetOutputFileext(sMarxanDir,"SAVESUMSOLN")))
  colnames(ssoln_table)[1] <- "PUID"
  
  if (iNumberOfZones > 2)
  {
    # read in the the SSOLN fields for multiple zones
    # "SELECT PUID, SSOLN1, SSOLN2, SSOLN3 from ssoln_table"
    sSelectSQL <- "SELECT PUID"
    for (i in 1:iNumberOfZones)
    {
      colnames(ssoln_table)[i+2] <- paste0("SSOLN",i)
      sSelectSQL <- paste0(sSelectSQL,", SSOLN",i)
    }
    sSelectSQL <- paste0(sSelectSQL," from ssoln_table")
    
    ssoln_table <- sqldf(sSelectSQL)
  } else {
    colnames(ssoln_table)[2] <- "SSOLN2"
    ssoln_table$SSOLN1 <- as.integer(iNumberOfRuns - ssoln_table$SSOLN2)
    ssoln_table$SSOLN2 <- as.integer(ssoln_table$SSOLN2)    
  }
  
  # join pu_table and ssoln_table
  pu_table <- sqldf("SELECT * from pu_table LEFT JOIN ssoln_table USING(PUID)")
  
  # load and prepare best_table
  best_table <- read.csv(paste0(sMarxanDir,"/output/output_best",GetOutputFileext(sMarxanDir,"SAVEBEST")))
  if (iNumberOfZones > 2)
  {
    # the field has a different field name in MarZone: "zone" instead of "SOLUTION"
    colnames(best_table)[1] <- "PUID"
    colnames(best_table)[2] <- "SOLUTION"
    best_table$BESTSOLN <- as.integer(best_table$SOLUTION)
  } else {
    best_table$BESTSOLN <- as.integer(best_table$SOLUTION + 1)
  }
  best_table <- sqldf("SELECT PUID, BESTSOLN from best_table")
  
  # join pu_table and best_table
  pu_table <- sqldf("SELECT * from pu_table LEFT JOIN best_table USING(PUID)")
  
  for (i in 1:iNumberOfRuns)
  {
    sFieldName <- paste0("SOLN",i)
    
    # load and prepare solnX_table
    solnX_table <- read.csv(GenerateSolnFilename(i,sMarxanDir))
    if (iNumberOfZones > 2)
    {
      # the field has a different field name in MarZone: "zone" instead of "SOLUTION"
      colnames(solnX_table)[1] <- "PUID"
      colnames(solnX_table)[2] <- "SOLUTION"
      solnX_table[sFieldName] <- as.integer(solnX_table$SOLUTION)
    } else {
      solnX_table[sFieldName] <- as.integer(solnX_table$SOLUTION + 1)
    }
    solnX_table <- sqldf(paste0("SELECT PUID, ",sFieldName," from solnX_table"))
  
    # join pu_table and solnX_table
    pu_table <- sqldf("SELECT * from pu_table LEFT JOIN solnX_table USING(PUID)")
    
    rm(solnX_table)
  }
  
  # save the new pu_table
  colnames(pu_table)[1] <- sPUID
  write.dbf(pu_table,sPuShapeFileDbf)  
}

ImportOutputsCsvToShpDbf <- function(sPuShapeFileDbf, sMarxanDir, iNumberOfRuns, sPUID)
{
  # Imports the relevant contents of output files to the planning unit shape file dbf.
  # load and prepare pu_table
  pu_table <- read.dbf(sPuShapeFileDbf)
  pu_table <- sqldf(paste("SELECT ", sPUID, " from pu_table",sep=""))
  colnames(pu_table)[1] <- "PUID"

  pu_table$PUID <- as.integer(pu_table$PUID)

  # load and prepare ssoln_table
  ssoln_table <- read.csv(paste(sMarxanDir,"/output/output_ssoln",GetOutputFileext(sMarxanDir,"SAVESUMSOLN"),sep=""))
  colnames(ssoln_table)[1] <- "PUID"
  colnames(ssoln_table)[2] <- "SSOLN2"
  ssoln_table$SSOLN1 <- as.integer(iNumberOfRuns - ssoln_table$SSOLN2)
  ssoln_table$SSOLN2 <- as.integer(ssoln_table$SSOLN2)

  # join pu_table and ssoln_table
  pu_table <- sqldf("SELECT * from pu_table LEFT JOIN ssoln_table USING(PUID)")

  # load and prepare best_table
  best_table <- read.csv(paste(sMarxanDir,"/output/output_best",GetOutputFileext(sMarxanDir,"SAVEBEST"),sep=""))
  best_table$BESTSOLN <- as.integer(best_table$SOLUTION + 1)
  best_table <- sqldf("SELECT PUID, BESTSOLN from best_table")

  # join pu_table and best_table
  pu_table <- sqldf("SELECT * from pu_table LEFT JOIN best_table USING(PUID)")

  # save the new pu_table
  colnames(pu_table)[1] <- sPUID
  write.dbf(pu_table,sPuShapeFileDbf)
}

labelCol <- function(x)
{
  # we set iBest as a global in PrepareCluster_compute before calling labelCol
  if (is.leaf(x))
  {
    ## fetch label
    a <- attributes(x)
    label <- attr(x, "label")
    colour <- "black"
    if (label == paste0("S",iBest," (Best)")) { colour <- "blue" }
    attr(x, "nodePar") <- c(a$nodePar, lab.col = colour)
  }
  return(x)
}

labelCol_MarZone <- function(x)
{
  if (is.leaf(x))
  {
    a <- attributes(x)
    label <- attr(x, "label") 
    colour <- "black"
    colours <- rainbow(iZones)
    for (k in 1:iZones)
    {
        if (substring(label,1,2) == paste0("Z",k)) { colour <- colours[k] }
    }
    #if (substring(label,1,2) == "Z2") { colour <- "green" }
    #if (substring(label,1,2) == "Z3") { colour <- "blue" }
    attr(x, "nodePar") <- c(a$nodePar, lab.col = colour)
  }
  return(x)
}

PrepareCluster_compute_MarZone <- function(sMarxanDir)
{
  # NOTE: we fail gracefully if there are not enough unique solutions
  # prepare the cluster analysis objects
  solutions_raw<-read.table(paste0(sMarxanDir,"/output/output_solutionsmatrix.csv"),header=TRUE, row.name=1, sep=",")

  solutions <- unique(solutions_raw)
  iUniqueSolutions <- dim(solutions)[1]
  if (iUniqueSolutions > 2)
  {
    # render the 2d
    nmdscolours <- rep("black",each = iUniqueSolutions)
    nmdscolours[1] <- "blue"
    soldist<-vegdist(solutions,distance="jaccard")
    sol.mds<-nmds(soldist,2)
    h<-hclust(soldist, method="complete")

    # render the dendogram
    d <- dendrapply(as.dendrogram(h), labelCol)

    # render the 3d
    #sol3d.mds <- nmds(soldist,3)
    sol3d.mds <- NA
  } else {
    sol.mds <- NA
    plotlabels <- NA
    nmdscolours <- NA
    d <- NA
    sol3d.mds <- NA
  }
  sRdata <- paste0(sMarxanDir,"/output/cluster.Rdata")
  save(sol.mds,plotlabels,nmdscolours,d,sol3d.mds,file=sRdata)
}

PrepareCluster_compute <- function(sMarxanDir)
{
  # NOTE: we fail gracefully if there are not enough unique solutions
  # prepare the cluster analysis objects
  solutions_raw<-read.table(paste0(sMarxanDir,"/output/output_solutionsmatrix.csv"),header=TRUE, row.name=1, sep=",")
  thetable <- read.csv(paste0(sMarxanDir,"/output/output_sum.csv"))
  iBest <<- which.min(thetable$Score)
  Best <- solutions_raw[iBest,]
  solutions_raw <- solutions_raw[-iBest,]
  solutions_join <- rbind(Best,solutions_raw)

  rownames(solutions_join) <- c(paste0("S",iBest," (Best)"),row.names(solutions_raw))
  plotlabels <- c(paste0("S",iBest," (Best)"),row.names(solutions_raw))

  solutions <- unique(solutions_join)
  iUniqueSolutions <- dim(solutions)[1]
  if (iUniqueSolutions > 2)
  {
    # render the 2d
    nmdscolours <- rep("black",each = iUniqueSolutions)
    nmdscolours[1] <- "blue"
    soldist<-vegdist(solutions,distance="jaccard")
    sol.mds<-nmds(soldist,2)
    h<-hclust(soldist, method="complete")

    # render the dendogram
    d <- dendrapply(as.dendrogram(h), labelCol)

    # render the 3d
    #sol3d.mds <- nmds(soldist,3)
    sol3d.mds <- NA
  } else {
    sol.mds <- NA
    plotlabels <- NA
    nmdscolours <- NA
    d <- NA
    sol3d.mds <- NA
  }
  sRdata <- paste0(sMarxanDir,"/output/cluster.Rdata")
  save(sol.mds,plotlabels,nmdscolours,d,sol3d.mds,file=sRdata)
}

RunMarxan_1st <- function(sDatabasePath,sShinyDataPath,iCores,iRepsPerCore)
{
  if (.Platform$pkgType == "source")
  {
      sExecutable <<- "MarOpt_v243_Linux64"
  } else {
      sExecutable <<- "MarOpt_v243_Mac64"
  }

  # copy the executable to the Marxan path
  file.copy(paste0(sShinyDataPath,"/",sExecutable),paste0(sDatabasePath,"/",sExecutable))
  system(paste0("chmod +x ",sDatabasePath,"/",sExecutable))

  withProgress(message="Run Marxan",value=0,
  {
    withProgress(message="Marxan",value=0,
    {
      RunMarxan(sDatabasePath,sExecutable,iCores,iRepsPerCore)
    })
    withProgress(message="Merge results",value=0,
    {
      JoinParallelResults(sDatabasePath,iCores,iRepsPerCore)
    })
    withProgress(message="Populate dbf",value=0,
    {
      ImportOutputsCsvToShpDbf(paste0(sDatabasePath,"/pulayer/pulayer.dbf"),sDatabasePath,round(iCores*iRepsPerCore),"PUID")
    })

    withProgress(message="Prepare cluster",value=0,
    {
      PrepareCluster_compute(sDatabasePath)
    })
  })
}

RunMarZone_1st <- function(sDatabasePath,sShinyDataPath,iCores,iRepsPerCore)
{
  if (.Platform$pkgType == "source")
  {
      sExecutable <<- "MarZone_v201_Linux64"
  } else {
      sExecutable <<- "MarZone_v201_Mac64"
  }

  # copy the executable to the Marxan path
  file.copy(paste0(sShinyDataPath,"/",sExecutable),paste0(sDatabasePath,"/",sExecutable))
  system(paste0("chmod +x ",sDatabasePath,"/",sExecutable))

  RunMarxan(sDatabasePath,sExecutable,iCores,iRepsPerCore)
  JoinParallelResults_MarZone(sDatabasePath,iCores,iRepsPerCore,iZones)
  ImportOutputsCsvToShpDbf_MarZone(paste0(sDatabasePath,"/pulayer/pulayer.dbf"),sDatabasePath,round(iCores*iRepsPerCore),"PUID",iZones)

  PrepareCluster_compute_MarZone(sDatabasePath)
}

list.dirs <- function(path=".", pattern=NULL, all.dirs=FALSE,
                      full.names=FALSE, ignore.case=FALSE)
{
  # use full.names=TRUE to pass to file.info
  all <- list.files(path, pattern, all.dirs,
                    full.names=TRUE, recursive=FALSE, ignore.case)
  dirs <- all[file.info(all)$isdir]
  # determine whether to return full names or just dir names
  if(isTRUE(full.names))
    return(dirs)
  else
    return(basename(dirs))
}

SafeDbName <- function(sDbName,sShinyUserPath,sUserName)
{
  # sDbName is the name the user wants to use
  # if it conflicts with another existing database, automatically fix it
  UserDb <- list.dirs(paste0(sShinyUserPath,"/",sUserName))
  sFixName <- sDbName

  iMatch <- grep(paste0("^",sDbName,"$"),UserDb)
  if (length(iMatch) > 0)
  {
    # database exists, generate a replacement name
    for (i in 1:1000)
    {
      sFixName <- sprintf("%s%s",sDbName,round(runif(1)*1000))
      iMatch <- grep(paste0("^",sFixName,"$"),UserDb)
      if (length(iMatch) == 0)
      {
        break()
      }
    }
  }
  return(sFixName)
}
