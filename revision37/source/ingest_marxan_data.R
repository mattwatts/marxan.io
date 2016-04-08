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
        
        iCountPU <<- 0
        iCountSpec <<- 0
        iCountBound <<- 0
        iCountMatrix <<- 0
        iCountZone <<- 0
        iCountPolygon <<- 0

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

                write(paste0("dos2unix Marxan ",date()),file=sLogFile,append=TRUE)

                system(paste0("dos2unix ",sPuDat))
                write(paste0("dos2unix pu.dat ok ",date()),file=sLogFile,append=TRUE)
                system(paste0("dos2unix ",sSpecDat))
                write(paste0("dos2unix spec.dat ok ",date()),file=sLogFile,append=TRUE)
                system(paste0("dos2unix ",sPuvsprDat))
                write(paste0("dos2unix puvsp.dat ok ",date()),file=sLogFile,append=TRUE)
                if (length(sBoundDat) == 1)
                {
                    system(paste0("dos2unix ",sBoundDat))
                    write(paste0("dos2unix bound.dat ",date()),file=sLogFile,append=TRUE)
                }
                if (fMarZone)
                {
                    incProgress(0,detail="converting MarZone files")

                    write(paste0("dos2unix MarZone ",date()),file=sLogFile,append=TRUE)

                    system(paste0("dos2unix ",sZonesDat))
                    write(paste0("dos2unix zones.dat ok ",date()),file=sLogFile,append=TRUE)
                    system(paste0("dos2unix ",sCostsDat))
                    write(paste0("dos2unix costs.dat ok ",date()),file=sLogFile,append=TRUE)
                    system(paste0("dos2unix ",sZoneCostDat))
                    write(paste0("dos2unix zonecost.dat ok ",date()),file=sLogFile,append=TRUE)
                    if (fZoneBoundCost)
                    {
                        write(paste0("dos2unix zoneboundcost.dat trying ",date()),file=sLogFile,append=TRUE)
                        system(paste0("dos2unix ",sZoneBoundCostDat))
                        write(paste0("dos2unix zoneboundcost.dat ok ",date()),file=sLogFile,append=TRUE)
                    }
                    if (fZoneContrib)
                    {
                        write(paste0("dos2unix zonecontrib.dat trying ",date()),file=sLogFile,append=TRUE)
                        system(paste0("dos2unix ",sZoneContribDat))
                        write(paste0("dos2unix zonecontrib.dat ok ",date()),file=sLogFile,append=TRUE)
                    }
                    if (fZoneContrib2)
                    {
                        write(paste0("dos2unix zonecontrib2.dat trying >",sZoneContrib2Dat,"< ",date()),file=sLogFile,append=TRUE)
                        system(paste0("dos2unix ",sZoneContrib2Dat))
                        write(paste0("dos2unix zonecontrib2.dat ok ",date()),file=sLogFile,append=TRUE)
                    }
                    if (fZoneContrib3)
                    {
                        write(paste0("dos2unix zonecontrib3.dat trying ",date()),file=sLogFile,append=TRUE)
                        system(paste0("dos2unix ",sZoneContrib3Dat))
                        write(paste0("dos2unix zonecontrib3.dat ok ",date()),file=sLogFile,append=TRUE)
                    }
                    if (fZoneTarget)
                    {
                        write(paste0("dos2unix zonetarget.dat trying ",date()),file=sLogFile,append=TRUE)
                        system(paste0("dos2unix ",sZoneTargetDat))
                        write(paste0("dos2unix zonetarget.dat ok ",date()),file=sLogFile,append=TRUE)
                    }
                    if (fZoneTarget2)
                    {
                        write(paste0("dos2unix zonetarget2.dat trying ",date()),file=sLogFile,append=TRUE)
                        system(paste0("dos2unix ",sZoneTarget2Dat))
                        write(paste0("dos2unix zonetarget2.dat ok ",date()),file=sLogFile,append=TRUE)
                    }
                    if (fPuLock)
                    {
                        write(paste0("dos2unix pulock.dat trying ",date()),file=sLogFile,append=TRUE)
                        system(paste0("dos2unix ",sPuLockDat))
                        write(paste0("dos2unix pulock.dat ok ",date()),file=sLogFile,append=TRUE)
                    }
                    if (fPuZone)
                    {
                        write(paste0("dos2unix puzone.dat trying ",date()),file=sLogFile,append=TRUE)
                        system(paste0("dos2unix ",sPuZoneDat))
                        write(paste0("dos2unix puzone.dat ok ",date()),file=sLogFile,append=TRUE)
                    }
                }
            }

            write(paste0("reading input files ",date()),file=sLogFile,append=TRUE)
            cat("reading input files\n")

            # read the default input.dat
            inputdat <- readLines(paste0(sDataPath,"/no_bound_input.dat"))

            # read the input files
            incProgress(0,detail="reading Marxan files")
          
            write(paste0("reading pu.dat ",date()),file=sLogFile,append=TRUE)
            pu.dat <- smart_read(sPuDat)
            iCountPU <<- nrow(pu.dat)
            write(paste0("reading spec.dat ",date()),file=sLogFile,append=TRUE)
            spec.dat <- smart_read(sSpecDat)
            iCountSpec <<- nrow(spec.dat)
            write(paste0("reading puvsp.dat ",date()),file=sLogFile,append=TRUE)
            puvspr.dat <- smart_read(sPuvsprDat)
            iCountMatrix <<- nrow(puvspr.dat)
            fBoundDat <- FALSE
            if (length(sBoundDat) == 1)
            {
                fBoundDat <- TRUE
                write(paste0("reading bound.dat ",date()),file=sLogFile,append=TRUE)
                bound.dat <- smart_read(sBoundDat)
                iCountBound <<- nrow(bound.dat)
            }
            write(paste0("marxan files ok ",date()),file=sLogFile,append=TRUE)

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
                inputdat <- c(inputdat,"AVAILABLEZONE 1")
                inputdat <- c(inputdat,"ZONESNAME zones.dat")
                inputdat <- c(inputdat,"COSTSNAME costs.dat")
                inputdat <- c(inputdat,"ZONECOSTNAME zonecost.dat")

                incProgress(0,detail="reading MarZone files")
          
                zones.dat <- smart_read(sZonesDat)
                iCountZone <<- nrow(zones.dat)
                costs.dat <- smart_read(sCostsDat)
                iCountCost <<- nrow(costs.dat)
                zonecost.dat <- smart_read(sZoneCostDat)
            
                # process zone names in zones.dat : replace space with underscore
                ZoneNames <<- as.character(unlist(zones.dat$zonename))
                for (i in 1:length(ZoneNames))
                {
                    if (grepl(" ",ZoneNames[i]))
                    {
                        ZoneNames[i] <- paste(strsplit(ZoneNames[i],split=" ")[[1]],collapse="_")
                    }
                }
                zones.dat$zonename <- ZoneNames

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
            })

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
            })

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
                iCountPolygon <<- nrow(putable)
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
            })

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
            })

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
            })

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
            })

            incProgress(0,detail="converting shapefile")

            tryCatch(
            {
                # create the pulayer.Rdata file
                if (!fMarZone) # marzone doesn't use status
                {
                    pustatus_ <- unlist(pu.dat$status)
                }
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
            })

            incProgress(0,detail="saving pulayer.Rdata")

            tryCatch(
            {
                sRdata <- paste0(sOutDir,"/pulayer/pulayer.Rdata")
                if (fMarZone) # marzone doesn't use status
                {
                    save(fOutline,puoutline,pulayer_,x_,y_,file=sRdata)
                } else {
                    save(fOutline,puoutline,pulayer_,pustatus_,x_,y_,file=sRdata)
                }
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
            })

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
