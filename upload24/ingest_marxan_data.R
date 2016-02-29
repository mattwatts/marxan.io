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
  sPath <- sTempPath

  sLogFile <- paste0(sPath,"/ParseMarxanZip.log")
  WarningMsg <- c()
  ErrorMsg <- c()

  write(paste0("ParseMarxanZip log start ",date()),file=sLogFile)
  write(paste0("temp path ",sPath," ",date()),file=sLogFile,append=TRUE)

  cat(paste0("temp path ",sPath,"\n"))

  # sPath is the temporary directory
  cat(paste0(">",sInputZipFile,"< >",paste0(sPath,"/data.zip"),"<\n"))
  file.copy(sInputZipFile,paste0(sPath,"/data.zip"))
  cat(paste0("copy done\n"))
  system(paste0("unzip ",sInputZipFile," -d ",sPath))
  cat(paste0("unzip done\n"))

  # create Marxan directories
  dir.create(paste0(sPath,"/marxan"))
  dir.create(paste0(sPath,"/marxan/input"))
  dir.create(paste0(sPath,"/marxan/output"))
  dir.create(paste0(sPath,"/marxan/pulayer"))

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

  sPuDat <- list.files(path = sPath, recursive = TRUE, pattern = sPUNAME, ignore.case = TRUE, full.names = TRUE)
  sSpecDat <- list.files(path = sPath, recursive = TRUE, pattern = sSPECNAME, ignore.case = TRUE, full.names = TRUE)
  sPuvsprDat <- list.files(path = sPath, recursive = TRUE, pattern = sPUVSPRNAME, ignore.case = TRUE, full.names = TRUE)
  sBoundDat <- list.files(path = sPath, recursive = TRUE, pattern = sBOUNDNAME, ignore.case = TRUE, full.names = TRUE)

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
    system(paste0("dos2unix ",sPuDat))
    system(paste0("dos2unix ",sSpecDat))
    system(paste0("dos2unix ",sPuvsprDat))
    if (length(sBoundDat) == 1)
    {
      system(paste0("dos2unix ",sBoundDat))
    }
  }

  write(paste0("reading input files ",date()),file=sLogFile,append=TRUE)
  cat("reading input files\n")

  # are the files CSV, Tab, or comma delimeted?
  pu.dat <- smart_read(sPuDat)
  spec.dat <- smart_read(sSpecDat)
  puvspr.dat <- smart_read(sPuvsprDat)
  if (length(sBoundDat) == 1)
  {
    bound.dat <- smart_read(sBoundDat)
  }

  write(paste0("input files read ",date()),file=sLogFile,append=TRUE)
  cat("input files read\n")

  write.csv(pu.dat,paste0(sPath,"/marxan/input/pu.dat"),quote=FALSE,row.names=FALSE)
  # if spec.dat has a name field, make the name field the last field
  # this will reduce possible errors where users include delimeter characters in the feature names
  cnames <- colnames(spec.dat)
  iName <- which(grepl("name",cnames))
  if (length(iName) > 0)
  {
    spec.dat <- spec.dat[c(cnames[-iName],"name")]
  }
  write.csv(spec.dat,paste0(sPath,"/marxan/input/spec.dat"),quote=FALSE,row.names=FALSE)
  if (length(sBoundDat) == 1)
  {
    write.csv(bound.dat,paste0(sPath,"/marxan/input/bound.dat"),quote=FALSE,row.names=FALSE)
  }

  if (ncol(puvspr.dat) == 3)
  {
    # This is a sparse matrix. Ensure it is sorted in the correct order.
    puorder.dat <- puvspr.dat[order(puvspr.dat$pu),]
    write.csv(puorder.dat,paste0(sPath,"/marxan/input/puorder.dat"),quote=FALSE,row.names=FALSE)
    sporder.dat <- puvspr.dat[order(puvspr.dat$species),]
    write.csv(sporder.dat,paste0(sPath,"/marxan/input/sporder.dat"),quote=FALSE,row.names=FALSE)

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
    sPuOrder <- paste0(sPath,"/marxan/input/puorder.dat")
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
    sSpOrder <- paste0(sPath,"/marxan/input/sporder.dat")
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
    # Warn user their matrix wasn't in the correct format.
    # warning message but not critical
    sWarningMsg <- paste0("puvspr.dat file ",sPuvsprDat," was not in sparse matrix format ")
    write(paste0(sWarningMsg," ",date()),file=sLogFile,append=TRUE)
    WarningMsg <- c(WarningMsg,sWarningMsg)
    cat(paste0(sWarningMsg,"\n"))
  }

  # save the input.dat
  if (length(sBoundDat) == 1)
  {
    inputdat <- readLines(paste0(sDataPath,"/input.dat"))
  } else {
    inputdat <- readLines(paste0(sDataPath,"/no_bound_input.dat"))
  }

  writeLines(inputdat,con=paste0(sPath,"/marxan/input.dat"))

  write(paste0("marxan files processed ",date()),file=sLogFile,append=TRUE)
  cat("marxan files processed\n")

  # find shapefiles
  ShapeFiles <- list.files(path = sPath, recursive = TRUE, pattern = "*.shp", ignore.case = TRUE, full.names = TRUE)
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

  # load the planning unit shapefile
  pushapefile <- readOGR(dirname(sPuLayer),file_path_sans_ext(basename(sPuLayer)))

  writeOGR(pushapefile,paste0(sPath,"/marxan/pulayer"),"pulayer",driver="ESRI Shapefile",overwrite_layer=TRUE)

  write(paste0("pulayer read ",sPuLayer," ",date()),file=sLogFile,append=TRUE)
  cat(paste0("pulayer read ",sPuLayer,"\n"))

  putable <- read.dbf(paste0(dirname(sPuLayer),"/",file_path_sans_ext(basename(sPuLayer)),".dbf"))
  dim(putable)
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
  putable <- sqldf("SELECT PUID FROM putable")
  dim(putable)

  write.dbf(putable,paste0(sPath,"/marxan/pulayer/pulayer.dbf"))

  # load the outline shapefile
  if (sOutlineLayer != "")
  {
    outlineshapefile <- readOGR(dirname(sOutlineLayer),file_path_sans_ext(basename(sOutlineLayer)))

    write(paste0("outline loaded ",sOutlineLayer," ",date()),file=sLogFile,append=TRUE)
    cat(paste0("outline loaded ",sOutlineLayer,"\n"))

    writeOGR(outlineshapefile,paste0(sPath,"/marxan/pulayer"),"puoutline",driver="ESRI Shapefile",overwrite_layer=TRUE)
    puoutline <- SpatialPolygons2PolySet(outlineshapefile)
    fOutline <- TRUE
  } else {
    fOutline <- FALSE
    puoutline <- NA
    # Warn user no outline layer.
    # warning message but not critical
    sWarningMsg <- "Warning: no outline layer."
    write(paste0(sWarningMsg," ",date()),file=sLogFile,append=TRUE)
    WarningMsg <- c(WarningMsg,sWarningMsg)
    cat(paste0(sWarningMsg,"\n"))
  }

  write(paste0("shapefiles processed ",date()),file=sLogFile,append=TRUE)
  cat("shapefiles processed\n")

  # create the pulayer.Rdata file
  pustatus_ <- unlist(pu.dat$status)
  pulayer_ <<- SpatialPolygons2PolySet(pushapefile)
  y_ <- bbox(pushapefile)[2,2] - bbox(pushapefile)[2,1]
  x_ <- bbox(pushapefile)[1,2] - bbox(pushapefile)[1,1]
  sRdata <- paste0(sPath,"/marxan/pulayer/pulayer.Rdata")
  save(fOutline,puoutline,pulayer_,pustatus_,x_,y_,file=sRdata)

  write(paste0("pulayer.Rdata created ",sRdata," ",date()),file=sLogFile,append=TRUE)
  cat(paste0("pulayer.Rdata created ",sRdata,"\n"))

  write(paste0("ParseMarxanZip log end ",date()),file=sLogFile,append=TRUE)
  cat(paste0("ParseMarxanZip end\n"))

  return(c(sPath,sLogFile,WarningMsg,ErrorMsg))
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
  dir.create(paste0(sShinyUserPath,"/",sUserName,"/",sDatabaseName))
  system(paste0("cp -r ",sPath,"/marxan/ ",sShinyUserPath,"/",sUserName,"/",sDatabaseName))
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
    file.copy(paste0(sMarxanDir,"/",sExecutable),paste0(sMarxanDir,"/core",i,"/",sExecutable))
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
    soldist<-vegdist(solutions,distance="bray")
    sol.mds<-nmds(soldist,2)
    h<-hclust(soldist, method="complete")

    # render the dendogram
    d <- dendrapply(as.dendrogram(h), labelCol)

    # render the 3d
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

  RunMarxan(sDatabasePath,sExecutable,iCores,iRepsPerCore)
  JoinParallelResults(sDatabasePath,iCores,iRepsPerCore)
  ImportOutputsCsvToShpDbf(paste0(sDatabasePath,"/pulayer/pulayer.dbf"),sDatabasePath,round(iCores*iRepsPerCore),"PUID")

  PrepareCluster_compute(sDatabasePath)
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
