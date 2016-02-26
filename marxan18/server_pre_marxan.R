# marxan.io

# init some variables
rprop <<- 0.3
iM <<- 1
fMarxanRunning <<- FALSE
sUserInterface <<- "Select Database"
if (.Platform$pkgType == "source")
{
    sExecutable <<- "MarOpt_v243_Linux64"
} else {
    sExecutable <<- "MarOpt_v243_Mac64"
}
fAllow <<- FALSE

SelectDatabase <- function()
{
    load(file=paste0(sMarxanDir,"/pulayer/pulayer.Rdata"))
    puoutline <<- puoutline
    pulayer_ <<- pulayer_
    pustatus_ <<- pustatus_
    specdat <- read.csv(paste(sMarxanDir,"/input/spec.dat",sep=""),stringsAsFactors=FALSE)
    # if name not present in spec.dat, use id as name
    if("name" %in% colnames(specdat))
    {
        specnames <<- unlist(as.character(specdat$name))
    } else {
        specnames <<- unlist(as.character(specdat$id))
    }
    sfeature <<- "All features"
    # to display the polygon set in the correct aspect ratio
    x__ <<- max(pulayer_$X)-min(pulayer_$X)
    y__ <<- max(pulayer_$Y)-min(pulayer_$Y)
    # read the BLM, prop, SPF ui control values
    inputdat <- readLines(paste(sMarxanDir,"/input.dat",sep=""))
    sParamBLM <- GetParamValue(inputdat,"BLM")
    if (sParamBLM != "")
    {
        sBLM <<- sParamBLM
        cat(paste0("sBLM ",sBLM,"\n"))
    }
}

PrepareDisplay <- function()
{
    cat("PrepareDisplay start\n")

    # prepare the map: pulayer object
    pulayer <<- pulayer_
    pu_table <<- read.dbf(paste(sMarxanDir,"/pulayer/pulayer.dbf",sep=""))

    # prepare the planning unit status object
    pustatus <<- pustatus_
    fExistingReserves <<- (2 %in% pustatus_)
    fExcluded <<- (3 %in% pustatus_)

    # load the cluster analysis objects from file
    load(paste0(sMarxanDir,"/output/cluster.Rdata"))
    sol.mds <<- sol.mds
    nmdscolours <<- nmdscolours
    plotlabels <<- plotlabels
    d <<- d

    cat("PrepareDisplay end\n")
}

GenerateSolnFilename <- function(iRunNumber,sMarxanDir)
{
  sFilename <- paste0(sMarxanDir,"/output/output_r")
  iPad <- 5 - nchar(as.character(iRunNumber))
  sFilename <- paste0(sFilename,paste0(rep("0",iPad),collapse=""))
  sFilename <- paste0(sFilename,iRunNumber,GetOutputFileext(sMarxanDir,"SAVERUN"))
}

map_ssolnNmap <- function()
{
    values <- sqldf(paste("SELECT SSOLN2 from pu_table",sep=""))
    blueramp <- colorRampPalette(c("white","blue"))(5)
    colours <- rep(blueramp[1],nrow(values))
    for (j in 1:nrow(values))
    {
        if (pustatus[j] == 2)
        {
            colours[j] <- "#40E0D0" # Turquoise
        } else {
            if (pustatus[j] == 3)
            {
                colours[j] <- "grey"
            } else {
                if (values[j,] == 0)
                {
                    colours[j] <- "white"
                } else {
                    if (values[j,] < 30)
                    {
                        colours[j] <- blueramp[2]
                    } else {
                        if (values[j,] < 70)
                        {
                            colours[j] <- blueramp[3] 
                        } else {
                            if (values[j,] < 100)
                            {
                                colours[j] <- blueramp[4]
                            } else {
                                colours[j] <- blueramp[5]
                            }
                        }
                    }
                }
            }
        }
    }
    plotPolys(pulayer,col=colours,axes=FALSE,border=NA,cex.lab=0.1,cex.axis=0.1)
}

binary_map <- function(values)
{
    greenramp <- colorRampPalette(c("white","green"))(2)
    colours <- rep(greenramp[1],nrow(values))
    for (j in 1:nrow(values))
    {
        if (pustatus[j] == 2)
        {
            colours[j] <- "#40E0D0" # Turquoise
        } else {
            if (pustatus[j] == 3)
            {
                colours[j] <- "grey"
            } else {
                colours[j] <- greenramp[values[j,]]
            }
        }
    }
    plotPolys(pulayer,col=colours,axes=FALSE,border=NA,cex.lab=0.1,cex.axis=0.1)
}

map_bestmap <- function()
{
    values <- sqldf("SELECT BESTSOLN from pu_table")
    binary_map(values)
}

map_runMmap <- function()
{
    solnX_table <- read.csv(GenerateSolnFilename(iM,sMarxanDir))
    values <- sqldf(paste("SELECT SOLUTION from solnX_table",sep="")) + 1
    binary_map(values)
}

cluster_2ds <- function()
{
    if (is.na(sol.mds))
    {
        plot(1,1)
    }
    else
    {
        plot(sol.mds$points, xlab='', ylab='', main='NMDS of solutions', col=nmdscolours)
        text(sol.mds$points,labels=plotlabels,pos=4, col=nmdscolours)
    }
}
 
cluster_3ds <- function()
{
    if (is.na(sol3d.mds))
    {
        plot(1,1)
    }
    else
    {
        #plot(sol.mds$points, xlab='', ylab='', main='NMDS of solutions', col=nmdscolours)
        #text(sol.mds$points,labels=plotlabels,pos=4, col=nmdscolours)

        # download the Rdata file to users computer
        # open in R on their client computer so they can interact in 3D with it
        # user interacts asymetrically with server because 3D display is running on client computer

        # display the url to the user so they can click on it
        # it will download the 3d file and launch it in R on the client computer
        # colour code the 3d file with objective function score
        # display colours like 3d terrain display in GIS
        # use Linda's cluster algorithm parameters

        # randomly permutate gurobi OR model parameters
        # run Gurobi many times
        # output is like Marxan solutions ~ ssoln
        # map out phase space with dissimilarity


        #sol3d.mds
        plot(1,1)
    }
}

cluster_dendogram <- function()
{
    if (is.na(d))
    {
        plot(1,1)
    }
    else
    {
        return(plot(d, xlab="Solutions", ylab="Disimilarity", main="Bray-Curtis dissimilarity of solutions"))
    }
}

RunMarxan_app <- function()
{
    # BLM parameter
    inputdat <- readLines(paste0(sMarxanDir,"/input.dat"))
    iBLMparam <- which(regexpr("BLM",inputdat)==1)
    inputdat[iBLMparam] <- paste0("BLM ",rblm)
    writeLines(inputdat,paste0(sMarxanDir,"/input.dat"))

    randomseeds <- round(runif(10)*100000)

    RunMarxan(sMarxanDir,sExecutable,iCores,iRepsPerCore)

    JoinParallelResults(sMarxanDir,iCores,iRepsPerCore)

    # write results to pu dbf
    ImportOutputsCsvToShpDbf(paste0(sMarxanDir,"/pulayer/pulayer.dbf"),
                             sMarxanDir,round(iCores*iRepsPerCore),"PUID")

    PrepareCluster_compute(sMarxanDir)
    PrepareDisplay()
}

RunMarxanParamTest_app <- function()
{
    # set min, max, interval for value ramping
    if (swhichparam == "BLM")
    {
        rMinimum <- safe_log(rRampBLMmin)
        rMaximum <- safe_log(rRampBLMmax)
        rInterval <- (rMaximum - rMinimum) / (iCores-1)
        rValue <- rRampBLMmin
    }
    if (swhichparam == "SPF")
    {
        rMinimum <- safe_log(rRampSPFmin)
        rMaximum <- safe_log(rRampSPFmax)
        rInterval <- (rMaximum - rMinimum) / (iCores-1)
        rValue <- rRampSPFmin
    }
    if (swhichparam == "Targ")
    {
        rMinimum <- rtargetmin
        rMaximum <- rtargetmax
        rInterval <- (rMaximum - rMinimum) / (iCores-1)
        rValue <- rtargetmin
    }

    # create the ramped value file
    write(paste0('i,',swhichparam),file=paste0(sMarxanDir,"/",swhichparam,".csv"))
    write(paste0(1,",",rValue),file=paste0(sMarxanDir,"/",swhichparam,".csv"),append=TRUE)       
    for (i in 2:iCores)
    {
        if (swhichparam == "Targ")
        {
            rValue <- rMinimum+((i-1)*rInterval)       # linear ramping for Target
        } else {
            rValue <- exp(rMinimum+((i-1)*rInterval))  # exponential ramping for BLM and SPF
        }
        write(paste0(i,",",rValue),file=paste0(sMarxanDir,"/",swhichparam,".csv"),append=TRUE)
    }
        
    # initialise a value summary file
    if (swhichparam == "BLM")
    {
        write("i,BLM,cost,boundary length",file=sSummary)
    }
    if (swhichparam == "SPF")
    {
        write("i,SPF,cost,shortfall",file=sSummary)
    }
    if (swhichparam == "Targ")
    {
        write('i,Targ,cost',file=sSummary)
    }
                
    # load the ramped value file
    VALUEcsv <- read.csv(paste0(sMarxanDir,"/",swhichparam,".csv"))
                
    foreach(i=1:iCores) %dopar%   
    {
        # copy files to execution directory
        dir.create(paste0(sMarxanDir,"/core",i))
        file.copy(paste0(sMarxanDir,"/",sExecutable),paste0(sMarxanDir,"/core",i,"/",sExecutable))
        system(paste0("chmod +x ",sMarxanDir,"/core",i,"/",sExecutable))

        # read input.dat and edit parameters
        inputdat <- readLines(paste0(sMarxanDir,"/input.dat"))
        iINPUTDIRparam <- which(regexpr("INPUTDIR",inputdat)==1)
        iOUTPUTDIRparam <- which(regexpr("OUTPUTDIR",inputdat)==1)
        iBLMparam <- which(regexpr("BLM",inputdat)==1)
        iSCENNAMEparam <- which(regexpr("SCENNAME",inputdat)==1)
        iNUMREPSparam <- which(regexpr("NUMREPS",inputdat)==1)
        iSPECNAMEparam <- which(regexpr("SPECNAME",inputdat)==1)
        # read spec.dat
        specdat <- read.csv(paste0(sMarxanDir,"/input/spec.dat"))
        if (swhichparam == "BLM")
        {
            inputdat[iBLMparam] <- paste0("BLM ",VALUEcsv[i,2])
            specdat$spf <- ruserspf
            specdat$prop <- rusertarg
        }
        if (swhichparam == "SPF")
        {
            inputdat[iBLMparam] <- paste0("BLM ",ruserblm)
            specdat$spf <- VALUEcsv[i,2]
            specdat$prop <- rusertarg
        }
        if (swhichparam == "Targ")
        {
            inputdat[iBLMparam] <- paste0("BLM ",ruserblm)
            specdat$spf <- ruserspf
            specdat$prop <- VALUEcsv[i,2]
        }
        # save spec.dat
        write.csv(specdat,paste0(sMarxanDir,"/input/spec",swhichparam,i,".dat"),quote=FALSE,row.names=FALSE)
        # edit parameters and save input.dat
        inputdat[iINPUTDIRparam] <- paste0("INPUTDIR ",sMarxanDir,"/input")
        inputdat[iOUTPUTDIRparam] <- paste0("OUTPUTDIR ",sMarxanDir,"/output")
        inputdat[iSPECNAMEparam] <- paste0("SPECNAME spec",swhichparam,i,".dat")
        inputdat[iSCENNAMEparam] <- paste0("SCENNAME output",swhichparam,i)
        inputdat[iNUMREPSparam] <- "NUMREPS 10"
        writeLines(inputdat,paste0(sMarxanDir,"/core",i,"/input",swhichparam,i,".dat"))
                
        # run Marxan
        setwd(paste0(sMarxanDir,"/core",i))
        system(paste0("./",sExecutable," -s input",swhichparam,i,".dat"))
                
        # read the Marxan summary file
        sumfile <- read.csv(paste0(sMarxanDir,"/output/output",swhichparam,i,"_sum.csv"))
  
        # write to the value summary file
        if (swhichparam == "BLM")
        {
            # write the cost and boundary length to the value summary file
            sRunSummary <<- paste0(sMarxanDir,"/output/output_BLMsummary",i,".csv")
            write(paste(i,VALUEcsv[i,2],mean(sumfile[,3]),mean(sumfile[,5]),sep=","),
                  file=sRunSummary)
        }
        if (swhichparam == "SPF")
        {
            # write the cost and target shortfall to the value summary file
            sRunSummary <<- paste0(sMarxanDir,"/output/output_SPFsummary",i,".csv")
            write(paste(i,VALUEcsv[i,2],mean(sumfile[,3]),mean(sumfile[,12]),sep=","),
                  file=sRunSummary)
        }
        if (swhichparam == "Targ")
        {
            # write the cost and target to the value summary file
            sRunSummary <<- paste0(sMarxanDir,"/output/output_Targsummary",i,".csv")
            write(paste(i,VALUEcsv[i,2],mean(sumfile[,3]),sep=","),
                  file=sRunSummary)
        }
    }     
                
    # compose summary table
    for (i in 1:iCores)
    {
        if (swhichparam == "BLM")
        {
            sRunSummary <<- paste0(sMarxanDir,"/output/output_BLMsummary",i,".csv")
        }
        if (swhichparam == "SPF")
        {
            sRunSummary <<- paste0(sMarxanDir,"/output/output_SPFsummary",i,".csv")
        }
        if (swhichparam == "Targ")
        {
            sRunSummary <<- paste0(sMarxanDir,"/output/output_Targsummary",i,".csv")
        }
        write(readLines(con=sRunSummary),file=sSummary,append=TRUE)
    }
                
    # append the relevant summary table
    if (file.exists(sAppendSummary))
    {
        # ignore header row in sSummary if sAppendSummary exists
        sBuffer <- readLines(con=sSummary)
        write(sBuffer[-1],file=sAppendSummary,append=TRUE)
    } else {
        write(readLines(con=sSummary),
              file=sAppendSummary,append=FALSE)
    }

    # fetch the results
    #PrepareParamTestDisplay()
}

CreateLogFile <- function(sPath,sID)
{
  Gfold <- sprintf("%s",round(runif(1)*1000000))
  for (ii in 1:100000){
    sFile <- sprintf("%s/%s_%s.log",sPath,sID,Gfold)
    if(!file.exists(sFile)) {
      write(paste0(date()," start log ",sID),file=sFile)
      break()
    }
  }
  return(sFile)
}

AppendLogFile <- function(sMessage)
{
  write(paste0(date()," ",sMessage),file=sLogFile,append=TRUE)
}

InitialiseUserSession <- function()
{
    # initialise the user session key and file
    sSessionsDir <- paste0(sShinyPath,"sessions/")
    dir.create(sSessionsDir)
    SessionLoginDate <- Sys.time()
    sSessionUserName <- sUserName
    sSessionUserIP <- sUserIP
    
    repeat
    ({
        sUserSessionKey <<- gen_pwd()
        sUserSessionKeyFile <- paste0(sSessionsDir,sUserSessionKey,".Rdata")

        if (!file.exists(sUserSessionKeyFile))
        {
            # create the session key file
            save(sSessionUserName,SessionLoginDate,sUserSessionKey,sSessionUserIP,file=sUserSessionKeyFile)
            sText <- paste0("sessionkey: ", sUserSessionKey,"\n",
                            "sessionkeyfile: ", sUserSessionKeyFile,"\n",
                            "username: ",sSessionUserName,"\n",
                            "logindate: ", SessionLoginDate, "\n",
                            "userip: ", sSessionUserIP)
            AppendLogFile(sText)
            cat(paste0(sText,"\n"))
            
            break
        }
    })
}

InitialiseUser <- function()
{
    cat(paste0("InitialiseUser start ",sUserName,"\n"))

    # if the users home directory doesn't exist, create it and populate it with a sample dataset
    sUserHome <<- paste0(sShinyUserPath,sUserName)
    if (!file.exists(sUserHome))
    {
        dir.create(sUserHome)
        system(paste0("unzip ",sShinyDataPath,"/",sSampleDataset,".zip -d ",sUserHome))
    }

    # if the users upload app doesn't exist, create it
    sUserApps <- paste0(sShinyPath,"apps/",sUserName,"/")
    dir.create(sUserApps)
    sUserUploadApp <- paste0(sUserApps,sUploadApp,"/")
    if (!file.exists(sUserUploadApp))
    {
        dir.create(sUserUploadApp)
        sCpCmd <- paste0("cp -r ",sShinyPath,"apps/",sUploadApp,"/* ",sUserUploadApp)
        cat(paste0(sCpCmd,"\n"))
        system(sCpCmd)
    }

    # if the users temp directory doesn't exist, create it
    sUserTemp <- paste0(sShinyTempPath,sUserName)
    if (!file.exists(sUserTemp))
    {
        dir.create(sUserTemp)
    }

    # initialise the user session log file
    sLogFile <<- CreateLogFile(paste0(sShinyUserPath,sUserName),sUserName)

    # initialise the user session temp dir
    sUserSession <<- CreateTempDir(sUserTemp)

    # restore the database name
    sRestoreFile <- paste0(sUserHome,"/database.txt")
    if (file.exists(sRestoreFile))
    {
        sRestoreDb <- readLines(paste0(sUserHome,"/database.txt"))
        # is the restored database name in our list of databases?
        if (length(grep(sRestoreDb,c(list.dirs(sUserHome)))) > 0)
        {
            # refresh ui control
            # updateSelectInput(session, inputId, label = NULL,choices = NULL, selected = NULL)
            sSelectDb <<- sRestoreDb
        }
    }

    # remember creation data for most recent database import
    ImportTime <<- max(file.info(c(list.dirs(sUserHome,full.names = TRUE)))$ctime)

    cat("InitialiseUser end\n")
}

ChangeDatabase <- function()
{
    cat("ChangeDatabase start\n")

    # do we need to do RunMarxan_1st ?
    if (length(list.files(path=paste0(sMarxanDir,"/output"))) == 0)
    {
        RunMarxan_1st(sMarxanDir,sShinyDataPath,iCores,iRepsPerCore)
        RunMarxan_paramtest_1st(sMarxanDir,sShinyDataPath,iCores,iRepsPerCore)
    }
    SelectDatabase()
    PrepareDisplay()

    # save the database name
    writeLines(sSelectDb,paste0(sUserHome,"/database.txt"))

    cat("ChangeDatabase end\n")
}

AddDatabase <- function(sDatabasePath)
{
    dir.create(sDatabasePath)
    dir.create(paste0(sDatabasePath,"/input"))
    dir.create(paste0(sDatabasePath,"/output"))
    dir.create(paste0(sDatabasePath,"/pulayer"))
    # copy the marxan files to new directory
    file.copy(paste0(sUserSession,"/marxan/input.dat"),paste0(sDatabasePath,"/input.dat"))
    system(paste0("cp ",sUserSession,"/marxan/input/* ",sDatabasePath,"/input/"))
    system(paste0("cp ",sUserSession,"/marxan/pulayer/* ",sDatabasePath,"/pulayer/"))
}

freegeoip <- function(ip, format = ifelse(length(ip)==1,'list','dataframe'))
{
    return("unknown")

#    url <- paste(c("http://freegeoip.net/json/", ip), collapse='')
#    error_caught <- FALSE
#    url_ <- tryCatch({
#      rtn <<- readLines(url, warn=FALSE)
#    }, error = function(e) {
#      error_caught <<- TRUE
#      cat("503 error caught\n")
#      rtn <<- "error caught"
#    })
    
#    if (error_caught)
#    {
#      return("unknown")
#    } else {
#      ret <- fromJSON(rtn)
#      return(ret)
#    }
}   

substrRight <- function(x, n)
{
    substr(x, nchar(x)-n+1, nchar(x))
}

gen_pwd <- function(iLength=16)
# password generator. minimum length is 4
# at least 1 upper case character, 1 lower case character, 1 number
# omit IiLlOo01 so no character confusion when reading/typing
{
    library(stringi)
    rand_all <- stri_rand_strings(n=1, length=iLength-3, pattern="[ABCDEFGHJKMNPQRSTUVWXYZabcdefghjkmnpqrstuvwxyz23456789]")
    rand_upper <- stri_rand_strings(n=1, length=1, pattern="[ABCDEFGHJKMNPQRSTUVWXYZ]")
    rand_lower <- stri_rand_strings(n=1, length=1, pattern="[abcdefghjkmnpqrstuvwxyz]")
    rand_numeric <- stri_rand_strings(n=1, length=1, pattern="[23456789]")
    x <- paste0(rand_all,rand_upper,rand_lower,rand_numeric)
    y <- as.data.frame(strsplit(x,""))
    return(paste(as.character(y[sample(nchar(x)),]),collapse=""))
}

