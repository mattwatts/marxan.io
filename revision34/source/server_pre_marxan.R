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
    #x__ <<- max(pulayer_$X)-min(pulayer_$X)
    #y__ <<- max(pulayer_$Y)-min(pulayer_$Y)
    iAspectX <<- max(pulayer_$X)-min(pulayer_$X)
    iAspectY <<- max(pulayer_$Y)-min(pulayer_$Y)
    # read the BLM, prop, SPF ui control values
    inputdat <- readLines(paste(sMarxanDir,"/input.dat",sep=""))
    sParamBLM <- GetParamValue(inputdat,"BLM")
    if (sParamBLM != "")
    {
        sBLM <<- sParamBLM
        cat(paste0("sBLM ",sBLM,"\n"))
    }
}

PrepareDisplay <- function(sCallingApp)
{
    cat("PrepareDisplay start\n")

    # prepare the map: pulayer object
    pulayer <<- pulayer_
    pu_table <<- read.dbf(paste(sMarxanDir,"/pulayer/pulayer.dbf",sep=""))

    # prepare the planning unit status object
    pustatus <<- pustatus_
    # make status work ok
    # join pu.dat and pulayer with PUID field (to order them and handle missing rows)
    prepare_pu_status()
    fExistingReserves <<- (2 %in% pustatus_)
    fExcluded <<- (3 %in% pustatus_)

    if (sCallingApp == "marxan")
    {
        # load the cluster analysis objects from file
        load(paste0(sMarxanDir,"/output/cluster.Rdata"))
        sol.mds <<- sol.mds
        sol3d.mds <<- sol3d.mds
        nmdscolours <<- nmdscolours
        plotlabels <<- plotlabels
        d <<- d
    }

    cat("PrepareDisplay end\n")
}

GenerateSolnFilename <- function(iRunNumber,sMarxanDir)
{
  sFilename <- paste0(sMarxanDir,"/output/output_r")
  iPad <- 5 - nchar(as.character(iRunNumber))
  sFilename <- paste0(sFilename,paste0(rep("0",iPad),collapse=""))
  sFilename <- paste0(sFilename,iRunNumber,GetOutputFileext(sMarxanDir,"SAVERUN"))
}

map_png <- function(colours,sPngFile,width=600)
{
    cat(paste0(sPngFile,"\n"))

    # plot to png file
    iWidth <- width + 57 + 28
    iHeight <- round(iWidth/iAspectX*iAspectY)
    # plot to png file
    png(filename = sPngFile,width = iWidth, height = iHeight)
    plotPolys(pulayer,col=colours,axes=FALSE,border=NA,cex.lab=0.1,cex.axis=0.1)
    if (!is.na(puoutline))
    {
        addLines(puoutline,col="black")
    }
    dev.off()
    # clip whitespace
    apng <- readPNG(sPngFile)
    apng <- apng[58:(iHeight - 71),58:(iWidth - 28),]
    writePNG(apng,target=sPngFile)
}

map_pt_ssolnNmap <- function(tempputable)
{
    sFilename <- paste0(sMarxanDir,"/output/output",swhichparam,iwhichmap,"_ssoln.csv")
    solution_table <- read.csv(sFilename)
    colnames(solution_table)[1] <- "PUID"
    colnames(solution_table)[2] <- "SSOLN2"
    solution_table$SSOLN2 <- as.integer(solution_table$SSOLN2)
    values_ <- sqldf("SELECT * from tempputable LEFT JOIN solution_table USING(PUID)")
    values_ <- sqldf("SELECT SSOLN2 from values_") # + 1
    
    # make NA values 0
    for (i in 1:nrow(values_))
    {
        if (is.na(values_[i,]))
        {
            values_[i,] <- 0
        }
    }
    
    blueramp <- colorRampPalette(c("white","blue"))(5)
    colours <- rep(blueramp[1],nrow(values_))
    for (j in 1:nrow(values_))
    {
        if (pustatus[j] == 2)
        {
            colours[j] <- "#40E0D0" # Turquoise
        } else {
            if (pustatus[j] == 3)
            {
                colours[j] <- "grey"
            } else {
                if (values_[j,] < 1)
                {
                    colours[j] <- "white"
                } else {
                    if (values_[j,] < 3)
                    {
                        colours[j] <- blueramp[2]
                    } else {
                        if (values_[j,] < 7)
                        {
                            colours[j] <- blueramp[3] 
                        } else {
                            if (values_[j,] < 10)
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
    #map_png(colours,paste0(sMarxanDir,"/output/output_pt_map.png"),600)
    plotPolys(pulayer,col=colours,axes=FALSE,border=NA,cex.lab=0.1,cex.axis=0.1)
}

map_pt <- function()
{
    tempputable <- sqldf("SELECT PUID from pu_table")
    colnames(tempputable)[1] <- "PUID"
    
    if (swhichrun == "ssoln")
    {
        cat("ssoln\n")
        
        map_pt_ssolnNmap(tempputable)
    } else {
        if (swhichrun == "best")
        {
            cat("best\n")
            sFilename <- paste0(sMarxanDir,"/output/output",swhichparam,iwhichmap,"_best.csv")
        } else {
            cat("solution M\n")
            sFilename <- paste0(sMarxanDir,"/output/output",swhichparam,iwhichmap,"_r",PadInt(as.integer(swhichrun)),".csv")
        }
        solution_table <- read.csv(sFilename)
        
        values_ <- sqldf("SELECT * from tempputable LEFT JOIN solution_table USING(PUID)")
        # plot the map
        values_ <- as.integer(unlist(sqldf("SELECT SOLUTION from values_") + 1))
        colourpalette <- c("white","blue")
        colours <- rep("white",each=length(values_))
        for (j in 1:length(values_))
        {
            if (pustatus[j] == 2)
            {
                colours[j] <- "#40E0D0" # Turquoise
            } else {
                if (pustatus[j] == 3)
                {
                    colours[j] <- "grey"
                } else {
                    colours[j] <- colourpalette[values_[j]]
                }
            }
        }
        
        #map_png(colours,paste0(sMarxanDir,"/output/output_pt_map.png"),600)
        plotPolys(pulayer,col=colours,axes=FALSE,border=NA,cex.lab=0.1,cex.axis=0.1)
    }
}

map_ssolnNmap <- function()
{
    values <- sqldf(paste("SELECT SSOLN2 from pu_table",sep=""))
    
    # make NA values 0
    for (i in 1:nrow(values))
    {
        if (is.na(values[i,]))
        {
            values[i,] <- 0
        }
    }

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
    #map_png(colours,paste0(sMarxanDir,"/output/output_map.png"),600)
    plotPolys(pulayer,col=colours,axes=FALSE,border=NA,cex.lab=0.1,cex.axis=0.1)
}

prepare_pu_status <- function()
{
    # prepare pustatus
    # join pu.dat and pulayer with PUID field (to order them and handle missing rows)
    pudat <- read.csv(paste0(sMarxanDir,"/input/pu.dat"),stringsAsFactors=FALSE)
    colnames(pudat)[1] <- "PUID"
    pu_id <- sqldf("SELECT PUID from pu_table")
    pustatus_sorted <- sqldf("SELECT * from pu_id LEFT JOIN pudat USING(PUID)")
    pustatus <- unlist(pustatus_sorted$status)
    # mark pu's in pulayer that are missing from pu.dat as status 0
    for (i in 1:length(pustatus))
    {
        if (is.na(pustatus[i]))
        {
            pustatus[i] <- 0
        }
    }
    pustatus <<- pustatus
}

binary_map <- function(values)
{
    greenramp <- colorRampPalette(c("white","blue"))(2)
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
    #map_png(colours,paste0(sMarxanDir,"/output/output_map.png"),600)
    plotPolys(pulayer,col=colours,axes=FALSE,border=NA,cex.lab=0.1,cex.axis=0.1)
}

map_bestmap <- function()
{
    values <- sqldf("SELECT BESTSOLN from pu_table")
    binary_map(values)
}

prepare_M_values <- function(solnX_table)
{
    # prepare pustatus
    # join solnX_table and pulayer with PUID field (to order them and handle missing rows)
    pu_id <- sqldf("SELECT PUID from pu_table")
    values_sorted <- sqldf("SELECT * from pu_id LEFT JOIN solnX_table USING(PUID)")
    values <- sqldf("SELECT SOLUTION from values_sorted")
    # mark pu's in pulayer that are missing from values as "Available"
    for (i in 1:nrow(values))
    {
        if (is.na(values[i,]))
        {
            values[i,] <- 0
        }
    }
    return(values + 1)
}

map_runMmap <- function()
{
    solnX_table <- read.csv(GenerateSolnFilename(iM,sMarxanDir))
    #values <- sqldf(paste("SELECT SOLUTION from solnX_table",sep="")) + 1
    
    # "fix" the values to match order of putable
    prepare_pu_status()
    values <- prepare_M_values(solnX_table)
    
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
        plot3d(sol3d.mds$points, xlab="",ylab="",zlab="", col=nmdscolours)
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

    withProgress(message="Run Marxan",value=0,
    {
        withProgress(message="Marxan",value=0,
        {
            RunMarxan(sMarxanDir,sExecutable,iCores,iRepsPerCore)
        })

        withProgress(message="Merge results",value=0,
        {
            JoinParallelResults(sMarxanDir,iCores,iRepsPerCore)
        })

        withProgress(message="Populate dbf",value=0,
        {
            # write results to pu dbf
            ImportOutputsCsvToShpDbf(paste0(sMarxanDir,"/pulayer/pulayer.dbf"),
                                     sMarxanDir,round(iCores*iRepsPerCore),"PUID")
        })

        withProgress(message="Prepare cluster",value=0,
        {
            PrepareCluster_compute(sMarxanDir)
        })

        withProgress(message="Prepare display",value=0,
        {
            PrepareDisplay("marxan")
        })
    })
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
                
    withProgress(message="Run parameter test",value=0,
    {
      withProgress(message="Marxan",value=0,
      {
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
            inputdat[iNUMREPSparam] <- paste0("NUMREPS ",iParamTestReps)
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
      })
    })
    
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

CreateLogFile <- function(sPath,sID,sCallingApp)
{
  Gfold <- sprintf("%s",round(runif(1)*1000000))
  for (ii in 1:100000){
    sFile <- sprintf("%s/%s_%s_%s.log",sPath,sCallingApp,sID,Gfold)
    if(!file.exists(sFile)) {
      write(paste0(date()," start log ",sID),file=sFile)
      break()
    }
  }
  return(sFile)
}

AppendLogFile <- function(sLogFileName,sMessage)
{
  write(paste0(date()," ",sMessage),file=sLogFileName,append=TRUE)
}

AuthenticateUserSession <- function(sessionkey)
{
    sUserSessionKey <<- sessionkey
    
    # get the user session file
    sSessionKeyFile <- paste0(sShinyPath,"sessions/",sessionkey,".Rdata")
    if (file.exists(sSessionKeyFile))
    {
        load(sSessionKeyFile)
        sText <- paste0("sessionkey: ", sUserSessionKey,"\n",
                        "sessionkeyfile: ", sSessionKeyFile,"\n",
                        "username: ",sSessionUserName,"\n",
                        "logindate: ", SessionLoginDate, "\n",
                        "userip: ", sSessionUserIP)
        cat(paste0(sText,"\n"))
        # verify session details
        # does ip match ip in session file ?
        #sUserIP <- as.character(input$ipid)
        if (fSessionValid) #(sUserIP == sSessionUserIP)
        {
            # is session < 12 hours old ?
            rDiffTime <- as(difftime(Sys.time(),SessionLoginDate,units="hours"),"numeric")
            if (rDiffTime < 12)
            {
                cat(paste0("session is ",rDiffTime," hours old\n"))
                cat("user is authenticated\n")
                sUserName <<- sSessionUserName
                return(TRUE)
            } else {
                cat("session has expired\n")
                return(FALSE)
            }
        } else {
            #cat(paste0("IP address mismatch user:>",sUserIP,"< session:>",sSessionUserIP,"<\n"))
            cat(paste0("User has logged out user:>",sUserIP,"< session:>",sSessionUserIP,"<\n"))
            return(FALSE)
        }
    } else {
        cat(paste0("session file >",sSessionKeyFile,"< does not exist\n"))
        return(FALSE)
    }
}

AuthenticateUser <- function(sCallingApp)
{
    cat(paste0("AuthenticateUser start ",sUserName,"\n"))

    # if the users home directory doesn't exist, create it and populate it with a sample dataset
    sUserHome <<- paste0(sShinyUserPath,sUserName)
    sMarxanHome <<- paste0(sShinyUserPath,sUserName,"/marxan/")
    sMarZoneHome <<- paste0(sShinyUserPath,sUserName,"/marzone/")
    sUserTemp <- paste0(sShinyTempPath,sUserName)
    
    if (sCallingApp == "upload")
    {
        
    } else {
    
        if (sCallingApp == "marxan")
        {
            sAppHome <<- sMarxanHome
        
        } else {
        
            sAppHome <<- sMarZoneHome
        }
    
        # restore the database name
        sRestoreFile <- paste0(sAppHome,"/database_",sCallingApp,".txt")
        if (file.exists(sRestoreFile))
        {
            sRestoreDb <- readLines(sRestoreFile)
            # is the restored database name in our list of databases?
            if (length(grep(sRestoreDb,c(list.dirs(sAppHome)))) > 0)
            {
                sSelectDb <<- sRestoreDb
            }
        }

        # remember creation data for most recent database import
        ImportTime <<- max(file.info(c(list.dirs(sAppHome,full.names = TRUE)))$ctime)
    }

    # initialise the user session log file
    sLogFile <<- CreateLogFile(paste0(sShinyUserPath,sUserName),sUserName,sCallingApp)

    # initialise the user session temp dir
    sUserSession <<- CreateTempDir(sUserTemp)

    cat("AuthenticateUser end\n")
}

LogoutSession <- function()
{
    # create the session key file
    load(file=sUserSessionKeyFile)
    fSessionValid <- FALSE
    save(sSessionUserName,SessionLoginDate,sUserSessionKey,sSessionUserIP,fSessionValid,file=sUserSessionKeyFile)
    AppendLogFile(sLogFile,"LogoutSession")
    cat("LogoutSession\n")
    USER$Logged <<- FALSE
    iLogin <<- iLoginClick
}

InitialiseUserSession <- function()
{
    # initialise the user session key and file
    sSessionsDir <- paste0(sShinyPath,"sessions/")
    dir.create(sSessionsDir)
    SessionLoginDate <- Sys.time()
    sSessionUserName <- sUserName
    sSessionUserIP <- sUserIP
    fSessionValid <- TRUE
    
    repeat
    ({
        sUserSessionKey <<- gen_pwd()
        sUserSessionKeyFile <<- paste0(sSessionsDir,sUserSessionKey,".Rdata")

        if (!file.exists(sUserSessionKeyFile))
        {
            # create the session key file
            save(sSessionUserName,SessionLoginDate,sUserSessionKey,sSessionUserIP,fSessionValid,file=sUserSessionKeyFile)
            sText <- paste0("sessionkey: ", sUserSessionKey,"\n",
                            "sessionkeyfile: ", sUserSessionKeyFile,"\n",
                            "username: ",sSessionUserName,"\n",
                            "logindate: ", SessionLoginDate, "\n",
                            "userip: ", sSessionUserIP)
            AppendLogFile(sLogFile,sText)
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
    sMarxanHome <<- paste0(sShinyUserPath,sUserName,"/marxan/")
    sMarZoneHome <<- paste0(sShinyUserPath,sUserName,"/marzone/")
    if (!file.exists(sUserHome))
    {
        dir.create(sUserHome)
        
        if (!file.exists(sMarxanHome))
        {
            dir.create(sMarxanHome)
            system(paste0("unzip ",sShinyDataPath,"/",sSampleMarxanDataset,".zip -d ",sMarxanHome))
        }
        
        if (!file.exists(sMarZoneHome))
        {
            dir.create(sMarZoneHome)
            system(paste0("unzip ",sShinyDataPath,"/",sSampleMarZoneDataset,".zip -d ",sMarZoneHome))
        }
    }

    # if the users apps don't exist, create them
    sUserApps <- paste0(sShinyPath,"apps/",sUserName,"/")
    dir.create(sUserApps)
    sUserAllApps <- paste0(sUserApps,sAllApps,"/")
    if (!file.exists(sUserAllApps))
    {
        dir.create(sUserAllApps)
        sCpCmd <- paste0("cp -r ",sShinyPath,"apps/",sAllApps,"/* ",sUserAllApps)
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
    sLogFile <<- CreateLogFile(paste0(sShinyUserPath,sUserName),sUserName,"login")

    cat("InitialiseUser end\n")
}

ChangeDatabase <- function(sCallingApp)
{
    cat("ChangeDatabase start\n")

    if (sCallingApp == "marxan")
    {
        # do we need to do RunMarxan_1st ?
        if (!file.exists(paste0(sMarxanDir,"/output/output_sum.csv")))
        {
            RunMarxan_1st(sMarxanDir,sShinyDataPath,iCores,iRepsPerCore)
        }
    }
    
    if (sCallingApp == "mxptest")
    {
        # do we need to do RunMarxan_paramtest_1st for this paramter?
        if (!file.exists(paste0(sMarxanDir,"/output/output_",swhichparam,"summary.csv")))
        {
            RunMarxan_paramtest_1st(swhichparam,sMarxanDir,sShinyDataPath,iCores,iParamTestReps)
            if (swhichparam == "BLM")
            {
                sAppendSummary <<- paste0(sMarxanDir,"/output/output_BLMsummary_SPF",ruserspf,"_Targ",rusertarg,".csv")
            }
            if (swhichparam == "SPF")
            {
                sAppendSummary <<- paste0(sMarxanDir,"/output/output_SPFsummary_BLM",ruserblm,"_Targ",rusertarg,".csv")
            }
            if (swhichparam == "Targ")
            {
                sAppendSummary <<- paste0(sMarxanDir,"/output/output_Targsummary_BLM",ruserblm,"_SPF",ruserspf,".csv")
            }
        }
    }
    
    SelectDatabase()
    PrepareDisplay(sCallingApp)

    # save the database name
    writeLines(sSelectDb,paste0(sAppHome,"/database_",sCallingApp,".txt"))

    cat("ChangeDatabase end\n")
}

AddDatabase <- function(sDatabasePath)
{
    dir.create(sDatabasePath)
    dir.create(paste0(sDatabasePath,"/input"))
    dir.create(paste0(sDatabasePath,"/output"))
    dir.create(paste0(sDatabasePath,"/pulayer"))
    # copy the marxan files to new directory
    if (fMarZone)
    {
        file.copy(paste0(sUserSession,"/marzone/input.dat"),paste0(sDatabasePath,"/input.dat"))
        system(paste0("cp ",sUserSession,"/marzone/input/* ",sDatabasePath,"/input/"))
        system(paste0("cp ",sUserSession,"/marzone/pulayer/* ",sDatabasePath,"/pulayer/"))
    } else {
        file.copy(paste0(sUserSession,"/marxan/input.dat"),paste0(sDatabasePath,"/input.dat"))
        system(paste0("cp ",sUserSession,"/marxan/input/* ",sDatabasePath,"/input/"))
        system(paste0("cp ",sUserSession,"/marxan/pulayer/* ",sDatabasePath,"/pulayer/"))
    }
}

freegeoip <- function(ip, format = ifelse(length(ip)==1,'list','dataframe'))
{
    return("unknown")
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

GenerateLegendPNG <- function(AColour,sOutputDir,sPngName)
{
  sPng <- paste0(sOutputDir,"/",sPngName)
  png(file=sPng,bg=AColour)
  plot(1:10)
  rect(1,10,1,10,col=AColour)
  dev.off()
  # crop a PNG file
  apng <- readPNG(sPng)
  png2 <- apng[1:19,1:19,]
  writePNG(png2,target=sPng)
}

GenerateMarZoneLegendPNG <- function(iZones,sOutputDir)
{
  ARainbox <- rainbow(iZones)
  for (i in 1:iZones)
  {
    GenerateLegendPNG(ARainbox[i],sOutputDir,paste0("rainbow_",iZones,"_",i,".png"))
  }
}



