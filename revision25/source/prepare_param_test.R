safe_log <- function(rValue)
{
  if (rValue > 0)
  {
    return(log(rValue))
  } else {
    return(-20)
  }
}

RunMarxan_paramtest_1st <- function(sDatabasePath,sShinyDataPath,iCores,iRepsPerCore)
{
  rRampBLMmin <- 0
  rRampBLMmax <- 10000000000000
  rRampSPFmin <- 0.0001
  rRampSPFmax <- 10000000000000
  rtargetmin <- 0
  rtargetmax <- 1
  rcostmin <- 0.0001
  rcostmax <- 10000000000000
  ruserblm <- 0
  ruserspf <- 1
  rusertarg <- 0.3
  rusercost <- 0.0001
  if (.Platform$pkgType == "source")
  {
    sExecutable <- "MarOpt_v243_Linux64"
  } else {
    sExecutable <- "MarOpt_v243_Mac64"
  }
  
  theparams <- c("BLM","SPF","Targ")
  for (iparam in 1:length(theparams))
  {
    swhichparam <- theparams[iparam]
    
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
    write(paste0('i,',swhichparam),file=paste0(sDatabasePath,"/",swhichparam,".csv"))
    write(paste0(1,",",rValue),file=paste0(sDatabasePath,"/",swhichparam,".csv"),append=TRUE)
    for (i in 2:iCores)
    {
      if (swhichparam == "Targ")
      {
        rValue <- rMinimum+((i-1)*rInterval)       # linear ramping for Target
      } else {
        rValue <- exp(rMinimum+((i-1)*rInterval))  # exponential ramping for BLM, SPF and Cost
      }
      write(paste0(i,",",rValue),file=paste0(sDatabasePath,"/",swhichparam,".csv"),append=TRUE)
    }
    
    # initialise a value summary file
    sSummary <- paste0(sDatabasePath,"/output/output_",swhichparam,"summary.csv")
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
    VALUEcsv <- read.csv(paste0(sDatabasePath,"/",swhichparam,".csv"))
    
    foreach(i=1:iCores) %dopar%   
    {
      dir.create(paste0(sDatabasePath,"/core",i))
      file.copy(paste0(sShinyDataPath,"/",sExecutable),paste0(sDatabasePath,"/core",i,"/",sExecutable))
      system(paste0("chmod +x ",sDatabasePath,"/core",i,"/",sExecutable))
      
      # read input.dat and edit parameters
      inputdat <- readLines(paste0(sDatabasePath,"/input.dat"))
      iINPUTDIRparam <- which(regexpr("INPUTDIR",inputdat)==1)
      iOUTPUTDIRparam <- which(regexpr("OUTPUTDIR",inputdat)==1)
      iBLMparam <- which(regexpr("BLM",inputdat)==1)
      iSCENNAMEparam <- which(regexpr("SCENNAME",inputdat)==1)
      iNUMREPSparam <- which(regexpr("NUMREPS",inputdat)==1)
      iSPECNAMEparam <- which(regexpr("SPECNAME",inputdat)==1)
      iPUNAMEparam <- which(regexpr("PUNAME",inputdat)==1)
      # read spec.dat
      specdat <- read.csv(paste0(sDatabasePath,"/input/spec.dat"))
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
      write.csv(specdat,paste0(sDatabasePath,"/input/spec",swhichparam,i,".dat"),quote=FALSE,row.names=FALSE)
      # edit parameters
      inputdat[iINPUTDIRparam] <- paste0("INPUTDIR ",sMarxanDir,"/input")
      inputdat[iOUTPUTDIRparam] <- paste0("OUTPUTDIR ",sMarxanDir,"/output")
      inputdat[iSPECNAMEparam] <- paste0("SPECNAME spec",swhichparam,i,".dat")
      inputdat[iSCENNAMEparam] <- paste0("SCENNAME output",swhichparam,i)
      inputdat[iNUMREPSparam] <- paste0("NUMREPS ",iRepsPerCore)
      # save input.dat
      writeLines(inputdat,paste0(sDatabasePath,"/core",i,"/input",swhichparam,i,".dat"))
    }
    
    # run Marxan
    foreach(i=1:iCores) %dopar%   
    {
      setwd(paste0(sDatabasePath,"/core",i))
      system(paste0("./",sExecutable," -s input",swhichparam,i,".dat"))

      # read the Marxan summary file
      sumfile <- read.csv(paste0(sDatabasePath,"/output/output",swhichparam,i,"_sum.csv"))
      
      # write to the value summary file
      sSummaryI <- paste0(sDatabasePath,"/output/output_",swhichparam,"summary",i,".csv")
      if (swhichparam == "BLM")
      {
        # write the cost and boundary length to the value summary file
        write(paste(i,VALUEcsv[i,2],mean(sumfile[,3]),mean(sumfile[,5]),sep=","),
              file=sSummaryI)
      }
      if (swhichparam == "SPF")
      {
        # write the cost and target shortfall to the value summary file
        write(paste(i,VALUEcsv[i,2],mean(sumfile[,3]),mean(sumfile[,12]),sep=","),
              file=sSummaryI)
      }
      if (swhichparam == "Targ")
      {
        # write the cost and target to the value summary file
        write(paste(i,VALUEcsv[i,2],mean(sumfile[,3]),sep=","),
              file=sSummaryI)
      }
    }              
    
    # compose summary table
    for (i in 1:iCores)
    {
      sSummaryI <- paste0(sDatabasePath,"/output/output_",swhichparam,"summary",i,".csv")
      if (swhichparam == "BLM")
      {
        write(readLines(con=sSummaryI),
              file=sSummary,append=TRUE)
      }
      if (swhichparam == "SPF")
      {
        write(readLines(con=sSummaryI),
              file=sSummary,append=TRUE)
      }
      if (swhichparam == "Targ")
      {
        write(readLines(con=sSummaryI),
              file=sSummary,append=TRUE)
      }
    }
    
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
    write(readLines(con=sSummary),
          file=sAppendSummary,append=FALSE)
  }
}
