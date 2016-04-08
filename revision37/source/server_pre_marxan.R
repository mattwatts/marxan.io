# marxan.io

# init some variables
rprop <<- 0.3
iM <<- 1
fMarxanRunning <<- FALSE
sUserInterface <<- "Select Database"
fAllow <<- FALSE

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
    ssolntable_ <- read.csv(paste0(sMarxanDir,"/output/output1_ssoln.csv"))
    selectionfreqs <- rep(0,nrow(ssolntable_))
    for (i in 2:iZones)
    {
      selectionfreqs <- cbind(selectionfreqs,rep(0,nrow(ssolntable_)))
    }
    for (j in 1:iZones)
    {
        selectionfreqs[,j] <- selectionfreqs[,j] + ssolntable_[,j+2]
    }
    for (i in 2:iCores)
    {
        ssolntable_ <- read.csv(paste0(sMarxanDir,"/output/output",i,"_ssoln.csv"))
        for (j in 1:iZones)
        {
            selectionfreqs[,j] <- selectionfreqs[,j] + ssolntable_[,j+2]
        }
    }
    ssolntable <- cbind(ssolntable_$planning.unit,(ssolntable_$number*10),selectionfreqs)
    colnames(ssolntable) <- c("planning.unit","number",ZoneNames)
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
        a <- attributes(x)
        label <- attr(x, "label")
        colour <- "black"
        if (label == paste0("S",iBest," (Best)")) { colour <- "blue" }
        attr(x, "nodePar") <- c(a$nodePar, lab.col = colour)
    }
    return(x)
}

labelCol_marzone <- function(x)
{
    if (is.leaf(x))
    {
        a <- attributes(x)
        label <- attr(x, "label") 
        colour <- "black"
        for (i in 1:iZones)
        {
            sZone <- paste0("Z",i)
            if (substring(label,1,2) == sZone) { colour <- rainbow(iZones)[i] }
        }
        attr(x, "nodePar") <- c(a$nodePar, lab.col = colour)
    }
    return(x)
}

PrepareCluster_compute_MarZone <- function(sMarxanDir)
{
    # NOTE: we fail gracefully if there are not enough unique solutions
    # prepare the cluster analysis objects
    withProgress(message="Load cluster matrix",value=0,
    {
        solutions_raw<-read.table(paste0(sMarxanDir,"/output/output_solutionsmatrix.csv"),header=TRUE, row.name=1, sep=",")
    })
    
    withProgress(message="Find unique solutions",value=0,
    {
        solutions <- unique(solutions_raw)
        iUniqueSolutions <- dim(solutions)[1]
    })
    
    if (iUniqueSolutions > 2)
    {
        # render the 2d
        withProgress(message="Compute distance",value=0,
        {
            rainbowpalette <- rainbow(iZones)
            nmdscolours <- rep("white",each = iUniqueSolutions)
            soldist<-vegdist(solutions,distance="jaccard")
        })
        withProgress(message="Compute 2ds",value=0,
        {
            sol.mds<-nmds(soldist,2)
        })
        withProgress(message="Compute clusters",value=0,
        {
            h<-hclust(soldist, method="complete")
        })
        withProgress(message="Compute palette",value=0,
        {
            plotlabels <- row.names(solutions)
            iCount <- 0
            for (j in 1:length(nmdscolours))
            {
                # plotlabel like "Z1S1"
                x <- strsplit(plotlabels[j],split="S")[1]
                y <- strsplit(x[[1]][1],split="Z")
                iZone <- as.numeric(y[[1]][2])

                # the plotlabel displayed is just the solution number
                plotlabels[j] <- paste0(x[[1]][2])

                iCount <- iCount + 1
                nmdscolours[iCount] <- rainbowpalette[iZone]
            }
        })

        withProgress(message="Compute dendogram",value=0,
        {
            # render the dendogram
            d <- dendrapply(as.dendrogram(h), labelCol_marzone)
        })

        withProgress(message="Compute 3ds",value=0,
        {
            # render the 3d
            #sol3d.mds <- nmds(soldist,3)
            sol3d.mds <- NA
        })
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

SelectDatabase <- function(sCallingApp)
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
    iAspectX <<- max(pulayer_$X)-min(pulayer_$X)
    iAspectY <<- max(pulayer_$Y)-min(pulayer_$Y)
    if ((sCallingApp == "marxan") | (sCallingApp == "mxptest"))
    {
        # read the BLM, prop, SPF ui control values
        inputdat <- readLines(paste(sMarxanDir,"/input.dat",sep=""))
        sParamBLM <- GetParamValue(inputdat,"BLM")
        if (sParamBLM != "")
        {
            sBLM <<- sParamBLM
            cat(paste0("sBLM ",sBLM,"\n"))
        }
    }
    if (sCallingApp == "marzone")
    {
        # iZones, ZoneNames
        zonesdat <- read.csv(paste0(sMarxanDir,"/input/zones.dat"))
        iZones <<- nrow(zonesdat)
        ZoneNames <<- as.character(unlist(zonesdat$zonename))
        
        GenerateMarZoneLegendPNG(iZones,paste0(sShinyPath,"images/"))
        
        updateSliderInput(session, "n", , value = iZones, max = iZones)
    }
}

PrepareDisplay <- function(sCallingApp)
{
    cat("PrepareDisplay start\n")

    # prepare the map: pulayer object
    pulayer <<- pulayer_
    pu_table <<- read.dbf(paste(sMarxanDir,"/pulayer/pulayer.dbf",sep=""))

    if (sCallingApp == "marxan")
    {
        # prepare the planning unit status object
        pustatus <<- pustatus_
        # make status work ok
        # join pu.dat and pulayer with PUID field (to order them and handle missing rows)
        prepare_pu_status()
        fExistingReserves <<- (2 %in% pustatus_)
        fExcluded <<- (3 %in% pustatus_)

        # load the cluster analysis objects from file
        load(file=paste0(sMarxanDir,"/output/cluster.Rdata"))
        sol.mds <<- sol.mds
        sol3d.mds <<- sol3d.mds
        nmdscolours <<- nmdscolours
        plotlabels <<- plotlabels
        d <<- d
    }

    if (sCallingApp == "marzone")
    {
        if (file.exists(paste0(sMarxanDir,"/output/cluster.Rdata")))
        {
            # load the cluster analysis objects from file
            load(file=paste0(sMarxanDir,"/output/cluster.Rdata"))
            sol.mds <<- sol.mds
            sol3d.mds <<- sol3d.mds
            nmdscolours <<- nmdscolours
            plotlabels <<- plotlabels
            d <<- d
        }
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

map_mz_ssolnNmap <- function(iN)
{
    # output_ssoln.csv
    # "planning unit","number","available","reserved"
    ssolntable <- read.csv(paste0(sMarxanDir,"/output/output_ssoln.csv"))
    colnames(ssolntable)[1] <- "PUID"
    # change the zone names in case they are using SQL reserved word as zone name
    #for (i in 1:iZones)
    #{
    #    colnames(ssolntable)[i+2] <- paste0("zone",i)
    #}
    
    # rectify values with order of planning units in the pulayer
    pu_id <- sqldf("SELECT PUID from pu_table")
    ssoln_sorted <- sqldf("SELECT * from pu_id LEFT JOIN ssolntable USING(PUID)")
    values <- sqldf(paste0("SELECT ",ZoneNames[iN]," from ssoln_sorted"))
    # mark pu's in pulayer that are missing from ssolntable as zone 0 (white)
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
    #map_png(colours,paste0(sMarxanDir,"/output/output_map.png"),600)
    plotPolys(pulayer,col=colours,axes=FALSE,border=NA,cex.lab=0.1,cex.axis=0.1)
}

map_mz_bestmap <- function()
{
    # output_sum.csv
    # "Run Number","Score","Cost","Planning Units",available PuCount,reserved PuCount,available Cost,reserved Cost,"Connection Strength","Penalty","Shortfall","Missing_Values","MPM"
    sumtable <- read.csv(paste0(sMarxanDir,"/output/output_sum.csv"))
    iBest <- which(sumtable$Score==min(sumtable$Score))
    if (length(iBest) > 1)
    {
        iBest <- iBest[1]
    }
    # output_r00001.csv
    # planning_unit,zone
    map_mz_runMmap(iBest)
}

map_mz_runMmap <- function(iM)
{
    # output_r00001.csv
    # planning_unit,zone
    soln <- read.csv(paste0(sMarxanDir,"/output/output_r",PadInt(iM),".csv"))
    colnames(soln)[1] <- "PUID"

    # rectify values with order of planning units in the pulayer
    pu_id <- sqldf("SELECT PUID from pu_table")
    soln_sorted <- sqldf("SELECT * from pu_id LEFT JOIN soln USING(PUID)")
    values <- sqldf("SELECT zone from soln_sorted")
    # mark pu's in pulayer that are missing from soln as zone 0 (white)
    for (i in 1:nrow(values))
    {
        if (is.na(values[i,]))
        {
            values[i,] <- 0
        }
    }

    # use a rainbow colour palette
    rainbowramp <- rainbow(iZones)
    colours <- rep("white",nrow(values))
    for (j in 1:nrow(values))
    {
        if (values[j,] == 0)
        {
            colours[j] <- "white" # mark pu's in pulayer that are missing from soln as zone 0 (white)
        } else {
            colours[j] <- rainbowramp[values[j,]]
        }
    }
    #map_png(colours,paste0(sMarxanDir,"/output/output_map.png"),600)
    plotPolys(pulayer,col=colours,axes=FALSE,border=NA,cex.lab=0.1,cex.axis=0.1)
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

auto_compute_marzone_cluster <- function()
{
    if (!file.exists(paste0(sMarxanDir,"/output/cluster.Rdata")))
    {
        PrepareCluster_compute_MarZone(sMarxanDir)

        withProgress(message="Load cluster.Rdata",value=0,
        {
            # load the cluster analysis objects from file

            load(file=paste0(sMarxanDir,"/output/cluster.Rdata"))
            sol.mds <<- sol.mds
            sol3d.mds <<- sol3d.mds
            nmdscolours <<- nmdscolours
            plotlabels <<- plotlabels
            d <<- d
        })
    }
}

cluster_2ds <- function(sCallingApp,fDisplayText)
{
    if (sCallingApp == "marzone")
    {
        auto_compute_marzone_cluster()
    }
    if (is.na(sol.mds))
    {
        plot(1,1)
    }
    else
    {
        withProgress(message="Plot cluster",value=0,
        {
            plot(sol.mds$points, xlab='', ylab='', main='NMDS of solutions', col=nmdscolours)
            if (fDisplayText)
            {
                text(sol.mds$points,labels=plotlabels,pos=4, col=nmdscolours)
            }
        })
    }
}
 
cluster_3ds <- function(sCallingApp)
{
    if (sCallingApp == "marzone")
    {
        auto_compute_marzone_cluster()
    }
    if (is.na(sol3d.mds))
    {
        plot(1,1)
    }
    else
    {
        withProgress(message="Plot cluster",value=0,
        {
            plot3d(sol3d.mds$points, xlab="",ylab="",zlab="", col=nmdscolours)
        })
    }
}

cluster_dendogram <- function(sCallingApp)
{
    if (sCallingApp == "marzone")
    {
        auto_compute_marzone_cluster()
    }
    if (is.na(d))
    {
        plot(1,1)
    }
    else
    {
        withProgress(message="Plot cluster",value=0,
        {
            plot(d, xlab="Solutions", ylab="Disimilarity", main="Bray-Curtis dissimilarity of solutions")
        })
    }
}

ExecuteMarxan <- function(sMarxanDir,sExecutable,iCores,iRepsPerCore)
{
    # read input.dat
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

RunMarZone <- function(sMarxanDir,sShinyDataPath,iCores,iRepsPerCore)
{
    withProgress(message="Run MarZone",value=0,
    {
        withProgress(message="MarZone",value=0,
        {
            ExecuteMarxan(sMarxanDir,sExecutable,iCores,iRepsPerCore)
        })

        withProgress(message="Merge results",value=0,
        {
            JoinParallelResults_MarZone(sMarxanDir,iCores,iRepsPerCore,iZones)
        })

        # invalidate cluster object by removing it
        file.remove(paste0(sMarxanDir,"/output/cluster.Rdata"))
    })
}

RunMarZone_app <- function()
{
    RunMarZone(sMarxanDir,sShinyDataPath,iCores,iRepsPerCore)
    
    withProgress(message="Run MarZone",value=0,
    {
        withProgress(message="Prepare display",value=0,
        {
            PrepareDisplay("marzone")
        })
    })
}

RunMarxan <- function(sMarxanDir,sShinyDataPath,iCores,iRepsPerCore)
{
    withProgress(message="Run Marxan",value=0,
    {
        withProgress(message="Marxan",value=0,
        {
            ExecuteMarxan(sMarxanDir,sExecutable,iCores,iRepsPerCore)
        })
        
        withProgress(message="Merge results",value=0,
        {
            JoinParallelResults(sMarxanDir,iCores,iRepsPerCore)
        })
        
        withProgress(message="Populate dbf",value=0,
        {
            ImportOutputsCsvToShpDbf(paste0(sMarxanDir,"/pulayer/pulayer.dbf"),sMarxanDir,round(iCores*iRepsPerCore),"PUID")
        })
        
        withProgress(message="Prepare cluster",value=0,
        {
            PrepareCluster_compute(sMarxanDir)
        })
    })
}

RunMarxan_app <- function()
{
    # save BLM parameter
    inputdat <- readLines(paste0(sMarxanDir,"/input.dat"))
    iBLMparam <- which(regexpr("BLM",inputdat)==1)
    inputdat[iBLMparam] <- paste0("BLM ",rblm)
    writeLines(inputdat,paste0(sMarxanDir,"/input.dat"))
    
    RunMarxan(sMarxanDir,sShinyDataPath,iCores,iRepsPerCore)

    withProgress(message="Run Marxan",value=0,
    {
        withProgress(message="Prepare display",value=0,
        {
            PrepareDisplay("marxan")
        })
    })
}

CreateLogFile <- function(sPath,sID,sCallingApp)
{
    Gfold <- sprintf("%s",round(runif(1)*1000000))
    for (ii in 1:100000)
    {
        sFile <- sprintf("%s/%s_%s_%s.log",sPath,sCallingApp,sID,Gfold)
        if(!file.exists(sFile))
        {
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
        load(file=sSessionKeyFile)
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
    sUserTemp <- paste0(sShinyTempPath,sUserName)
    
    if (sCallingApp == "upload")
    {
        
    } else {
    
        if ((sCallingApp == "marxan") | (sCallingApp == "mxptest"))
        {
            if (.Platform$pkgType == "source")
            {
                sExecutable <<- "MarOpt_v243_Linux64"
            } else {
                sExecutable <<- "MarOpt_v243_Mac64"
            }

            sAppHome <<- paste0(sShinyUserPath,sUserName,"/marxan/")
        
        }
        if (sCallingApp == "marzone")
        {
            if (.Platform$pkgType == "source")
            {
                sExecutable <<- "MarZone_v201_Linux64"
            } else {
                sExecutable <<- "MarZone_v201_Mac64"
            }

            sAppHome <<- paste0(sShinyUserPath,sUserName,"/marzone/")
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
    #sUserSession <<- CreateTempDir(sUserTemp)

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
    sMarxanHome <- paste0(sShinyUserPath,sUserName,"/marxan/")
    sMarZoneHome <- paste0(sShinyUserPath,sUserName,"/marzone/")
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
        # do we need to do RunMarxan 1st ?
        if (!file.exists(paste0(sMarxanDir,"/output/output_sum.csv")))
        {
            RunMarxan(sMarxanDir,sShinyDataPath,iCores,iRepsPerCore)
        }
    }
    
    if (sCallingApp == "marzone")
    {
        # do we need to do RunMarZone 1st ?
        if (!file.exists(paste0(sMarxanDir,"/output/output_sum.csv")))
        {
            RunMarZone(sMarxanDir,sShinyDataPath,iCores,iRepsPerCore)
        }
        
        # generate a list of input files for this dataset
        cat(paste0(paste0(sMarxanDir,"/input.dat"),"\n"))
        inputdat <- readLines(paste0(sMarxanDir,"/input.dat"))
        sZONEBOUNDCOSTNAME <- GetParamValue(inputdat,"ZONEBOUNDCOSTNAME")
        sZONECONTRIBNAME <- GetParamValue(inputdat,"ZONECONTRIBNAME")
        sZONECONTRIB2NAME <- GetParamValue(inputdat,"ZONECONTRIB2NAME")
        sZONETARGETNAME <- GetParamValue(inputdat,"ZONETARGETNAME")
        sZONETARGET2NAME <- GetParamValue(inputdat,"ZONETARGET2NAME")
        input_list <- c("spec","zones","costs","zonecost")
        if (GetParamValue(inputdat,"ZONEBOUNDCOSTNAME") != "")
        {
            input_list <- c(input_list,"zoneboundcost")
        }
        if ((GetParamValue(inputdat,"ZONECONTRIBNAME") != "") | (GetParamValue(inputdat,"ZONECONTRIB2NAME") != ""))
        {
            input_list <- c(input_list,"zonecontrib")
        }
        if ((GetParamValue(inputdat,"ZONETARGETNAME") != "") | (GetParamValue(inputdat,"ZONETARGET2NAME") != ""))
        {
            input_list <- c(input_list,"zonetarget")
        }
        input_list <<- input_list
        cat(paste0(input_list,"\n"))
    }
    
    if (sCallingApp == "mxptest")
    {
        # do we need to do RunMarxan_paramtest 1st for this parameter?
        if (!file.exists(paste0(sMarxanDir,"/output/output_",swhichparam,"summary.csv")))
        {
            RunMarxan_paramtest(swhichparam)
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
    
    SelectDatabase(sCallingApp)
    PrepareDisplay(sCallingApp)

    # save the database name
    writeLines(sSelectDb,paste0(sAppHome,"/database_",sCallingApp,".txt"))

    cat("ChangeDatabase end\n")
}

AddDatabase <- function(sDatabase)
{
    dir.create(sDatabase)
    dir.create(paste0(sDatabase,"/input"))
    dir.create(paste0(sDatabase,"/output"))
    dir.create(paste0(sDatabase,"/pulayer"))
    # copy the marxan files to new directory
    if (fMarZone)
    {
        file.copy(paste0(sUserSession,"/marzone/input.dat"),paste0(sDatabase,"/input.dat"))
        system(paste0("cp ",sUserSession,"/marzone/input/* ",sDatabase,"/input/"))
        system(paste0("cp ",sUserSession,"/marzone/pulayer/* ",sDatabase,"/pulayer/"))
    } else {
        file.copy(paste0(sUserSession,"/marxan/input.dat"),paste0(sDatabase,"/input.dat"))
        system(paste0("cp ",sUserSession,"/marxan/input/* ",sDatabase,"/input/"))
        system(paste0("cp ",sUserSession,"/marxan/pulayer/* ",sDatabase,"/pulayer/"))
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



