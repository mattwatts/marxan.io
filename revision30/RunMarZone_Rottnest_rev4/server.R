# MConsBiol Activity 6

require(shiny)
require(sp)
require(maptools)
require(PBSmapping)
require(foreign)
require(sqldf)
require(vegan)
require(labdsv)
library(foreach)
library(doMC)

registerDoMC(10)  # the number of CPU cores 

cat("\n")
cat(sMarxanDir)
cat("\n")

# initialise objects from R binary files
load(file=paste0(sMarxanDir,"/pulayer/pulayer.Rdata"))
#puoutline <<- puoutline
pulayer <<- pulayer_
pustatus_ <<- pustatus_

PrepareDisplay <- function()
{
    # prepare the map: pulayer object
    pu_table <<- read.dbf(paste0(sMarxanDir,"/pulayer/pulayer.dbf"))

    cat("PrepareDisplay cluster\n")

    # prepare the cluster analysis objects
    solutions_raw<-read.table(paste0(sMarxanDir,"/output/output","_solutionsmatrix.csv"),header=TRUE, row.name=1, sep=",")
    solutions <- unique(solutions_raw)
    iUniqueSolutions <- dim(solutions)[1]
    soldist<-vegdist(solutions,distance="bray")
    sol.mds<<-nmds(soldist,2)
    h<<-hclust(soldist, method="complete")
    
    plotlabels <<- row.names(solutions)

    cat("PrepareDisplay end\n")
}

PrepareDisplay()

eucdist <- function(xloc,yloc,adataframe)
# This function handles the click event on the NMDS plot to identify the solution
# that is nearest the x,y location clicked.
{
    mindistance <- 1000

    for (i in 1:dim(adataframe)[1]){
    
        x1 <- adataframe[i,][1]
        y1 <- adataframe[i,][2]
        distance <- sqrt(((x1 - xloc) ^ 2) + ((y1 - yloc) ^ 2))
        
        if (i==1){
            distances <- c(distance)
        } else {
            distances <- c(distances,distance)
        }
        
        if (distance < mindistance){
            mindistance <- distance
            closestpoint <- i
        }
    }    
    return(closestpoint)
}

GetOutputFileext <- function(sMarxanDir,sParam)
# For the specified Marxan output file, return the file extension (.csv or .txt)
# Scan input.dat for the parameter,
# if value = 1, .dat, tab delimited, no header
# if value = 2, .txt, comma delimited (Qmarxan sets this value)
# if value = 3, .csv, comma delimited
{
  inputdat <- readLines(paste(sMarxanDir,"/input.dat",sep=""))
  iParam <- which(regexpr(sParam,inputdat)==1)
  
  cat(paste0("GetOutputFileext start",sMarxanDir," ",sParam,"\n"))
        
  iValue <- as.integer(unlist(strsplit(inputdat[iParam], split=" "))[2])
  
  if (iValue == 1)
  {
    return(".dat")
  }
  if (iValue == 2)
  {
    return(".txt")
  }
  if (iValue == 3)
  {
    return(".csv")
  }
}

GenerateSolnFilename <- function(iRunNumber,sMarxanDir)
{
  sFilename <- paste(sMarxanDir,"/output/output_r",sep="")  
  iPadding <- 5 - nchar(as.character(iRunNumber))
  if (iPadding > 0)
  {
    for (i in 1:iPadding)
    {
      sFilename <- paste(sFilename,"0",sep="")
    }
  }
  sFilename <- paste(sFilename,iRunNumber,GetOutputFileext(sMarxanDir,"SAVERUN"),sep="")  
}

ImportOutputsCsvToShpDbf <- function(sPuShapeFileDbf, sMarxanDir, iNumberOfRuns, iNumberOfZones, sPUID)
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

labelCol <- function(x)
{
  if (is.leaf(x))
  {
    a <- attributes(x)
    label <- attr(x, "label") 
    colour <- "black"
    if (substring(label,1,2) == "Z2") { colour <- "green" }
    if (substring(label,1,2) == "Z3") { colour <- "blue" }
    #if (label == "ILP") { colour <- "blue"}
    #if (label == paste0("S",iBest," (Best)")) { colour <- "blue" } #"green" }
    #cat(paste0("label ",label,"\n"))
    #
    attr(x, "nodePar") <- c(a$nodePar, lab.col = colour)
  }
  return(x)
}

PadInt <- function(iRunNumber)
{
  sFilename <- ""
  iPadding <- 5 - nchar(as.character(iRunNumber))
  if (iPadding > 0)
  {
    for (i in 1:iPadding)
    {
      sFilename <- paste0(sFilename,"0")
    }
  }
  sFilename <- paste0(sFilename,iRunNumber)  
}

JoinParallelResults <- function()
{
    # combine the summary tables
    sumtable <- c()
    for (i in 1:10)
    {
        sumtable_ <- read.csv(paste0(sMarxanDir,"/output/output",i,"_sum.csv"))
        sumtable <- rbind(sumtable,sumtable_)
    }
    for (i in 1:100)
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
    for (i in 1:10)
    {
        for (j in 1:10)
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
    sZ1Name <- colnames(ssolntable)[3]
    sZ2Name <- colnames(ssolntable)[4]
    sZ3Name <- colnames(ssolntable)[5]
    colnames(ssolntable)[1] <- "PUID"
    colnames(ssolntable)[2] <- "R1N"
    colnames(ssolntable)[3] <- "R1Z1"
    colnames(ssolntable)[4] <- "R1Z2"
    colnames(ssolntable)[5] <- "R1Z3"
    for (i in 2:10)
    {
        ssolntable_ <- read.csv(paste0(sMarxanDir,"/output/output",i,"_ssoln.csv"))
        colnames(ssolntable_)[1] <- "PUID"
        ssolntable <- sqldf("SELECT * from ssolntable LEFT JOIN ssolntable_ USING(PUID)")
        colnames(ssolntable)[ncol(ssolntable)] <- paste0("SS",i)
        colnames(ssolntable)[ncol(ssolntable)-3] <- paste0("R",i,"N")
        colnames(ssolntable)[ncol(ssolntable)-2] <- paste0("R",i,"Z1")
        colnames(ssolntable)[ncol(ssolntable)-1] <- paste0("R",i,"Z2")
        colnames(ssolntable)[ncol(ssolntable)] <- paste0("R",i,"Z3")
    }
    ssolntable$number <- ssolntable$R1N + ssolntable$R2N + ssolntable$R3N + ssolntable$R4N + ssolntable$R5N + ssolntable$R6N + ssolntable$R7N + ssolntable$R8N + ssolntable$R9N + ssolntable$R10N
    ssolntable$Z1 <- ssolntable$R1Z1 + ssolntable$R2Z1 + ssolntable$R3Z1 + ssolntable$R4Z1 + ssolntable$R5Z1 + ssolntable$R6Z1 + ssolntable$R7Z1 + ssolntable$R8Z1 + ssolntable$R9Z1 + ssolntable$R10Z1
    ssolntable$Z2 <- ssolntable$R1Z2 + ssolntable$R2Z2 + ssolntable$R3Z2 + ssolntable$R4Z2 + ssolntable$R5Z2 + ssolntable$R6Z2 + ssolntable$R7Z2 + ssolntable$R8Z2 + ssolntable$R9Z2 + ssolntable$R10Z2
    ssolntable$Z3 <- ssolntable$R1Z3 + ssolntable$R2Z3 + ssolntable$R3Z3 + ssolntable$R4Z3 + ssolntable$R5Z3 + ssolntable$R6Z3 + ssolntable$R7Z3 + ssolntable$R8Z3 + ssolntable$R9Z3 + ssolntable$R10Z3
    ssolntable <- sqldf("SELECT PUID, number, Z1, Z2, Z3 from ssolntable")
    colnames(ssolntable)[1] <- sPUName
    colnames(ssolntable)[3] <- sZ1Name
    colnames(ssolntable)[4] <- sZ2Name
    colnames(ssolntable)[5] <- sZ3Name
    write.csv(ssolntable,
                  paste0(sMarxanDir,"/output/output_ssoln.csv"),
                  quote=FALSE,row.names=FALSE)

    # join cluster files: text parse
    outfile <- file(paste0(sMarxanDir,"/output/output_solutionsmatrix.csv"),"w")
    iRow <- 0
    for (i in 1:10)
    {
        infile <- file(paste0(sMarxanDir,"/output/output",i,"_solutionsmatrix.csv"),"r")
        # read header row
        sLine <- readLines(con=infile,n=1)
  
        # write header row if i == 1
        if (i == 1)
        {
            write(sLine,file=outfile)
        }
    
        for (j in 1:30)
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
    for (i in 1:100)
    {
      for (j in 1:3)
      {
         iInc <- iInc + 1
         solutionsmatrix$SolutionsMatrix[iInc] <- paste0("Z",j,"S",i)
      }
    }
    write.csv(solutionsmatrix,paste0(sMarxanDir,"/output/output_solutionsmatrix.csv"),quote=FALSE,row.names=FALSE)
}

shinyServer(function(input, output, session) {

    cat("shinyServer start\n")
    
    system("touch /srv/shiny-server/RunMarZone_Rottnest_rev4/restart.txt")

    observe({
        rprop <<- input$prop
    })

    observe({
        rspf <<- input$spf
    })
    
    observe({
        sfeature <<- input$feature
        
        # change the target text control for the selected feature
        specdat <- read.csv(paste(sMarxanDir,"/input/spec.dat",sep=""),stringsAsFactors=FALSE)
        for (j in 1:nrow(specdat))
        {
            if (specdat[j,4] == sfeature)
            {
                updateNumericInput(session, "prop", value = specdat[j,2])
                updateNumericInput(session, "spf", value = specdat[j,3])
            }
        }
    })

    observe({
        input$savetargetspf
        cat("savetargetspf\n")
        
        rtarget <- rprop
        if (rtarget < 0)
             { rtarget <- 0 }
        if (rtarget > 1)
            { rtarget <- 1 }
        if (rspf < 0)
            { rspf <- 0 }

        # save target/spf to spec.dat
        specdat <- read.csv(paste(sMarxanDir,"/input/spec.dat",sep=""),stringsAsFactors=FALSE)
        # change the value only for the row with name == input$feature
        for (j in 1:nrow(specdat))
        {
            if (sfeature == "All features")
            {
                cat("saving all features...\n")
                
                specdat[j,2] <- rtarget
                specdat[j,3] <- rspf
                
            } else {
                if (specdat[j,4] == sfeature)
                {
                    specdat[j,2] <- rtarget
                    specdat[j,3] <- rspf
                }
            }
        }
        write.csv(specdat,paste0(sMarxanDir,"/input/spec.dat"),quote=FALSE,row.names=FALSE)
    })

    observe({
        swhichzbc <<- input$whichzbc

        # change the zbc text control for the selected boundary cost
        if (input$whichzbc == "zones 1<->2")
        {
            iZone1 <- 1
            iZone2 <- 2
        }
        if (input$whichzbc == "zones 1<->3")
        {
            iZone1 <- 1
            iZone2 <- 3
        }
        if (input$whichzbc == "zones 2<->3")
        {
            iZone1 <- 2
            iZone2 <- 3
        }

        zoneboundcost <- read.csv(paste0(sMarxanDir,"/input/zoneboundcost.dat"))
        for (j in 1:nrow(zoneboundcost))
        {
            if (iZone1 == zoneboundcost[j,1])
            {
                if (iZone2 == zoneboundcost[j,2])
                {
                    updateNumericInput(session, "zbc", value = zoneboundcost[j,3])
                }
            }
        }        
    })

    runmarxan <- reactive({
        if (input$mrun == 0)
        {
            imrun <<- 0
            cat("init mrun\n")
        }
        else
        {
            if (input$mrun > imrun)
            {
                imrun <<- input$mrun
                cat("mrun incremented\n")
                
                inputdat <- readLines(paste0(sMarxanDir,"/input.dat"))
                randomseeds <- round(runif(10)*100000)
                
                # run Marxan
                foreach(i=1:10) %dopar%
                {
                    # set parameters for multi core
                    iINPUTDIRparam <- which(regexpr("INPUTDIR",inputdat)==1)
                    iOUTPUTDIRparam <- which(regexpr("OUTPUTDIR",inputdat)==1)
                    iSCENNAMEparam <- which(regexpr("SCENNAME",inputdat)==1)
                    iNUMREPSparam <- which(regexpr("NUMREPS",inputdat)==1)
                    iRANDSEEDparam <- which(regexpr("RANDSEED",inputdat)==1)
                    inputdat[iINPUTDIRparam] <- paste0("INPUTDIR ",sMarxanDir,"/input")
                    inputdat[iOUTPUTDIRparam] <- paste0("OUTPUTDIR ",sMarxanDir,"/output")
                    inputdat[iSCENNAMEparam] <- paste0("SCENNAME output",i)         
                    inputdat[iNUMREPSparam] <- "NUMREPS 10"
                    inputdat[iRANDSEEDparam] <- paste0("RANDSEED ",randomseeds[i])
                    writeLines(inputdat,paste0(sMarxanDir,"/core",i,"/input.dat"))
                
                    cat(paste0("getwd ",getwd(),"\n"))
                    setwd(paste0(sMarxanDir,"/core",i))
                    cat(paste0("getwd ",getwd(),"\n"))
                    cat(paste0(".Platform$pkgType ",.Platform$pkgType,"\n"))
                    if (.Platform$pkgType == "source")
                    {
                        cat("linux\n")
                        system("./MarZone_v201_Linux64 -s")
                    } else {
                        cat("mac\n")
                        system("./MarZone_v201_Mac64 -s")
                    }
                }
                
                JoinParallelResults()

                # run Marxan
                #if (.Platform$pkgType == "source")
                #{
                #    system("./MarZone_v201_Linux64 -s")
                #} else {
                #    system("./MarZone_v201_Mac64 -s")
                #}
                
                # write results to pu dbf
                ImportOutputsCsvToShpDbf(paste0(sMarxanDir,"/pulayer/pulayer.dbf"),
                                         sMarxanDir,iNUMREPS,3,"PUID")
                
                # fetch the results
                PrepareDisplay()

                irefreshinput <<- irefreshinput + 1
                updateNumericInput(session, "refreshinput", value = irefreshinput)
            }
        }
    
        return(as.character(input$mrun))
    })

    outputmap <- reactive({
        input$refreshinput
        
        cat('click ')
        cat(str(input$click))
        #cat('hover ')
        #cat(str(input$hover))
        
        if (input$map == "ssolnNmap")
        {
            values <- sqldf(paste("SELECT SSOLN",input$n," from pu_table",sep=""))
            blueramp <- colorRampPalette(c("white","blue"))(16)
            colours <- rep(blueramp[1],nrow(values))
            for (j in 1:nrow(values))
            {
                colours[j] <- blueramp[round(15 / iNUMREPS * values[j,])+1]
            }
            plotPolys(pulayer,col=colours,axes=FALSE,border=NA,cex.lab=0.1,cex.axis=0.1)
        }
        
        if (input$map == "bestmap")
        {
            values <- sqldf("SELECT BESTSOLN from pu_table")
            colourpalette <- c("white","green","blue")
            colours <- rep(colourpalette[1],nrow(values))
            for (j in 1:nrow(values))
            {
                colours[j] <- colourpalette[values[j,]]
            }
            plotPolys(pulayer,col=colours,axes=FALSE,border=NA,cex.lab=0.1,cex.axis=0.1)
        }
        
        if (input$map == "runMmap")
        {
            values <- sqldf(paste("SELECT SOLN",input$m," from pu_table",sep=""))
            colourpalette <- c("white","green","blue")
            colours <- rep(colourpalette[1],nrow(values))
            for (j in 1:nrow(values))
            {
                colours[j] <- colourpalette[values[j,]]
            }
            plotPolys(pulayer,col=colours,axes=FALSE,border=NA,cex.lab=0.1,cex.axis=0.1)
        }
        
        # add the overlay shape files
        if (isTRUE(input$shoreline_fishing))
        {
            addPolys(shoreline_fishing,col="yellow",border=NA)
        }
        if (isTRUE(input$fisheries_notice_301))
        {
            addPolys(fisheries_notice_301,col="red",border=NA)
        }
        if (isTRUE(input$fisheries_notice_332))
        {
            addPolys(fisheries_notice_332,col="red",border=NA)
        }
        if (isTRUE(input$recfish_game))
        {
            addPolys(recfish_game,col="orange",border=NA)
        }
        if (isTRUE(input$recfish_trolling))
        {
            addPolys(recfish_trolling,col="purple",border=NA)
        }
        addLines(outline_rottnest,col="black")
    })

    outputtable <- reactive({
    
        input$refreshinput
        input$savetargetspf

        if ((input$tabletype == "input") & (input$table_i == "spec"))
        {
            thetable <- read.csv(paste0(sMarxanDir,"/input/spec.dat"))
        }
        
        if (input$savezbc == 0)
        {
            isavezbc <<- 0
            cat("init savezbc\n")
        } else {
            if (input$savezbc > isavezbc)
            {
                    isavezbc <<- input$savezbc
                    cat("savezbc incremented\n")
                    
                    # save value to the zoneboundcost table
                    zoneboundcost <- read.csv(paste0(sMarxanDir,"/input/zoneboundcost.dat"))
                    zoneboundcost <- sqldf("SELECT * from zoneboundcost where cost > 0")
                    # change the value only for correct row
                    if (swhichzbc == "zones 1<->2")
                    {
                        iZone1 <- 1
                        iZone2 <- 2
                    }
                    if (swhichzbc == "zones 1<->3")
                    {
                        iZone1 <- 1
                        iZone2 <- 3
                    }
                    if (swhichzbc == "zones 2<->3")
                    {
                        iZone1 <- 2
                        iZone2 <- 3
                    }
                    for (j in 1:nrow(zoneboundcost))
                    {
                        if (iZone1 == zoneboundcost[j,1])
                        {
                            if (iZone2 == zoneboundcost[j,2])
                            {
                                zoneboundcost[j,3] <- input$zbc
                            }
                        }
                    }
                    write.csv(zoneboundcost,paste0(sMarxanDir,"/input/zoneboundcost.dat"),quote=FALSE,row.names=FALSE)
                }
        }

        if ((input$tabletype == "output") & (input$table_o == "sumtable"))
        {
                thetable <- read.csv(paste0(sMarxanDir,"/output/output_sum.csv"))
                colnames(thetable)[5] <- "Zone1_PuCount"
                colnames(thetable)[6] <- "Zone2_PuCount"
                colnames(thetable)[7] <- "Zone3_PuCount"
                thetable <- round(sqldf("SELECT Score, Cost, Zone1_PuCount, Zone2_PuCount, Zone3_PuCount, Shortfall from thetable"))
                iBest <- which.min(thetable$Score)
                Run <- c()
                for (j in 1:nrow(thetable))
                {
                  if (j == iBest)
                  {
                    Run <- c(Run,"Best")
                  } else {
                    Run <- c(Run,j)
                  }
                }        

                thetable <- cbind(Run,thetable)
                thetable$Run <- as.character(thetable$Run)
                for (j in 1:7)
                {
                  thetable[iBest,j] <- HTML(paste0("<FONT COLOR='blue'>",thetable[iBest,j],"</FONT>"))
                }
        }

        if ((input$tabletype == "output") & ((input$table_o == "mvbesttable") || (input$table_o == "mvNtable")))
        {
            if (input$table_o == "mvbesttable")
            {
                    thetable <- read.csv(paste0(sMarxanDir,"/output/output_sum.csv"))
                    thetable <- round(sqldf("SELECT Score, Cost from thetable"))
                    iTable <- which.min(thetable[,1])
            }

            if (input$table_o == "mvNtable")
            {
                    iTable <- input$m
            }
                
            sFilename <- paste(sMarxanDir,"/output/output_mv",sep="")
            iPadding <- 5 - nchar(as.character(iTable))
            if (iPadding > 0)
            {
                for (i in 1:iPadding)
                {
                    sFilename <- paste(sFilename,"0",sep="")
                }
            }
            sFilename <- paste0(sFilename,iTable,".csv")
            thetable <- read.csv(sFilename,stringsAsFactors=FALSE)
            
            # sort the table the way spec.dat is ordered
            tableorder <- c(28,27,26,25,24,23,22,21,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1)
            thetable <- thetable[tableorder,]

            # rename & extract relevant fields
            colnames(thetable)[2] <- "Name"
            colnames(thetable)[4] <- "Total"
            colnames(thetable)[5] <- "AmountHeld"
            colnames(thetable)[8] <- "TargetMet"
            thetable <- sqldf("SELECT Name, Total, Target, AmountHeld, TargetMet, MPM from thetable")

            # compute target gap
            targetgap <- rep(0,each=nrow(thetable))
            for (i in 1:nrow(thetable))
            {
                if (thetable$Target[i] > 0)
                {
                    if (thetable$AmountHeld[i] < thetable$Target[i])
                    {
                        targetgap[i] <- thetable$Target[i] - thetable$AmountHeld[i]
                    }
                }
            }
            # join target gap
            thetable <- cbind(thetable,targetgap)
            colnames(thetable)[7] <- "TargetGap"

            # colour code features that have not met targets
            for (i in 1:nrow(thetable))
            {
                if (thetable[i,5] == "no")
                {
                    for (j in 1:7)
                    {
                        thetable[i,j] <- HTML(paste0("<FONT COLOR='blue'>",thetable[i,j],"</FONT>"))
                    }
                } else {
                    for (j in 1:7)
                    {
                        thetable[i,j] <- HTML(paste0("<FONT COLOR='black'>",thetable[i,j],"</FONT>"))
                    }
                }
            }
        }
        
        if ((input$tabletype == "input") & ((input$table_i == "costs") | (input$table_i == "zones") | (input$table_i == "zonecost")))
        {
            thetable <- read.csv(paste0(sMarxanDir,"/input/",input$table_i,".dat"))
        }
        
        if ((input$tabletype == "input") & (input$table_i == "zonecontrib"))
        {
            thetable <- read.csv(paste0(sMarxanDir,"/input/zonecontrib2.dat"))
        }
        
        if ((input$tabletype == "input") & (input$table_i == "zoneboundcost"))
        {
            zoneboundcost <- read.csv(paste0(sMarxanDir,"/input/zoneboundcost.dat"))
            zoneboundcost <- sqldf("SELECT * from zoneboundcost where cost > 0")
            ZBC <- rbind(c(0,0,0),c(0,0,0),c(0,0,0))
            for (i in (1:nrow(zoneboundcost)))
            {
                ZBC[zoneboundcost[i,1],zoneboundcost[i,2]] <- as.character(zoneboundcost[i,3])
                ZBC[zoneboundcost[i,2],zoneboundcost[i,1]] <- as.character(zoneboundcost[i,3])
            }
            colnames(ZBC) <- c("Available","Partial","Reserved")
            rownames(ZBC) <- c("Available","Partial","Reserved")
            thetable <- ZBC
        }
        
        return(thetable)
    })

    output2ds <- reactive({

        input$refreshinput

        cat("output2ds start\n")

        colourpalette <- c("black","green","blue")
        colours <- rep(colourpalette[1],30)
                
        iCount <- 0
        for (j in 1:10)
        {
            for (i in 1:3)
            {
                iCount <- iCount + 1
                colours[iCount] <- colourpalette[i]
            }
        }

        plot(sol.mds$points, col=colours, xlab='', ylab='', main='NMDS of solutions')
        text(sol.mds$points,labels=plotlabels,pos=4, col=colours)
        #return(plot(sol.mds$points, col=colours, xlab='', ylab='', main='NMDS of solutions'))

        cat("output2ds end\n")
    })
    
    outputdendogram <- reactive({

        input$refreshinput

        cat("outputdendogram start\n")

        d <- dendrapply(as.dendrogram(h), labelCol)
        return(plot(d, xlab="Solutions", ylab="Disimilarity", main="Bray-Curtis dissimilarity of solutions"))
        #return(plot(h, xlab="Solutions", ylab="Disimilarity", main="Bray-Curtis dissimilarity of solutions"))

        cat("outputdendogram end\n")
    })

    output$clickcoordsmap = renderText({
        click=input$clickmap
        sprintf("click map x=%.4g   y=%.4g", click$x, click$y)
    })
 
    output$marxanmap <- renderPlot({
        print(outputmap())
    }, height=450,width=round(450/y_*x_)) # height=450,width=450) # height=450,width=round(450/y_*x_))
    
    output$marxantable <- renderTable({
        dat <- data.frame(outputtable())
        dat
    }, sanitize.text.function = function(x) x)
    
    output$clickcoordsnmds = renderText({
        #click = input$clicknmds
        #if (!is.null(click)){
        #    iClosestPoint <- eucdist(click$x,click$y,sol.mds$points)
        #    sprintf("click nmds closest point %s", row.names(sol.mds$points)[iClosestPoint])
        #    # this finds the closest solution to the x & y point clicked
        #    # an enhancement would be:
        #    #     if solutions are overlapping or nearby, we return a list of solutions with closest one first
        #}
    })
 
    output$plot2ds <- renderPlot({ 
         print(output2ds())
    })
    
    output$plotdendogram <- renderPlot({ 
         print(outputdendogram())
    })
    
    output$buttonfeedback = renderText({
        runmarxan()
        sprintf("Finished")
    })
    
    output$zonename = renderText({
        sprintf(paste0(ZoneNames[input$n]," Zone"))
    })
})

