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
    blueramp <- colorRampPalette(c("white","blue"))(16)
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
                colours[j] <- blueramp[round(15 / 100 * values[j,])+1]
            }
        }
    }
    plotPolys(pulayer,col=colours,axes=FALSE,border=NA,cex.lab=0.1,cex.axis=0.1)
}

map_bestmap <- function()
{
    values <- sqldf("SELECT BESTSOLN from pu_table")
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

map_runMmap <- function()
{
    solnX_table <- read.csv(GenerateSolnFilename(iM,sMarxanDir))
    values <- sqldf(paste("SELECT SOLUTION from solnX_table",sep="")) + 1
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

    cat("InitialiseUser end\n")
}

ChangeDatabase <- function()
{
    cat("ChangeDatabase start\n")

    # do we need to do RunMarxan_1st ?
    if (length(list.files(path=paste0(sMarxanDir,"/output"))) == 0)
    {
        RunMarxan_1st(sMarxanDir,sShinyDataPath,10,10)
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

