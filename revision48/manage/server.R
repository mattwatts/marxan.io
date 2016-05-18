# marxan.io

library(shiny)
require(sp)
require(maptools)
require(PBSmapping)
require(foreign)
require(sqldf)
require(vegan)
require(labdsv)
require(xtable)
library(foreach)
library(doMC)
library(rhandsontable)
library(iptools)
library(png)
library(rjson)

registerDoMC(iRepsPerCore)  # the number of CPU cores

Logged = FALSE;

is_dataset_shared_publicly <- function()
{
    sPublicDir <- paste0(sShinyDataPath,"/public")

    if (fMarxan)
    {
        sShareDir <- paste0(sPublicDir,"/", sUserName,"/marxan")
    } else {
        sShareDir <- paste0(sPublicDir,"/", sUserName,"/marzone")
    }

    return(file.exists(paste0(sShareDir,"/",sSelectDb)))
}

share_dataset_publicly <- function()
{
    sPublicDir <- paste0(sShinyDataPath,"/public")
    dir.create(sPublicDir)
    dir.create(paste0(sPublicDir,"/", sUserName))
    dir.create(paste0(sPublicDir,"/", sUserName,"/marxan"))
    dir.create(paste0(sPublicDir,"/", sUserName,"/marzone"))

    if (fMarxan)
    {
        sShareDir <- paste0(sPublicDir,"/", sUserName,"/marxan")
    } else {
        sShareDir <- paste0(sPublicDir,"/", sUserName,"/marzone")
    }

    system(paste0("cp -rf ",sMarxanDir," ",sShareDir))
}

remove_dataset_public_share <- function()
{
    sPublicDir <- paste0(sShinyDataPath,"/public")

    if (fMarxan)
    {
        sShareDir <- paste0(sPublicDir,"/", sUserName,"/marxan")
    } else {
        sShareDir <- paste0(sPublicDir,"/", sUserName,"/marzone")
    }

    sPublicShare <- paste0(sShareDir,"/",sSelectDb)
    if (file.exists(sPublicShare))
    {
        system(paste0("rm -rf ",sPublicShare))
    }
}

shinyServer(function(input, output, session, clientData) {

    observe({
        sUserIP <<- as.character(input$ipid)
        cat(paste0("sUserIP ",sUserIP,"\n"))
    })

    observe({
        sFingerprint <<- as.character(input$fingerprint)
        cat(paste0("sFingerprint ",sFingerprint,"\n"))
    })

    source(paste0(sAppDir,"/authenticate.R"),  local = TRUE)
    values = list()
    setHot = function(x) values[["hot"]] <<- x  

    source(paste0(sShinySourcePath,"/server_pre_marxan.R"),  local = TRUE)

    autoInvalidate <- reactiveTimer(2000,session=session)

    observe({

        if (USER$Logged == TRUE)
        {
            autoInvalidate()

            list_dirs <- c(list.dirs(sMarxanHome,full.names = TRUE),
                           list.dirs(sMarZoneHome,full.names = TRUE))

            # we detect if there are new folders in the users directory, indicating a new database import
            CurrentImportTime <- max(file.info(list_dirs)$ctime)
            if (!(CurrentImportTime == ImportTime))
            {
                # user has imported a new dataset
                cat(paste0("new dataset detected","\n"))
                ImportTime <<- CurrentImportTime

                # update the list of datasets to include the new one(s)
                updateSelectInput(session, "database",
                                  choices = c(list.dirs(sMarxanHome),list.dirs(sMarZoneHome)),
                                  selected = sSelectDb)
                                  
                # trigger a refresh of the UI
                irefreshtable <<- irefreshtable + 1
                updateNumericInput(session, "refreshtable", value = irefreshtable)
                updateNumericInput(session,"areyousure",value=0)
            }
        }
    })

    observe({

        if (USER$Logged == TRUE)
        {
            # render the user interface
            source(paste0(sAppDir,"/render_ui.R"),  local = TRUE)
        } # if
    })

    observe({

        if (USER$Logged == TRUE)
        {
            if (!is.null(input$publicdb))
            {
                # select this database from the list of databases
                sSelectPublicDb <<- input$publicdb
                cat(paste0("sSelectPublicDb ",sSelectPublicDb,"\n"))
                y <- strsplit(sSelectPublicDb,"/")
                sSelectPublicUser <<- y[[1]][1]
                sSelectPublicType <<- y[[1]][2]
                sSelectPublicDatabase <<- y[[1]][3]
                updateNumericInput(session,"copypublicdata",value=0)
            }
        }
    })

    observe({

        if (USER$Logged == TRUE)
        {
            if (!is.null(input$database))
            {
                # select this database from the list of databases
                sSelectDb <<- input$database
                cat(paste0("sSelectDb ",sSelectDb,"\n"))
                sPrevious <- sMarxanDir
                sMarxanDir <<- paste0(sMarxanHome,"/",sSelectDb)
                sZipWD <<- paste0(sMarxanHome)
                fMarxan <<- TRUE
                fMarZone <<- FALSE
                if (!file.exists(sMarxanDir))
                {
                    sMarxanDir <<- paste0(sMarZoneHome,"/",sSelectDb)
                    sZipWD <<- paste0(sMarZoneHome)
                    fMarxan <<- FALSE
                    fMarZone <<- TRUE
                }
                cat(paste0("sMarxanDir ",sMarxanDir,"\n"))
                AppendLogFile(sLogFile,paste0("sSelectDb ",sSelectDb))
                AppendLogFile(sLogFile,paste0("sMarxanDir ",sMarxanDir))

                if (sPrevious != sMarxanDir)
                {
                    if (sSelectDb != "")
                    {
                        #ChangeDatabase("marxan")

                        # update the relevant UI components
                        # trigger a refresh of the marxan UI
                        # trigger a refresh of the cluster
                        #irefreshcluster <<- irefreshcluster + 1
                        #updateNumericInput(session, "refreshcluster", value = irefreshcluster)
                        updateNumericInput(session,"areyousure",value=0)
                    }
                }
            }
        }
    })

    observe({
        fWindowsEOLN <<- input$windowseoln
    })

    observe({
        fWindowsEOLNPublic <<- input$windowseolnPublic
    })

    output$downloadPublic <- downloadHandler(
        filename = function()
        {
            paste0(sSelectPublicDatabase, '.zip')
        },
        content = function(file) {

            withProgress(message="Generating download",value=0,
            {
                # remove existing zip file
                sZipFile <- paste0(sAppHome,"/",sSelectPublicDatabase,".zip")
                if (file.exists(sZipFile))
                {
                    file.remove(sZipFile)
                }

                # create temp directory
                sTempDir <- paste0(sShinyTempPath,"/",sUserName)
                dir.create(sTempDir)
                
                sMxDir <- paste0(sShinyDataPath,"/public/",sSelectPublicDb)

                # copy files to temp directory
                system(paste0("rm -rf ",sTempDir,"/",sSelectPublicDatabase))
                system(paste0("cp -rf ",sMxDir," ",sTempDir))
                system(paste0("cp -f ",sTempDir,"/",sSelectPublicDatabase,"/core1/*.csv ",sTempDir,"/",sSelectPublicDatabase))
                system(paste0("cp -f ",sTempDir,"/",sSelectPublicDatabase,"/core1/*.txt ",sTempDir,"/",sSelectPublicDatabase))

                # remove unnecessary files
                system(paste0("rm -rf ",sTempDir,"/",sSelectPublicDatabase,"/core*"))
                system(paste0("rm ",sTempDir,"/",sSelectPublicDatabase,"/BLM.csv"))
                system(paste0("rm ",sTempDir,"/",sSelectPublicDatabase,"/SPF.csv"))
                system(paste0("rm ",sTempDir,"/",sSelectPublicDatabase,"/Targ.csv"))
                system(paste0("rm ",sTempDir,"/",sSelectPublicDatabase,"/input/specBLM*.dat"))
                system(paste0("rm ",sTempDir,"/",sSelectPublicDatabase,"/input/specSPF*.dat"))
                system(paste0("rm ",sTempDir,"/",sSelectPublicDatabase,"/input/specTarg*.dat"))
                for (i in 1:10)
                {
                    system(paste0("rm ",sTempDir,"/",sSelectPublicDatabase,"/output/output",i,"*.csv"))
                    system(paste0("rm ",sTempDir,"/",sSelectPublicDatabase,"/output/output",i,"*.dat"))
                    system(paste0("rm ",sTempDir,"/",sSelectPublicDatabase,"/output/outputBLM",i,"*.csv"))
                    system(paste0("rm ",sTempDir,"/",sSelectPublicDatabase,"/output/outputBLM",i,"*.dat"))
                    system(paste0("rm ",sTempDir,"/",sSelectPublicDatabase,"/output/outputSPF",i,"*.csv"))
                    system(paste0("rm ",sTempDir,"/",sSelectPublicDatabase,"/output/outputSPF",i,"*.dat"))
                    system(paste0("rm ",sTempDir,"/",sSelectPublicDatabase,"/output/outputTarg",i,"*.csv"))
                    system(paste0("rm ",sTempDir,"/",sSelectPublicDatabase,"/output/outputTarg",i,"*.dat"))
                    system(paste0("rm ",sTempDir,"/",sSelectPublicDatabase,"/output/output_BLMsummary",i,".csv"))
                    system(paste0("rm ",sTempDir,"/",sSelectPublicDatabase,"/output/output_SPFsummary",i,".csv"))
                    system(paste0("rm ",sTempDir,"/",sSelectPublicDatabase,"/output/output_Targsummary",i,".csv"))
                    system(paste0("rm ",sTempDir,"/",sSelectPublicDatabase,"/output/output_BLMsummary.csv"))
                    system(paste0("rm ",sTempDir,"/",sSelectPublicDatabase,"/output/output_SPFsummary.csv"))
                    system(paste0("rm ",sTempDir,"/",sSelectPublicDatabase,"/output/output_Targsummary.csv"))
                }

                # convert windows eoln
                if (fWindowsEOLNPublic)
                {
                    system(paste0("unix2dos ",sTempDir,"/",sSelectPublicDatabase,"/input.dat"))
                    system(paste0("unix2dos ",sTempDir,"/",sSelectPublicDatabase,"/*.txt"))
                    system(paste0("unix2dos ",sTempDir,"/",sSelectPublicDatabase,"/*.csv"))
                    system(paste0("unix2dos ",sTempDir,"/",sSelectPublicDatabase,"/input/*"))
                    system(paste0("unix2dos ",sTempDir,"/",sSelectPublicDatabase,"/output/*.csv"))
                    system(paste0("unix2dos ",sTempDir,"/",sSelectPublicDatabase,"/output/*.dat"))
                }

                sWD <- getwd()
                setwd(sTempDir)

                # create new zip file
                system(paste0("zip -r ",sZipFile," ",sSelectPublicDatabase))

                setwd(sWD)
            }) 

            file.copy(sZipFile,file)
        }
    )

    output$downloadData <- downloadHandler(
        filename = function()
        {
            paste0(sSelectDb, '.zip')
        },
        content = function(file) {

            withProgress(message="Generating download",value=0,
            {
                # remove existing zip file
                sZipFile <- paste0(sAppHome,"/",sSelectDb,".zip")
                if (file.exists(sZipFile))
                {
                    file.remove(sZipFile)
                }

                # create temp directory
                sTempDir <- paste0(sShinyTempPath,"/",sUserName)
                dir.create(sTempDir)

                # copy files to temp directory
                system(paste0("rm -rf ",sTempDir,"/",sSelectDb))
                system(paste0("cp -rf ",sMarxanDir," ",sTempDir))
                system(paste0("cp -f ",sTempDir,"/",sSelectDb,"/core1/*.csv ",sTempDir,"/",sSelectDb))
                system(paste0("cp -f ",sTempDir,"/",sSelectDb,"/core1/*.txt ",sTempDir,"/",sSelectDb))

                # remove unnecessary files
                system(paste0("rm -rf ",sTempDir,"/",sSelectDb,"/core*"))
                system(paste0("rm ",sTempDir,"/",sSelectDb,"/BLM.csv"))
                system(paste0("rm ",sTempDir,"/",sSelectDb,"/SPF.csv"))
                system(paste0("rm ",sTempDir,"/",sSelectDb,"/Targ.csv"))
                system(paste0("rm ",sTempDir,"/",sSelectDb,"/input/specBLM*.dat"))
                system(paste0("rm ",sTempDir,"/",sSelectDb,"/input/specSPF*.dat"))
                system(paste0("rm ",sTempDir,"/",sSelectDb,"/input/specTarg*.dat"))
                for (i in 1:10)
                {
                    system(paste0("rm ",sTempDir,"/",sSelectDb,"/output/output",i,"*.csv"))
                    system(paste0("rm ",sTempDir,"/",sSelectDb,"/output/output",i,"*.dat"))
                    system(paste0("rm ",sTempDir,"/",sSelectDb,"/output/outputBLM",i,"*.csv"))
                    system(paste0("rm ",sTempDir,"/",sSelectDb,"/output/outputBLM",i,"*.dat"))
                    system(paste0("rm ",sTempDir,"/",sSelectDb,"/output/outputSPF",i,"*.csv"))
                    system(paste0("rm ",sTempDir,"/",sSelectDb,"/output/outputSPF",i,"*.dat"))
                    system(paste0("rm ",sTempDir,"/",sSelectDb,"/output/outputTarg",i,"*.csv"))
                    system(paste0("rm ",sTempDir,"/",sSelectDb,"/output/outputTarg",i,"*.dat"))
                    system(paste0("rm ",sTempDir,"/",sSelectDb,"/output/output_BLMsummary",i,".csv"))
                    system(paste0("rm ",sTempDir,"/",sSelectDb,"/output/output_SPFsummary",i,".csv"))
                    system(paste0("rm ",sTempDir,"/",sSelectDb,"/output/output_Targsummary",i,".csv"))
                    system(paste0("rm ",sTempDir,"/",sSelectDb,"/output/output_BLMsummary.csv"))
                    system(paste0("rm ",sTempDir,"/",sSelectDb,"/output/output_SPFsummary.csv"))
                    system(paste0("rm ",sTempDir,"/",sSelectDb,"/output/output_Targsummary.csv"))
                }

                # convert windows eoln
                if (fWindowsEOLN)
                {
                    system(paste0("unix2dos ",sTempDir,"/",sSelectDb,"/input.dat"))
                    system(paste0("unix2dos ",sTempDir,"/",sSelectDb,"/*.txt"))
                    system(paste0("unix2dos ",sTempDir,"/",sSelectDb,"/*.csv"))
                    system(paste0("unix2dos ",sTempDir,"/",sSelectDb,"/input/*"))
                    system(paste0("unix2dos ",sTempDir,"/",sSelectDb,"/output/*.csv"))
                    system(paste0("unix2dos ",sTempDir,"/",sSelectDb,"/output/*.dat"))
                }

                sWD <- getwd()
                setwd(sTempDir)

                # create new zip file
                system(paste0("zip -r ",sZipFile," ",sSelectDb))

                setwd(sWD)
            }) 

            file.copy(sZipFile,file)
        }
    )

    observe({
        if (USER$Logged == TRUE)
        if (!is.null(input$deletedb))
        {
            if (input$deletedb > 0)
            {
                # user has pressed delete
                cat(paste0("delete clicked ",input$deletedb,"\n"))
                if (is_dataset_shared_publicly())
                {
                    # can't delete a dataset that is shared. You must remove the share first
                    withProgress(message=paste0("Can't delete ",sSelectDb," as it's shared publicly. Remove share first"),value=0,min=0,max=20, { Sys.sleep(5) })
                } else
                {
                    updateNumericInput(session,"areyousure",value=1)
                }
            }
        }
    })

    observe({
        if (USER$Logged == TRUE)
        if (!is.null(input$cancelDelete))
        {
            if (input$cancelDelete > 0)
            {
                updateNumericInput(session,"areyousure",value=0)
            }
        }
    })

    observe({
        # click rename
        if (USER$Logged == TRUE)
        if (!is.null(input$renameData))
        {
            if (input$renameData > 0)
            {
                cat(paste0("delete clicked ",input$deletedb,"\n"))
                if (is_dataset_shared_publicly())
                {
                    # can't rename a dataset that is shared. You must remove the share first
                    withProgress(message=paste0("Can't rename ",sSelectDb," as it's shared publicly. Remove share first"),value=0,min=0,max=20, { Sys.sleep(5) })
                } else
                {
                    updateNumericInput(session,"renamemydata",value=1)
                    updateTextInput(session,"renameName",value=isolate(sSelectDb))
                }
            }
        }
    })

    observe({
        # click accept rename
        if (USER$Logged == TRUE)
        if (!is.null(input$acceptName))
        {
            if (input$acceptName > 0)
            {
                # does the new name already exist?
                if (fMarxan)
                {
                    sNewNameDb <- paste0(sMarxanHome,"/",sRenameName)
                } else {
                    sNewNameDb <- paste0(sMarZoneHome,"/",sRenameName)
                }
                if (file.exists(sNewNameDb))
                {
                    # can't rename. name already exists
                    withProgress(message=paste0("Can't rename. New name ",sRenameName," already exists"),value=0,min=0,max=20, { Sys.sleep(5) })
                } else {
                    # rename the dataset
                    file.rename(sMarxanDir,sNewNameDb)

                    sOldName <- sSelectDb
                    sSelectDb <<- sRenameName
                    if (fMarxan)
                    {
                        sMarxanDir <<- paste0(sMarxanHome,"/",sSelectDb)
                        sZipWD <<- paste0(sMarxanHome)
                    } else {
                        sMarxanDir <<- paste0(sMarZoneHome,"/",sSelectDb)
                        sZipWD <<- paste0(sMarZoneHome)
                    }
                    
                    # trigger a refresh of the "My data" table & "database"
                    irefreshtable <<- irefreshtable + 1
                    updateNumericInput(session, "refreshtable", value = irefreshtable)
                    updateSelectInput(session, "database",
                                      choices = c(list.dirs(sMarxanHome),list.dirs(sMarZoneHome)),
                                      selected = sSelectDb)

                    withProgress(message=paste0("Dataset ",sOldName," renamed to ",sSelectDb),value=0,min=0,max=20, { Sys.sleep(5) })
                    updateNumericInput(session,"renamemydata",value=0)
                }
                
            }
        }
    })

    observe({
        # click cancel rename
        if (USER$Logged == TRUE)
        if (!is.null(input$cancelName))
        {
            if (input$cancelName > 0)
            {
                updateNumericInput(session,"renamemydata",value=0)
            }
        }
    })

    observe({
        # click share public
        if (USER$Logged == TRUE)
        if (!is.null(input$sharePublic))
        {
            if (input$sharePublic > 0)
            {
                updateNumericInput(session,"sharemydata",value=0)
                updateNumericInput(session,"sharemydatapublic",value=1)
            }
        }
    })

    observe({
        # click share public ok
        if (USER$Logged == TRUE)
        if (!is.null(input$publicOk))
        {
            if (input$publicOk > 0)
            {
                updateNumericInput(session,"sharemydatapublic",value=0)
                updateNumericInput(session,"sharemydata",value=1)
                if (is_dataset_shared_publicly())
                {
                    withProgress(message=paste0("Dataset ",sSelectDb," is already shared publicly"),value=0,min=0,max=20, { Sys.sleep(5) })
                } else {
                    share_dataset_publicly()
                    withProgress(message=paste0("Dataset ",sSelectDb," shared publicly"),value=0,min=0,max=20, { Sys.sleep(5) })
                }
            }
        }
    })

    observe({
        # click share public cancel
        if (USER$Logged == TRUE)
        if (!is.null(input$publicCancel))
        {
            if (input$publicCancel > 0)
            {
                updateNumericInput(session,"sharemydatapublic",value=0)
                updateNumericInput(session,"sharemydata",value=1)
            }
        }
    })

    observe({
        # click remove public share
        if (USER$Logged == TRUE)
        if (!is.null(input$removeShare))
        {
            if (input$removeShare > 0)
            {
                updateNumericInput(session,"sharemydata",value=0)
                updateNumericInput(session,"sharemydataremove",value=1)
            }
        }
    })

    observe({
        # click remove public share yes
        if (USER$Logged == TRUE)
        if (!is.null(input$removeOk))
        {
            if (input$removeOk > 0)
            {
                updateNumericInput(session,"sharemydataremove",value=0)
                updateNumericInput(session,"sharemydata",value=1)
                if (is_dataset_shared_publicly())
                {
                    remove_dataset_public_share()
                    withProgress(message=paste0("Dataset ",sSelectDb," public share removed"),value=0,min=0,max=20, { Sys.sleep(5) })
                } else {
                    withProgress(message=paste0("Dataset ",sSelectDb," is not shared publicly"),value=0,min=0,max=20, { Sys.sleep(5) })
                }

            }
        }
    })

    observe({
        # click remove public share cancel
        if (USER$Logged == TRUE)
        if (!is.null(input$removeCancel))
        {
            if (input$removeCancel > 0)
            {
                updateNumericInput(session,"sharemydataremove",value=0)
                updateNumericInput(session,"sharemydata",value=1)
            }
        }
    })

    observe({
        # click share private
        if (USER$Logged == TRUE)
        if (!is.null(input$sharePrivate))
        {
            if (input$sharePrivate > 0)
            {
                updateNumericInput(session,"sharemydata",value=0)
                updateNumericInput(session,"sharemydataprivate",value=1)
            }
        }
    })

    observe({
        # click share private ok
        if (USER$Logged == TRUE)
        if (!is.null(input$privateOk))
        {
            if (input$privateOk > 0)
            {
                updateNumericInput(session,"sharemydataprivate",value=0)
                updateNumericInput(session,"sharemydata",value=1)
            }
        }
    })

    observe({
        # click share private cancel
        if (USER$Logged == TRUE)
        if (!is.null(input$privateCancel))
        {
            if (input$privateCancel > 0)
            {
                updateNumericInput(session,"sharemydataprivate",value=0)
                updateNumericInput(session,"sharemydata",value=1)
            }
        }
    })

    observe({
        # click share edit
        if (USER$Logged == TRUE)
        if (!is.null(input$editShare))
        {
            if (input$editShare > 0)
            {
                updateNumericInput(session,"sharemydata",value=0)
                updateNumericInput(session,"sharemydataedit",value=1)
            }
        }
    })

    observe({
        # click share edit ok
        if (USER$Logged == TRUE)
        if (!is.null(input$editOk))
        {
            if (input$editOk > 0)
            {
                updateNumericInput(session,"sharemydataedit",value=0)
                updateNumericInput(session,"sharemydata",value=1)
            }
        }
    })

    observe({
        # click share edit cancel
        if (USER$Logged == TRUE)
        if (!is.null(input$editCancel))
        {
            if (input$editCancel > 0)
            {
                updateNumericInput(session,"sharemydataedit",value=0)
                updateNumericInput(session,"sharemydata",value=1)
            }
        }
    })

    observe({
        if (USER$Logged == TRUE)
        if (!is.null(input$copyPublic))
        {
            if (input$copyPublic > 0)
            {
                updateNumericInput(session,"copypublicdata",value=1)
                updateTextInput(session,"publicName",value=sSelectPublicDatabase)
            }
        }
    })
    
    observe({
        sPublicName <<- input$publicName
    })
    
    observe({
        sRenameName <<- input$renameName
    })

    observe({
        if (USER$Logged == TRUE)
        if (!is.null(input$acceptPublic))
        {
            if (input$acceptPublic > 0)
            {
                if (sSelectPublicType == "marxan")
                {
                    sDestDb <- paste0(sMarxanHome)
                } else {
                    sDestDb <- paste0(sMarZoneHome)
                }

                if (file.exists(paste0(sDestDb,"/",sPublicName)))
                {
                    # dataset already exists - choose another name
                    withProgress(message=paste0("Choose a new name - dataset ",sSelectPublicDatabase," already exists"),value=0,min=0,max=20, { Sys.sleep(5) })
                    
                } else {
                    # copy the dataset
                    dir.create(paste0(sDestDb,"/",sPublicName))
                    sPublicDb <- paste0(sShinyDataPath,"/public/",sSelectPublicDb)
                    sSystemCmd <- paste0("cp -rf ",sPublicDb,"/* ",sDestDb,"/",sPublicName,"/")
                    cat(paste0("sSystemCmd ",sSystemCmd,"\n"))
                    system(sSystemCmd)
                    updateNumericInput(session,"copypublicdata",value=0)
                    withProgress(message=paste0("Public dataset ",sSelectPublicDatabase," copied"),value=0,min=0,max=20, { Sys.sleep(5) })
                }
            }
        }
    })

    observe({
        if (USER$Logged == TRUE)
        if (!is.null(input$cancelPublic))
        {
            if (input$cancelPublic > 0)
            {
                updateNumericInput(session,"copypublicdata",value=0)
            }
        }
    })

    observe({
        if (USER$Logged == TRUE)
        if (!is.null(input$copyPrivate))
        {
            if (input$copyPrivate > 0)
            {
                updateNumericInput(session,"copyprivatedata",value=1)
            }
        }
    })

    observe({
        if (USER$Logged == TRUE)
        if (!is.null(input$acceptPrivate))
        {
            if (input$acceptPrivate > 0)
            {
                updateNumericInput(session,"copyprivatedata",value=0)
            }
        }
    })

    observe({
        if (USER$Logged == TRUE)
        if (!is.null(input$cancelPrivate))
        {
            if (input$cancelPrivate > 0)
            {
                updateNumericInput(session,"copyprivatedata",value=0)
            }
        }
    })

    observe({
        if (USER$Logged == TRUE)
        if (!is.null(input$yesimsure))
        {
            if (input$yesimsure > 0)
            {
                # user has pressed yesimsure
                cat("yesimsure clicked\n")
                
                if (is_dataset_shared_publicly())
                {
                    # can't delete a dataset that is shared. You must remove the share first
                    withProgress(message=paste0("Can't delete ",sSelectDb," as it's shared publicly. Remove share first"),value=0,min=0,max=20, { Sys.sleep(5) })
                    updateNumericInput(session,"areyousure",value=0)
                } else
                {
                    cat(paste0("deleting ",sMarxanDir,"\n"))

                    # erase the database
                    system(paste0("rm -rf ",sMarxanDir))

                    system(paste0("touch ",sMarxanHome))
                    system(paste0("touch ",sMarZoneHome))

                    # refresh dataset list
                    list_dirs <- c(list.dirs(sMarxanHome,full.names = TRUE),
                                   list.dirs(sMarZoneHome,full.names = TRUE))
                    cat(paste0("new dataset detected","\n"))
                    ImportTime <<-max(file.info(list_dirs)$ctime)

                    a_choices <- c(list.dirs(sMarxanHome),list.dirs(sMarZoneHome))
                    # update the list of datasets to include the new one(s)
                    updateSelectInput(session, "database",
                                      choices = a_choices,
                                      selected = a_choices[1])

                    # trigger a refresh of the UI
                    irefreshtable <<- irefreshtable + 1
                    updateNumericInput(session, "refreshtable", value = irefreshtable)
                    updateNumericInput(session,"areyousure",value=0)

                    # display a message to user for 5 seconds
                    withProgress(message=paste0("Deleted ",sSelectDb),value=0,min=0,max=20, { Sys.sleep(5) })
                }
            }
        }
    })

    output$mydatatable <- renderTable({

        input$publicOk
        input$removeOk

        if (USER$Logged == TRUE)
        {
            input$refreshtable

            # parse the marxan and marzone databases, listing them in the grid
            col_names <- c("name","type","used","shared","planning_units","features","polygons","leaflet","zones","costs","created","last_run")
            list_dirs_mx <- list.dirs(sMarxanHome,full.names=FALSE)
            list_dirs_mz <- list.dirs(sMarZoneHome,full.names=FALSE)
            for (i in 1:length(list_dirs_mx))
            {
                # read stats for this database
                sMarxanDir <- paste0(sMarxanHome,"/",list_dirs_mx[i],"/") 
                sName <- list_dirs_mx[i]
                sType <- "marxan"
                sPuRdataFile <- paste0(sMarxanDir,"/pulayer/pulayer.Rdata")
                sCreated <- as.character(file.info(sPuRdataFile)$ctime)
                sSumFile <- paste0(sMarxanDir,"/output/output_sum.csv")
                fUsed <- file.exists(sSumFile)
                if (fUsed)
                {
                    sLastUsed <- as.character(file.info(sSumFile)$ctime)
                } else {
                    sLastUsed <- ""
                }
                pudat <- read.csv(paste0(sMarxanDir,"/input/pu.dat"),stringsAsFactors=FALSE)
                sPlanningUnits <- nrow(pudat)
                specdat <- read.csv(paste0(sMarxanDir,"/input/spec.dat"),stringsAsFactors=FALSE)
                sFeatures <- nrow(specdat)

                putable <- read.dbf(paste0(sMarxanDir,"/pulayer/pulayer.dbf"))
                sPolygons <- nrow(putable)
                sZones <- ""
                sCosts <- ""

                fLeaflet <- file.exists(paste0(sMarxanDir,"/pulayer/leaflet.Rdata"))
                fShared <- file.exists(paste0(sShinyDataPath,"/public/",sUserName,"/marxan/",sName))
                if (fShared)
                {
                    sShared <- "public"
                } else {
                    sShared <- ""
                }

                a_row <- c(sName,sType,as.character(fUsed),sShared,sPlanningUnits,sFeatures,sPolygons,as.character(fLeaflet),sZones,sCosts,sCreated,sLastUsed)
                if (i == 1)
                {
                    the_table <- a_row
                } else {
                    the_table <- rbind(the_table,a_row)
                }
            }
            for (i in 1:length(list_dirs_mz))
            {
                # read stats for this database
                sMarxanDir <- paste0(sMarZoneHome,"/",list_dirs_mz[i],"/") 
                sName <- list_dirs_mz[i]
                sType <- "marzone"
                sCreated <- as.character(file.info(sMarxanDir)$ctime)
                sSumFile <- paste0(sMarxanDir,"/output/output_sum.csv")
                fUsed <- file.exists(sSumFile)
                if (fUsed)
                {
                    sLastUsed <- as.character(file.info(sSumFile)$ctime)
                } else {
                    sLastUsed <- ""
                }
                pudat <- read.csv(paste0(sMarxanDir,"/input/pu.dat"),stringsAsFactors=FALSE)
                sPlanningUnits <- nrow(pudat)
                specdat <- read.csv(paste0(sMarxanDir,"/input/spec.dat"),stringsAsFactors=FALSE)
                sFeatures <- nrow(specdat)

                putable <- read.dbf(paste0(sMarxanDir,"/pulayer/pulayer.dbf"))
                sPolygons <- nrow(putable)
                zonesdat <- read.csv(paste0(sMarxanDir,"/input/zones.dat"),stringsAsFactors=FALSE)
                sZones <- nrow(zonesdat)
                costsdat <- read.csv(paste0(sMarxanDir,"/input/costs.dat"),stringsAsFactors=FALSE)
                sCosts <- nrow(costsdat)

                fLeaflet <- file.exists(paste0(sMarxanDir,"/pulayer/leaflet.Rdata"))
                fShared <- file.exists(paste0(sShinyDataPath,"/public/",sUserName,"/marzone/",sName))
                if (fShared)
                {
                    sShared <- "public"
                } else {
                    sShared <- ""
                }

                a_row <- c(sName,sType,as.character(fUsed),sShared,sPlanningUnits,sFeatures,sPolygons,as.character(fLeaflet),sZones,sCosts,sCreated,sLastUsed)
                the_table <- rbind(the_table,a_row)
            }
            colnames(the_table) <- col_names
            rownames(the_table) <- rep("",nrow(the_table))

            return(the_table)
        }
    })

    output$publictable <- renderTable({

        input$publicOk
        input$removeOk

        if (USER$Logged == TRUE)
        {
            sPublicDir <- paste0(sShinyDataPath,"/public")

            sMxHome <- paste0(sPublicDir,"/marxan")
            sMzHome <- paste0(sPublicDir,"/marzone")

            list_users <- list.dirs(sPublicDir,full.names=FALSE)

            for (j in 1:length(list_users))
            {
                sUser <- list_users[j]

                sMxHome <- paste0(sPublicDir,"/",sUser,"/marxan")
                sMzHome <- paste0(sPublicDir,"/",sUser,"/marzone")

                col_names <- c("user","name","type","planning_units","features","polygons","leaflet","zones","costs","shared")

                list_dirs_mx <- list.dirs(sMxHome,full.names=FALSE)
                list_dirs_mz <- list.dirs(sMzHome,full.names=FALSE)
                list_dirs_mx
                list_dirs_mz

                for (i in 1:length(list_dirs_mx))
                {
                    # read stats for this database
                    sMarxanDir <- paste0(sMxHome,"/",list_dirs_mx[i],"/") 
                    sName <- list_dirs_mx[i]
                    sType <- "marxan"
                    sPuRdataFile <- paste0(sMarxanDir,"/pulayer/pulayer.Rdata")
                    sCreated <- as.character(file.info(sPuRdataFile)$ctime)
                    pudat <- read.csv(paste0(sMarxanDir,"/input/pu.dat"),stringsAsFactors=FALSE)
                    sPlanningUnits <- nrow(pudat)
                    specdat <- read.csv(paste0(sMarxanDir,"/input/spec.dat"),stringsAsFactors=FALSE)
                    sFeatures <- nrow(specdat)

                    putable <- read.dbf(paste0(sMarxanDir,"/pulayer/pulayer.dbf"))
                    sPolygons <- nrow(putable)
                    sZones <- ""
                    sCosts <- ""

                    fLeaflet <- file.exists(paste0(sMarxanDir,"/pulayer/leaflet.Rdata"))

                    a_row <- c(sUser,sName,sType,sPlanningUnits,sFeatures,sPolygons,as.character(fLeaflet),sZones,sCosts,sCreated)
                    if (i == 1)
                    {
                        the_table <- a_row
                    } else {
                        the_table <- rbind(the_table,a_row)
                    }
                }
                if (length(list_dirs_mz) > 0)
                for (i in 1:length(list_dirs_mz))
                {
                    # read stats for this database
                    sMarxanDir <- paste0(sMzHome,"/",list_dirs_mz[i],"/") 
                    sName <- list_dirs_mz[i]
                    sType <- "marzone"
                    sCreated <- as.character(file.info(sMarxanDir)$ctime)
                    pudat <- read.csv(paste0(sMarxanDir,"/input/pu.dat"),stringsAsFactors=FALSE)
                    sPlanningUnits <- nrow(pudat)
                    specdat <- read.csv(paste0(sMarxanDir,"/input/spec.dat"),stringsAsFactors=FALSE)
                    sFeatures <- nrow(specdat)

                    putable <- read.dbf(paste0(sMarxanDir,"/pulayer/pulayer.dbf"))
                    sPolygons <- nrow(putable)
                    zonesdat <- read.csv(paste0(sMarxanDir,"/input/zones.dat"),stringsAsFactors=FALSE)
                    sZones <- nrow(zonesdat)
                    costsdat <- read.csv(paste0(sMarxanDir,"/input/costs.dat"),stringsAsFactors=FALSE)
                    sCosts <- nrow(costsdat)

                    fLeaflet <- file.exists(paste0(sMarxanDir,"/pulayer/leaflet.Rdata"))

                    a_row <- c(sUser,sName,sType,sPlanningUnits,sFeatures,sPolygons,as.character(fLeaflet),sZones,sCosts,sCreated)
                    the_table <- rbind(the_table,a_row)
                }
            }
            colnames(the_table) <- col_names
            rownames(the_table) <- rep("",nrow(the_table))

            return(the_table)
        }
    })

    generate_public_datasets <- reactive({
    
        input$publicOk
        input$removeOk

        public_datasets <- c()
        sPublicDir <- paste0(sShinyDataPath,"/public")
        list_users <- list.dirs(sPublicDir,full.names=FALSE)
        for (i in 1:length(list_users))
        {
            sUser <- list_users[i]
            sMxHome <- paste0(sPublicDir,"/",sUser,"/marxan")
            sMzHome <- paste0(sPublicDir,"/",sUser,"/marzone")
            
            list_dirs_mx <- list.dirs(sMxHome,full.names=FALSE)
            list_dirs_mz <- list.dirs(sMzHome,full.names=FALSE)
            
            if (length(list_dirs_mx) > 0)
            {
                for (j in 1:length(list_dirs_mx))
                {
                    
                    public_datasets <- c(public_datasets,paste0(sUser,"/marxan/",list_dirs_mx[j]))
                }
            }
            if (length(list_dirs_mz) > 0)
            {
                for (j in 1:length(list_dirs_mz))
                {
                    
                    public_datasets <- c(public_datasets,paste0(sUser,"/marzone/",list_dirs_mz[j]))
                }
            }
        }
        public_datasets <<- public_datasets
        return(public_datasets)
    })

    generate_1st_public_dataset <- reactive({

        input$publicOk
        input$removeOk

        return(public_datasets[1])
    })

    output$sharedwithmetable <- renderTable({

        if (USER$Logged == TRUE)
        {
            the_table <- as.data.frame(rbind(c("e","f"),c("g","h")))

            return(the_table)
        }
    })

    output$usermessage = renderText({
        if (USER$Logged == TRUE)
        {
            sprintf(paste0("Hello ",sUserName))
        } else {
            sprintf("")
        }
    })

    observe({
        sUserIP <<- as.character(input$ipid)
        UserGeoIP <<- freegeoip(sUserIP)
        Hostname <- ip_to_hostname(sUserIP)
        sUserHostname <<- Hostname[[1]]
    })

    output$userLocation <- renderText({
        paste0("Login from ",sUserHostname)
    })

    observe({

        if (USER$Logged == TRUE)
        {
            # User has logged in. Record details about the HTTP session.
            query <- parseQueryString(session$clientData$url_search)
            sText <- paste0("fingerprint: ", input$fingerprint,"\n",
                            "ip: ", sUserIP,"\n",
                            "userhostname: ",sUserHostname,"\n",
                            "protocol: ", session$clientData$url_protocol, "\n",
                            "hostname: ", session$clientData$url_hostname, "\n",
                            "pathname: ", session$clientData$url_pathname, "\n",
                            "port: ",     session$clientData$url_port,     "\n",
                            "search: ",   session$clientData$url_search,   "\n",
                            "queries: ",paste(names(query), query, sep = "=", collapse=", "),"\n")

            AppendLogFile(sLogFile,sText)
            cat(paste0(sText,"\n"))
        }
    })

    output$lastLogin <- renderText({

        if (USER$Logged == TRUE)
        {
            sLastLogin <- paste0(sUserHome,"/lastLogin.Rdata")
            if (file.exists(sLastLogin))
            {
                load(file=sLastLogin)
                sMessage <- paste0("Last login ",as.character(LastLoginDate)," from ",sUserLastHostname)
            } else {
                sMessage <- "First login"
            }

            LastLoginDate <- date()
            sUserLastIP <- sUserIP
            sUserLastHostname <- sUserHostname
            UserLastGeoIP <- UserGeoIP
            save(LastLoginDate,sUserLastIP,sUserLastHostname,UserLastGeoIP,file=sLastLogin)

            sMessage
        }
    })
})
