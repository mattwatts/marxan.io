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

shinyServer(function(input, output, session, clientData) {

    observe({
        sUserIP <<- as.character(input$ipid)
        cat(paste0("sUserIP ",sUserIP,"\n"))
    })

    observe({
        sFingerprint <<- as.character(input$fingerprint)
        cat(paste0("sFingerprint ",sFingerprint,"\n"))
    })

    #values = list()
    #setHot = function(x) values[["hot"]] <<- x  

    source(paste0(sShinySourcePath,"/server_pre_marxan.R"),  local = TRUE)

    observe({

        # render the user interface
        source(paste0(sAppDir,"/render_ui.R"),  local = TRUE)
    })

    observe({

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
                sZipFile <- paste0(sShinyTempPath,"/",sSelectPublicDatabase,".zip")
                if (file.exists(sZipFile))
                {
                    file.remove(sZipFile)
                }

                # create temp directory
                sTempDir <- paste0(sShinyTempPath)
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

    output$publictable <- renderTable({

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
    })

    generate_public_datasets <- reactive({

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

        return(public_datasets[1])
    })

    observe({
        sUserIP <<- as.character(input$ipid)
        UserGeoIP <<- freegeoip(sUserIP)
        Hostname <- ip_to_hostname(sUserIP)
        sUserHostname <<- Hostname[[1]]
    })
})
