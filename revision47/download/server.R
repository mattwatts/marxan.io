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

#options(rgl.useNULL=TRUE)
#library(shinyRGL)
#library(rgl)

registerDoMC(iRepsPerCore)  # the number of CPU cores

Logged = FALSE;

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
            if (!is.null(input$database))
            {
                # select this database from the list of databases
                sSelectDb <<- input$database
                cat(paste0("sSelectDb ",sSelectDb,"\n"))
                sPrevious <- sMarxanDir
                sMarxanDir <<- paste0(sMarxanHome,"/",sSelectDb)
                sZipWD <<- paste0(sMarxanHome)
                if (!file.exists(sMarxanDir))
                {
                    sMarxanDir <<- paste0(sMarZoneHome,"/",sSelectDb)
                    sZipWD <<- paste0(sMarZoneHome)
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
                updateNumericInput(session,"areyousure",value=1)
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
    })
    
    output$downloadtable <- renderTable({
    
        if (USER$Logged == TRUE)
        {
            input$refreshtable
    
            # parse the marxan and marzone databases, listing them in the grid
            col_names <- c("name","type","created","used","last_run","planning_units","features","polygons","zones","costs")
            list_dirs_mx <- list.dirs(sMarxanHome,full.names=FALSE)
            list_dirs_mz <- list.dirs(sMarZoneHome,full.names=FALSE)
            for (i in 1:length(list_dirs_mx))
            {
              # read stats for this database
              sMarxanDir <- paste0(sMarxanHome,"/",list_dirs_mx[i],"/") 
              sName <- list_dirs_mx[i]
              sType <- "marxan"
                  aPuRdataFile <- paste0(sMarxanDir,"/pulayer/pulayer.Rdata")
              sCreated <- as.character(file.info(aPuRdataFile)$ctime)
              sSumFile <- paste0(sMarxanDir,"/output/output_sum.csv")
              fUsed <- file.exists(sSumFile)
              #as.character(fUsed)
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
  
              a_row <- c(sName,sType,sCreated,as.character(fUsed),sLastUsed,sPlanningUnits,sFeatures,sPolygons,sZones,sCosts)
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
  
              a_row <- c(sName,sType,sCreated,as.character(fUsed),sLastUsed,sPlanningUnits,sFeatures,sPolygons,sZones,sCosts)
              the_table <- rbind(the_table,a_row)    
            }
            colnames(the_table) <- col_names             
            rownames(the_table) <- rep("",nrow(the_table))               

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
