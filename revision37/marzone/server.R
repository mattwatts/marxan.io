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

options(rgl.useNULL=TRUE)
library(shinyRGL)
library(rgl)

registerDoMC(iRepsPerCore)  # the number of CPU cores

Logged = FALSE;

iAspectX <<- 1
iAspectY <<- 1

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

            # we detect if there are new folders in the users directory, indicating a new database import
            CurrentImportTime <- max(file.info(c(list.dirs(sAppHome,full.names = TRUE)))$ctime)
            if (!(CurrentImportTime == ImportTime))
            {
                # user has imported a new dataset
                cat(paste0("new dataset detected","\n"))
                ImportTime <<- CurrentImportTime

                # update the list of datasets to include the new one(s)
                updateSelectInput(session, "database",
                                  choices = c(list.dirs(sAppHome)),
                                  selected = sSelectDb)
            }
        }
    })

    generate_ssoln_html_legend <- reactive({

        # generates map legend for marzone selection frequency as HTML
        input$database

        legend_text <- c()
        for (i in 1:iZones)
        {
            legend_text <- c(legend_text,paste0("<img src='http://marxan.io/images/rainbow_",iZones,"_",i,".png' /></a>"))
            legend_text <- c(legend_text,paste0("&nbsp;",ZoneNames[i]))
            if (i != iZones)
            {
                legend_text <- c(legend_text,"<br>")
            }
        }
        return(paste(legend_text,collapse=''))
    })

    generate_input_files_list <- reactive({
        if (USER$Logged == TRUE)
        {
            input$database

            if (sMarxanDir == "")
            {
                c("spec","zones","costs","zonecost","zoneboundcost","zonecontrib","zonetarget")
            } else {
                c(input_list)
            }
        }
    })

    observe({
        if (USER$Logged == TRUE)
        {
            # render the user interface
            source(paste0(sAppDir,"/render_ui.R"),  local = TRUE)
        } # if
    }) # observe

    observe({
        if (USER$Logged == TRUE)
        {
            input$saveBtn

            if (!is.null(values[["hot"]]))
            {
                # if there are extra rows that have been added by "dragging" the control, remove them
                specdat_edit <- values[["hot"]]
                iEditRows <- nrow(specdat_edit)

                if (iSpecDatRows > 0)
                {
                    if (iEditRows > iSpecDatRows)
                    {
                        # delete the extra rows before saving
                        iRowsToDelete <- iEditRows - iSpecDatRows
                        for (i in 1:iRowsToDelete)
                        {
                            specdat_edit <- specdat_edit[-c(nrow(specdat_edit)),]
                        }
                    }
                }

                write.csv(specdat_edit, paste0(sMarxanDir,"/input/spec.dat"), row.names=F)
            }
        }
    })

    output$hot = renderRHandsontable({
        if (USER$Logged == TRUE)
        {
            if (!is.null(input$database))
            {    
                if (!is.null(input$hot))
                {
                    DF = hot_to_r(input$hot)
                } else {
                    DF = read.csv(paste0(sMarxanDir,"/input/spec.dat"),stringsAsFactors=FALSE)
                    iSpecDatRows <<- nrow(DF)
                    DF$spf <- as.numeric(DF$spf)
                }

                setHot(DF)
                rhandsontable(DF, readOnly = T) %>%
                hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
                hot_col(c("prop","spf"), readOnly = FALSE) %>%
                hot_cols(renderer = "
                    function (instance, td, row, col, prop, value, cellProperties)
                    {
                        Handsontable.renderers.TextRenderer.apply(this, arguments);
                        if (col == 1 && (value > 1.0 || value < 0.0))
                        {
                            td.style.background = 'red';
                        }
                    }"
                )
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
                sMarxanDir <<- paste0(sAppHome,"/",sSelectDb)
                cat(paste0("sMarxanDir ",sMarxanDir,"\n"))
                AppendLogFile(sLogFile,paste0("sSelectDb ",sSelectDb))
                AppendLogFile(sLogFile,paste0("sMarxanDir ",sMarxanDir))

                if (sPrevious != sMarxanDir)
                {
                    if (sSelectDb != "")
                    {
                        ChangeDatabase("marzone")

                        # update the relevant UI components
                        # trigger a refresh of the marxan UI
                        # trigger a refresh of the map
                        irefreshmap <<- irefreshmap + 1
                        updateNumericInput(session, "refreshmap", value = irefreshmap)
                        # trigger a refresh of the table
                        irefreshtable <<- irefreshtable + 1
                        updateNumericInput(session, "refreshtable", value = irefreshtable)
                        # trigger a refresh of the cluster
                        irefreshcluster <<- irefreshcluster + 1
                        updateNumericInput(session, "refreshcluster", value = irefreshcluster)
                    }
                }
            }
        }
    }) # observe

    observe({

        cat("input$m\n")

        if (USER$Logged == TRUE)
        if (!is.null(input$m))
        if (input$m > 0)
        {
            iM <<- input$m
            cat(paste0("iM ",iM,"\n"))
            AppendLogFile(sLogFile,paste0("input$m ",input$m))
        }
    })

    observe({

        cat("input$n\n")

        if (USER$Logged == TRUE)
        if (!is.null(input$n))
        if (input$n > 0)
        {
            iN <<- input$n
            cat(paste0("iN ",iN,"\n"))
            AppendLogFile(sLogFile,paste0("input$n ",input$n))
        }
    })

    runclicked <- reactive({

        cat(paste0("mrun ",fMarxanRunning,"\n"))

        if (USER$Logged == TRUE)
        if (!is.null(input$mrun))
        if (input$mrun > 0)
        if (fMarxanRunning == FALSE)
        {
            fMarxanRunning <<- TRUE
            ptm <- proc.time()

            cat(paste0("click mrun ",input$mrun,"\n"))

            RunMarZone_app()

            # trigger a refresh of the UI
            # trigger a refresh of the map
            irefreshmap <<- irefreshmap + 1
            updateNumericInput(session, "refreshmap", value = irefreshmap)
            # trigger a refresh of the table
            irefreshtable <<- irefreshtable + 1
            updateNumericInput(session, "refreshtable", value = irefreshtable)
            # trigger a refresh of the cluster
            irefreshcluster <<- irefreshcluster + 1
            updateNumericInput(session, "refreshcluster", value = irefreshcluster)

            AppendLogFile(sLogFile,paste0("input$mrun ",input$mrun," elapsed ",(proc.time() - ptm)[3]))
            fMarxanRunning <<- FALSE
        }
        return(0)
    })

    output$marzoneplot <- renderPlot({

        input$refreshcluster

        if (!is.null(input$refreshcluster))
        {
            AppendLogFile(sLogFile,paste0("output$marzoneplot ",input$refreshcluster," ",input$cluster))

            withProgress(message="Rendering cluster",value=0,
           {
                if (input$cluster == "cluster2ds") { cluster_2ds("marzone",FALSE) }
                #if (input$cluster == "cluster3ds") { cluster_3ds("marzone") }
                if (input$cluster == "clusterdendogram") { cluster_dendogram("marzone") }
            })
        }
    }, height=600,width=600) # renderPlot

    output$marzonemap <- renderPlot({

        input$refreshmap
        input$m
        input$n

        if (!is.null(input$refreshmap))
        {
            AppendLogFile(sLogFile,paste0("output$marzonemap ",input$refreshmap," ",input$map))
            
            cat(paste0("x ",iAspectX," y ",iAspectY," height ",600," width ",round(600/iAspectY*iAspectX),"\n"))

            withProgress(message="Rendering map",value=0,
            {
                if (input$map == "ssolnNmap") { map_mz_ssolnNmap(input$n) }
                if (input$map == "bestmap") { map_mz_bestmap() }
                if (input$map == "runMmap") { map_mz_runMmap(input$m) }

                if (!is.na(puoutline))
                {
                    addLines(puoutline,col="black")
                }
            })
        }
         
    }, height=600,width=round(600/iAspectY*iAspectX)) # renderPlot

    output$marzone3d <- renderWebGL({

        #input$refreshcluster
        #cluster_3ds()
    }, height=600,width=600)
    
    output$marzoneinputtable <- renderTable({
    
        input$refreshtable

        if (input$table_i == "zones")
        {
            sTable <- "zones.dat"
            thetable <- read.csv(paste0(sMarxanDir,"/input/",sTable))
        }
        if (input$table_i == "costs")
        {
            sTable <- "costs.dat"
            thetable <- read.csv(paste0(sMarxanDir,"/input/",sTable))
        }
        if (input$table_i == "zonecost")
        {
            sTable <- "zonecost.dat"
            thetable <- read.csv(paste0(sMarxanDir,"/input/",sTable))
        }
        if (input$table_i == "zoneboundcost")
        {
            sTable <- "zoneboundcost.dat"
            sFile <- paste0(sMarxanDir,"/input/",sTable)
            if (file.exists(sFile))
            {
                zoneboundcost <- read.csv(sFile)
                zoneboundcost <- sqldf("SELECT * from zoneboundcost where cost > 0")
                # make blank 2d ZBC table
                ZBC <- rep(0,iZones)
                for (i in 2:iZones)
                {
                  ZBC <- rbind(ZBC,rep(0,iZones))
                }
                colnames(ZBC) <- ZoneNames
                rownames(ZBC) <- ZoneNames
                # populate ZBC table from zoneboundcost file
                for (i in (1:nrow(zoneboundcost)))
                {
                  ZBC[zoneboundcost$zoneid1[i],zoneboundcost$zoneid2[i]] <- as.character(zoneboundcost$cost[i])
                  ZBC[zoneboundcost$zoneid2[i],zoneboundcost$zoneid1[i]] <- as.character(zoneboundcost$cost[i])
                }
                thetable <- ZBC
            } else {
                thetable <- as.data.frame(cbind(c("input","1","2"),c(sTable,"a","b")))
            }
        }
        if (input$table_i == "zonecontrib")
        {
            sTable <- "zonecontrib.dat"
            sFile <- paste0(sMarxanDir,"/input/",sTable)
            if (file.exists(sFile))
            {
                thetable <- read.csv(sFile)
            } else {
                sFile <- paste0(sMarxanDir,"/input/zonecontrib2.dat")
                if (file.exists(sFile))
                {
                    thetable <- read.csv(sFile)
                } else {
                    thetable <- as.data.frame(cbind(c("input","1","2"),c(sTable,"a","b")))
                }
            }
        }
        if (input$table_i == "zonetarget")
        {
            sTable <- "zonetarget.dat"
            sFile <- paste0(sMarxanDir,"/input/",sTable)
            if (file.exists(sFile))
            {
                thetable <- read.csv(sFile)
            } else {
                sFile <- paste0(sMarxanDir,"/input/zonetarget2.dat")
                if (file.exists(sFile))
                {
                    thetable <- read.csv(sFile)
                } else {
                    thetable <- as.data.frame(cbind(c("input","1","2"),c(sTable,"a","b")))
                }
            }
        }

        return(thetable)
    })

    output$marzoneoutputtable <- renderTable({
        if (input$table_o == "sumtable")
        {
            sTable <- "sumtable.dat"
            thetable <- read.csv(paste0(sMarxanDir,"/output/output_sum.csv"))
            # NOTE : sql select will fail if zone names are sql keywords
            thetable <- round(sqldf(paste0("SELECT Score, Cost, Shortfall from thetable")))
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
            #for (j in 1:ncol(thetable))
            #{
            #  thetable[iBest,j] <- HTML(paste0("<FONT COLOR='blue'>",thetable[iBest,j],"</FONT>"))
            #}
        }
        if ((input$table_o == "mvbesttable") | (input$table_o == "mvNtable"))
        {
            if (input$table_o == "mvbesttable")
            {
            sTable <- "mvbesttable.dat"
                thetable <- read.csv(paste0(sMarxanDir,"/output/output_sum.csv"))
                #thetable <- round(sqldf("SELECT Score, Cost from thetable"))
                iTable <- which.min(thetable$Score)
            }
            if (input$table_o == "mvNtable")
            {
                iTable <- input$m
                sTable <- paste0("mv_",input$m,"_table.dat")
            }
            
            
            sFile <- paste0(sMarxanDir,"/output/output_mv",PadInt(iTable),".csv")
            thetable <- read.csv(sFile,stringsAsFactors=FALSE)
            
            # sort the table the way spec.dat is ordered
            tableorder <- seq.int(from=nrow(thetable),to=1)
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
            #for (i in 1:nrow(thetable))
            #{
            #    if (thetable[i,5] == "no")
            #    {
            #        for (j in 1:7)
            #        {
            #            thetable[i,j] <- HTML(paste0("<FONT COLOR='blue'>",thetable[i,j],"</FONT>"))
            #        }
            #    } else {
            #        for (j in 1:7)
            #        {
            #            thetable[i,j] <- HTML(paste0("<FONT COLOR='black'>",thetable[i,j],"</FONT>"))
            #        }
            #    }
            #}
        }

        return(thetable)
    })

    output$usermessage = renderText({
        if (USER$Logged == TRUE)
        {
            sprintf(paste0("Hello ",sUserName))
        } else {
            sprintf("")
        }
    })

    output$textfeedback = renderText({
        runclicked()
        sprintf("Finished")
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
    
    zonelabel <- reactive({
    
        input$database
    
        if (!is.null(input$n))
        {
            sZone <- ZoneNames[input$n]
            if (sZone == NA)
            {
                sZone <- "fred"
            }
            return(paste0("Selection frequency ",sZone," zone"))
        } else {
            return("")
        }
    })
    
    output$zonename = renderText({
    
        input$database
        
        #sprintf(zonelabel())
        paste0("Selection frequency ",ZoneNames[input$n]," zone")
    })
})
