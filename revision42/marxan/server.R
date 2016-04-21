# marxan.io

library(shiny)
library(sp)
library(maptools)
library(PBSmapping)
library(foreign)
library(sqldf)
library(vegan)
library(labdsv)
library(xtable)
library(foreach)
library(doMC)
library(rhandsontable)
library(iptools)
library(png)
library(rjson)

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

                write.csv(specdat_edit, paste0(sMarxanDir,"/input/spec.dat"),row.names=F,quote=FALSE)
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
                        ChangeDatabase("marxan")

                        # update the relevant UI components
                        # update BLM
                        updateNumericInput(session,"blm",value=as.numeric(sBLM))
                        # trigger a refresh of the marxan UI
                        # trigger a refresh of the map
                        irefreshmap <<- irefreshmap + 1
                        updateNumericInput(session, "refreshmap", value = irefreshmap)
                        # trigger a refresh of the cluster
                        irefreshcluster <<- irefreshcluster + 1
                        updateNumericInput(session, "refreshcluster", value = irefreshcluster)
                        # trigger a refresh of the table
                        irefreshtable <<- irefreshtable + 1
                        updateNumericInput(session, "refreshtable", value = irefreshtable)
                    }
                }
            }
        }
    })

    observe({

        cat("m\n")

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

        cat("blm\n")

        if (USER$Logged == TRUE)
        {
            if (!is.null(input$blm))
            {
                rblm <<- input$blm
                cat(paste0("rblm ",rblm,"\n"))
                AppendLogFile(sLogFile,paste0("input$blm ",input$blm))
            }
        }
    })

    runclicked <- reactive({

        cat(paste0("mrun ",fMarxanRunning,"\n"))

        if (USER$Logged == TRUE)
        {
            if (!is.null(input$mrun))
            {
                if (input$mrun > 0)
                {
                    fMarxanRunning <<- TRUE
                    ptm <- proc.time()

                    cat(paste0("click mrun ",input$mrun,"\n"))

                    RunMarxan_app()

                    # trigger a refresh of the UI
                    # trigger a refresh of the map
                    irefreshmap <<- irefreshmap + 1
                    updateNumericInput(session, "refreshmap", value = irefreshmap)
                    # trigger a refresh of the cluster
                    irefreshcluster <<- irefreshcluster + 1
                    updateNumericInput(session, "refreshcluster", value = irefreshcluster)
                    # trigger a refresh of the table
                    irefreshtable <<- irefreshtable + 1
                    updateNumericInput(session, "refreshtable", value = irefreshtable)

                    AppendLogFile(sLogFile,paste0("input$mrun ",input$mrun," elapsed ",(proc.time() - ptm)[3]))
                    fMarxanRunning <<- FALSE
                }
            }
        }
        return(0)
    })

    output$marxanplot <- renderPlot({

        input$refreshcluster

        if (!is.null(input$refreshcluster))
        {
            AppendLogFile(sLogFile,paste0("output$marxancluster ",sdisplaywhat," ",input$cluster))

            withProgress(message="Rendering cluster",value=0,
           {
                if (input$cluster == "cluster2ds") { cluster_2ds("marxan",FALSE) }
                #if (input$cluster == "cluster3ds") { cluster_3ds("marxan") }
                if (input$cluster == "clusterdendogram") { cluster_dendogram("marxan") }
           })
        }
    }, height=600,width=600) # renderPlot
  
    output$marxanmap <- renderPlot({

        input$refreshmap
        input$m

        if (!is.null(input$refreshmap))
        {
            AppendLogFile(sLogFile,paste0("output$marxanplot ",sdisplaywhat," ",input$map," ",input$m))
            
            cat(paste0("x ",iAspectX," y ",iAspectY," height ",600," width ",round(600/iAspectY*iAspectX),"\n"))

            withProgress(message="Rendering map",value=0,
            {
                if (input$map == "ssolnNmap") { map_ssolnNmap() }
                if (input$map == "bestmap") { map_bestmap() }
                if (input$map == "runMmap") { map_runMmap() }

                if (!is.na(puoutline))
                {
                    addLines(puoutline,col="black")
                }
            })
        }
    }, height=600,width=round(600/iAspectY*iAspectX)) # renderPlot
    
    output$marxantable <- renderTable({
    
        input$refreshtable
        
        if (input$table == "sumtable")
        {
            sFilename <- paste(sMarxanDir,"/output/output_sum.csv",sep="")
            thetable <- read.csv(sFilename)
            thetable <- round(sqldf("SELECT Score, Cost, Planning_Units, Penalty, Shortfall from thetable"))
            iBest <- which.min(thetable[,1])
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
                        
            thetable <- cbind(Run,thetable)#,
            thetable$Run <- as.character(thetable$Run)
            thetable$Run <- as.character(thetable$Run)
            #for (j in 1:6)
            #{
            #    thetable[iBest,j] <- HTML(paste0("<FONT COLOR='blue'>",thetable[iBest,j],"</FONT>"))
            #}
        }
        if ((input$table == "mvbesttable") | (input$table == "mvNtable"))
        {
            if (input$table == "mvbesttable")
            {
                sFilename <- paste(sMarxanDir,"/output/output_sum.csv",sep="")
                thetable <- read.csv(sFilename)
                iTable <- which.min(thetable$Score)[1]
            }
            if (input$table == "mvNtable")
            {
                iTable <- input$m
            }

            sFilename <- paste0(sMarxanDir,"/output/output_mv",PadInt(iTable),".csv")
            thetable <- read.csv(sFilename,stringsAsFactors=FALSE)
            # sort the table the way spec.dat is ordered
            tableorder <- seq.int(from=nrow(thetable),to=1)
            thetable <- thetable[tableorder,]
            # select just fields we want
            colnames(thetable)[4] <- "AmountHeld"
            colnames(thetable)[9] <- "TargetMet"
            thetable <- sqldf(paste0("SELECT Target, AmountHeld, TargetMet from thetable"))
            # load feature name from spec.dat
            specdat <- read.csv(paste0(sMarxanDir,"/input/spec.dat"),stringsAsFactors=FALSE)
            if ('name' %in% specdat)
            {
                name <- sqldf(paste0("SELECT name from specdat"))
            } else {
                name <- sqldf(paste0("SELECT id from specdat"))
                colnames(name)[1] <- "name"
            }
            # join the name
            thetable <- cbind(name,thetable)
            # select the fields we want from total areas file
            tafile <- read.csv(paste0(sMarxanDir,"/core1/MarOptTotalAreas.csv"))
            tafile <- tafile[tableorder,]
            tafile <- sqldf(paste0("SELECT totalarea,reservedarea from tafile"))
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
            # join and tidy the table
            thetable <- cbind(thetable,tafile,targetgap)
            thetable <- sqldf(paste0("SELECT name, totalarea, reservedarea, Target, AmountHeld, TargetMet, targetgap from thetable"))
            colnames(thetable)[2] <- "Total"
            colnames(thetable)[3] <- "Reserved"
            colnames(thetable)[7] <- "TargetGap"
            # colour code features that have not met targets
            #for (i in 1:nrow(thetable))
            #{
            #    if (thetable[i,6] == "no")
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

    #output$marxan3d <- renderWebGL({
        #input$refres hinput

        #cluster_3ds()
    #}, height=600,width=600)

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
  
})
