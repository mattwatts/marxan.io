# marxan.io upload

library(shiny)
library(sp)
library(maptools)
library(PBSmapping)
library(rgeos)
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

# Set the file size limit for uploads here in megabytes
iMegabytes <- 500
options(shiny.maxRequestSize = iMegabytes*1024^2)

registerDoMC(iRepsPerCore)  # the number of CPU cores

Logged = FALSE;

shinyServer(function(input, output, session, clientData) {

    observe({
        sUserIP <<- as.character(input$ipid)
        cat(paste0("sUserIP ",sUserIP,"\n"))
    })

    source(paste0(sAppDir,"/authenticate.R"),  local = TRUE)

    source(paste0(sShinySourcePath,"/ingest_marxan_data.R"),  local = TRUE)
    source(paste0(sShinySourcePath,"/server_pre_marxan.R"),  local = TRUE)

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
            sDatabase <<- input$uploadname
        }
    }) # observe
  
    output$contents <- renderTable({
        # input$file1 is the zip file containing the Marxan database.
        # summarytable lists info gleaned from parsing the Marxan database.
    
        inFile <- input$file1

        if (is.null(inFile))
          return(NULL)

        # create new user session for this upload
        sUserSession <<- CreateTempDir(paste0(sShinyTempPath,sUserName))

        file.copy(as.character(inFile$datapath),paste0(sUserSession,"/",as.character(inFile$name)),overwrite=TRUE)

        ptm <- proc.time()
        ParseResult <- ParseMarxanZip(paste0(sUserSession,"/",as.character(inFile$name)),sUserSession,sShinyUserPath,sShinyDataPath,sUserName)
        iElapsed <- (proc.time() - ptm)[3]
        summarytable <- rbind(c("name",as.character(inFile$name)),
                              c("size",paste0(as.character(inFile$size)," bytes")),
                              c("elapsed",paste0(as.character(iElapsed)," seconds")))
        if (fMarZone)
        {
            summarytable <- rbind(summarytable,
                                  c("MarZone",""),
                                  c("zones",iCountZone),
                                  c("costs",iCountCost))
        } else {
            summarytable <- rbind(summarytable,
                                  c("Marxan",""))
        }
        summarytable <- rbind(summarytable,
                              c("planning units",iCountPU),
                              c("features",iCountSpec),
                              c("connections",iCountBound),
                              c("matrix",iCountMatrix),
                              c("polygons",iCountPolygon))
        ReadParseErrors(ParseResult)
        if (is.null(Warnings))
        {
            summarytable <- rbind(summarytable,c("Warnings",0))
        } else {
            summarytable <- rbind(summarytable,c("Warnings",length(Warnings)))
            for (i in 1:length(Warnings))
            {
                summarytable <- rbind(summarytable,c("Warning",as.character(Warnings[i])))
            }
        }
        if (is.null(Errors))
        {
            summarytable <- rbind(summarytable,c("Errors",0))
        
            # turn on accept controls
            updateNumericInput(session,"showacceptcontrols",value=1)

        } else {
            summarytable <- rbind(summarytable,c("Errors",length(Errors)))
            for (i in 1:length(Errors))
            {
                summarytable <- rbind(summarytable,c("Error",as.character(Errors[i])))
            }
        }
        updateTextInput(session, "uploadname",value = SafeDbName(basename(file_path_sans_ext(inFile$name)),sShinyUserPath,sUserName))

        as.data.frame(summarytable)
    })

    acceptclicked <- reactive({

        cat("acceptclicked\n")

        if (USER$Logged == TRUE)
        if (!is.null(input$acceptupload))
        if (input$acceptupload > 0)
        {
            AppendLogFile(sLogFile,paste0("input$acceptupload start ",input$acceptupload))

            cat(paste0("click acceptupload ",input$acceptupload,"\n"))

            # add this database to list of databases
            if (fMarZone)
            {
                sDatabasePath <- paste0(sUserHome,"/marzone/",sDatabase)
            } else {
                sDatabasePath <- paste0(sUserHome,"/marxan/",sDatabase)
            }
            cat(paste0("sDatabasePath ",sDatabasePath,"\n"))
            # create directory
            if (!file.exists(sDatabasePath))
            {
                AddDatabase(sDatabasePath)

                # turn off accept controls
                updateNumericInput(session,"showacceptcontrols",value=0)

                withProgress(message="Dataset accepted",value=0,min=0,max=20, { Sys.sleep(5) })

                # trigger a refresh of the relevant UI components
                return(1)
            } else {
                # duplicate database name detected
                withProgress(message="Duplicate dataset name",value=0,min=0,max=20, { Sys.sleep(5) })
                return(2)
            }

            AppendLogFile(sLogFile,paste0("input$acceptupload end ",input$acceptupload))

        } else {
            return(0)
        }
    })

    output$feedbackupload = renderText({
        acceptclicked()
        sprintf("")
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
        if (UserGeoIP == "unknown")
        {
            sText <- paste0("Login from ",sUserHostname)
        } else {
            sText <- paste0("Login from ",sUserHostname," ",UserGeoIP$city)
        }
        sText
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
                            "queries: ",paste(names(query), query, sep = "=", collapse=", "))
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
                if (UserLastGeoIP == "unknown")
                {
                    sMessage <- paste0("Last login ",as.character(LastLoginDate)," from ",sUserLastHostname)
                } else {
                    sMessage <- paste0("Last login ",as.character(LastLoginDate)," from ",sUserLastHostname," ",UserLastGeoIP$city)
                }
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
