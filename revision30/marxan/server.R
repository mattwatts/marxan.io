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

  source(paste0(sShinySourcePath,"/prepare_param_test.R"),  local = TRUE)
  source(paste0(sShinySourcePath,"/ingest_marxan_data.R"),  local = TRUE)
  source(paste0(sShinySourcePath,"/server_pre_marxan.R"),  local = TRUE)

  autoInvalidate <- reactiveTimer(2000,session=session)

  observe({
      if (USER$Logged == TRUE)
      {

          autoInvalidate()
          
          # we detect if there are new folders in the users directory, indicating a new database import
          CurrentImportTime <- max(file.info(c(list.dirs(sMarxanHome,full.names = TRUE)))$ctime)
          if (!(CurrentImportTime == ImportTime))
          {
              # user has imported a new dataset
              cat(paste0("new dataset detected","\n"))
              ImportTime <<- CurrentImportTime
              
              # update the list of datasets to include the new one(s)
              updateSelectInput(session, "database",
                                choices = c(list.dirs(sMarxanHome)),
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
  }) # observe
  
  observe({
    if (USER$Logged == TRUE)
    {    
      input$saveBtn
      
      if (!is.null(values[["hot"]])) {
      
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
    if (!is.null(input$database))
    {    
      if (!is.null(input$hot)) {
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
      #        hot_validate_numeric(col = 2, min = 0, max = 1.0) #%>%
      hot_cols(renderer = "
         function (instance, td, row, col, prop, value, cellProperties) {
           Handsontable.renderers.TextRenderer.apply(this, arguments);
           if (col == 1 && (value > 1.0 || value < 0.0)) {
            td.style.background = 'red';
           }
         }")
    }
  })

  observe({
    if (USER$Logged == TRUE)
    if (!is.null(input$database))
    {
        # select this database from the list of databases
        sSelectDb <<- input$database
        cat(paste0("sSelectDb ",sSelectDb,"\n"))
        sPrevious <- sMarxanDir
        sMarxanDir <<- paste0(sMarxanHome,"/",sSelectDb)
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
                irefreshinput <<- irefreshinput + 1
                
                if (irefreshinput > 1)
                {
                    updateNumericInput(session, "refreshinput", value = irefreshinput)
                }
                
                cat(paste0("database change irefreshinput ",irefreshinput,"\n"))
            }
        }
    }
  }) # observe

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
      if (!is.null(input$blm))
      {
          rblm <<- input$blm
          cat(paste0("rblm ",rblm,"\n"))
          AppendLogFile(sLogFile,paste0("input$blm ",input$blm))
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

          RunMarxan_app()

          # trigger a refresh of the UI
          irefreshinput <<- irefreshinput + 1
          updateNumericInput(session, "refreshinput", value = irefreshinput)
          
          cat(paste0("mrun change irefreshinput ",irefreshinput,"\n"))

          AppendLogFile(sLogFile,paste0("input$mrun ",input$mrun," elapsed ",(proc.time() - ptm)[3]))
          fMarxanRunning <<- FALSE
      }
      return(0)
  })

  observe ({
    if (USER$Logged == TRUE)
    if (!is.null(input$displaywhat))
    {
      if (input$displaywhat == "map")
      {
          #iAspectX <<- x_
          #iAspectY <<- y_
      }
      if (input$displaywhat == "cluster")
      {
          #iAspectX <<- 1
          #iAspectY <<- 1
      }
    }
  })

  output$marxanplot <- renderPlot({

      input$refreshinput
      input$m

      if (!is.null(input$displaywhat))
      {
          AppendLogFile(sLogFile,paste0("output$marxancluster ",input$displaywhat," ",input$cluster))

          if (input$cluster == "cluster2ds") { cluster_2ds() }
          if (input$cluster == "cluster3ds") { cluster_3ds() }
          if (input$cluster == "cluster3ds") { plot(1,1) }
          if (input$cluster == "clusterdendogram") { cluster_dendogram() }
      }
  }, height=600,width=600) # renderPlot
  
  output$marxanmap <- renderPlot({

      input$refreshinput
      input$m

      if (!is.null(input$displaywhat))
      {
          AppendLogFile(sLogFile,paste0("output$marxanplot ",input$displaywhat," ",input$map," ",input$m))

          if (input$map == "ssolnNmap") { map_ssolnNmap() }
          if (input$map == "bestmap") { map_bestmap() }
          if (input$map == "runMmap") { map_runMmap() }

          if (!is.na(puoutline))
          {
              addLines(puoutline,col="black")
          }
       }
  }, height=600,width=round(600/iAspectY*iAspectX)) # renderPlot
  
  output$marxan3d <- renderWebGL({
      input$refreshinput

      cluster_3ds()
  }, height=600,width=600)

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
