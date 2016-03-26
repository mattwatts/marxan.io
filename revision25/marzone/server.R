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

# Set the file size limit for uploads here in megabytes
iMegabytes <- 500
options(shiny.maxRequestSize = iMegabytes*1024^2)

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
          
          # we detect if the user has logged out
          #if (!AuthenticateUserSession(sUrlSessionKey))
          #{
          #    USER$Logged <<- FALSE
          #}
      
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
        #print(fname)
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
    {
        sDatabase <<- input$uploadname
    }
  }) # observe
  
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
        AppendLogFile(paste0("sSelectDb ",sSelectDb))
        AppendLogFile(paste0("sMarxanDir ",sMarxanDir))
        # update input file name for ptplot
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
        
        if (sPrevious != sMarxanDir)
        {
            if (sSelectDb != "")
            {
                ChangeDatabase()

                # update the relevant UI components
                # update BLM
                updateNumericInput(session,"blm",value=as.numeric(sBLM))
                # trigger a refresh of the marxan UI
                irefreshinput <<- irefreshinput + 1
                updateNumericInput(session, "refreshinput", value = irefreshinput)
                # trigger a refresh of the param testing UI
                irefreshptinput <<- irefreshptinput + 1
                updateNumericInput(session, "refreshptinput", value = irefreshptinput)
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
          AppendLogFile(paste0("input$m ",input$m))
      }
  })

  observe({

      cat("blm\n")

      if (USER$Logged == TRUE)
      if (!is.null(input$blm))
      {
          rblm <<- input$blm
          cat(paste0("rblm ",rblm,"\n"))
          AppendLogFile(paste0("input$blm ",input$blm))
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

          AppendLogFile(paste0("input$mrun ",input$mrun," elapsed ",(proc.time() - ptm)[3]))
          fMarxanRunning <<- FALSE
      }
      return(0)
  })

  runparamtest <- reactive({
      cat("runparamtest\n")
      if (input$mptrun == 0)
      {
          imptrun <<- 0
          cat("init mptrun\n")
      }
      else
      {
          if (input$mptrun > imptrun)
          {
              imptrun <<- input$mptrun
              cat("mptrun incremented\n")
              
              RunMarxanParamTest_app()
              
              irefreshptinput <<- irefreshptinput + 1
              updateNumericInput(session, "refreshptinput", value = irefreshptinput)
          }
      }
    
      return(as.character(input$mptrun))
  })

  acceptclicked <- reactive({

      cat("acceptclicked\n")

      if (USER$Logged == TRUE)
      if (!is.null(input$acceptupload))
      if (input$acceptupload > 0)
      {
          AppendLogFile(paste0("input$acceptupload start ",input$acceptupload))

          cat(paste0("click acceptupload ",input$acceptupload,"\n"))

          # validate accept

          # add this database to list of databases
          sDatabasePath <- paste0(sMarxanHome,"/",sDatabase)
          cat(paste0("sDatabasePath ",sDatabasePath,"\n"))
          # create directory
          if (!file.exists(sDatabasePath))
          {
              AddDatabase(sDatabasePath)

              # trigger a refresh of the relevant UI components
              updateSelectInput(session, "database",choices = c(list.dirs(sMarxanHome)))
              return(1)
          } else {
              # duplicate database name detected
              return(2)
          }

          AppendLogFile(paste0("input$acceptupload end ",input$acceptupload))

      } else {
          return(0)
      }
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

  observe ({
    if (USER$Logged == TRUE)
    if (!is.null(input$whichparam))
    {
        cat("observe whichparam\n")

        if (input$whichparam == "BLM Calibration")
        {
            swhichparam <<- "BLM"
        }
        if (input$whichparam == "SPF Calibration")
        {
            swhichparam <<- "SPF"
        }
        if (input$whichparam == "Target Sensitivity")
        {
            swhichparam <<- "Targ"
        }
        
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
                
        sSummary <<- paste0(sMarxanDir,"/output/output_",swhichparam,"summary.csv")

        irefreshptinput <<- irefreshptinput + 1
        updateNumericInput(session, "refreshptinput", value = irefreshptinput)
    }
  })

  observe ({
    if (USER$Logged == TRUE)
    {
        ruserblm <<- as.numeric(input$userblm)
        cat(paste0("ruserblm ",ruserblm,"\n"))
        if (swhichparam == "SPF")
        {
            sAppendSummary <<- paste0(sMarxanDir,"/output/output_SPFsummary_BLM",ruserblm,"_Targ",rusertarg,".csv")
        }
        if (swhichparam == "Targ")
        {
            sAppendSummary <<- paste0(sMarxanDir,"/output/output_Targsummary_BLM",ruserblm,"_SPF",ruserspf,".csv")
        }
    }
  })
    
  observe ({
    if (USER$Logged == TRUE)
    {
        ruserspf <<- as.numeric(input$userspf)
        cat(paste0("ruserspf ",ruserspf,"\n"))
        if (swhichparam == "BLM")
        {
            sAppendSummary <<- paste0(sMarxanDir,"/output/output_BLMsummary_SPF",ruserspf,"_Targ",rusertarg,".csv")
        }
        if (swhichparam == "Targ")
        {
            sAppendSummary <<- paste0(sMarxanDir,"/output/output_Targsummary_BLM",ruserblm,"_SPF",ruserspf,".csv")
        }
    }
  })
    
  observe ({
    if (USER$Logged == TRUE)
    {
        rusertarg <<- as.numeric(input$usertarg)
        cat(paste0("rusertarg ",rusertarg,"\n"))
        if (swhichparam == "BLM")
        {
            sAppendSummary <<- paste0(sMarxanDir,"/output/output_BLMsummary_SPF",ruserspf,"_Targ",rusertarg,".csv")
        }
        if (swhichparam == "SPF")
        {
            sAppendSummary <<- paste0(sMarxanDir,"/output/output_SPFsummary_BLM",ruserblm,"_Targ",rusertarg,".csv")
        }
    }
  })
    
  observe ({
    if (USER$Logged == TRUE)
    {
        cat("observe whichmap\n")

        iwhichmap <<- input$whichmap
        
        irefreshptinput <<- irefreshptinput + 1
        updateNumericInput(session, "refreshptinput", value = irefreshptinput)
    }
  })

  observe ({
    if (USER$Logged == TRUE)
    if (!is.null(input$whichrun))
    {
        cat("observe whichrun\n")

        if (input$whichrun == "Best Solution")
        {
            swhichrun <<- "best"
        } else {
            if (input$whichrun == "Selection Frequency")
            {
                swhichrun <<- "ssoln"
            } else {
               swhichrun <<- substrRight(input$whichrun, nchar(input$whichrun)-4)
            }
        }    
    
        # "Best Solution","Run 1","Run 2","Run 3","Run 4","Run 5",
        # "Run 6","Run 7","Run 8","Run 9","Run 10","Selection Frequency"

        irefreshptinput <<- irefreshptinput + 1
        ################## This is probably the line that is sending the double refresh message on startup. 
        updateNumericInput(session, "refreshptinput", value = irefreshptinput)
    }
  })
    
  observe ({
    if (USER$Logged == TRUE)
    {
        rRampBLMmin <<- input$rampBLMmin
        cat(paste0("rRampBLMmin ",rRampBLMmin,"\n"))
    }
  })

  observe ({
    if (USER$Logged == TRUE)
    {
        rRampBLMmax <<- input$rampBLMmax
        cat(paste0("rRampBLMmax ",rRampBLMmax,"\n"))
    }
  })
    
  observe ({
    if (USER$Logged == TRUE)
    {
        rRampSPFmin <<- input$rampSPFmin
        cat(paste0("rRampSPFmin ",rRampSPFmin,"\n"))
    }
  })

  observe ({
    if (USER$Logged == TRUE)
    {
        rRampSPFmax <<- input$rampSPFmax
        cat(paste0("rRampSPFmax ",rRampSPFmax,"\n"))
    }
  })
    
  observe ({
    if (USER$Logged == TRUE)
    {
        rtargetmin <<- input$targetmin
        cat(paste0("rtargetmin ",rtargetmin,"\n"))
    }
  })

  observe ({
    if (USER$Logged == TRUE)
    {
        rtargetmax <<- input$targetmax
        cat(paste0("rtargetmax ",rtargetmax,"\n"))
    }
  })

  outputptmap <- reactive({
        cat("outputmap\n")
        
        input$refreshptinput
        
        map_pt()
        
        #addLines(puoutline,col="black")
  })

  outputptplot <- reactive({
      cat("outputptplot\n")

      input$refreshptinput
      input$whichmap
        
      VALUEsummary <- read.csv(sAppendSummary)
      VALUElabel <- sqldf(paste0("SELECT ",swhichparam," from VALUEsummary"))
      colnames(VALUElabel)[1] <- "label"
      VALUElabel <- unlist(VALUElabel$label)
        
      # make all plotlabels blank except for the last iNumberOfTests
      if (length(VALUElabel) > iCores)
      {
          VALUElabel[1:(length(VALUElabel)-iCores)] <- ""
      }
        
      if (swhichparam == "BLM")
      {
          colnames(VALUEsummary)[4] <- "boundary"
          VALUEsummary <- sqldf("SELECT cost, boundary from VALUEsummary")
      }
      if (swhichparam == "SPF")
      {
          colnames(VALUEsummary)[4] <- "shortfall"
          VALUEsummary <- sqldf("SELECT cost, shortfall from VALUEsummary")
      }
      if (swhichparam == "Targ")
      {
          colnames(VALUEsummary)[2] <- "target"
          VALUEsummary <- sqldf("SELECT cost, target from VALUEsummary")
      }
      colours <- rep("black",each=nrow(VALUEsummary))
      colours[length(VALUElabel)-iCores+as.numeric(iwhichmap)] <- "blue"
      plot(VALUEsummary,col=colours)
      if (input$plotvalues)
      {
          text(VALUEsummary,labels=VALUElabel,pos=4,col=colours)
      }
  })

  outputpttable <- reactive({
      cat("outputpttable\n")

      input$refreshptinput
        
      thetable <- read.csv(paste0(sMarxanDir,"/output/output_",swhichparam,"summary.csv"),stringsAsFactors=FALSE)
      if (swhichparam == "BLM")
      {       
          colnames(thetable)[4] <- "boundary"
          thetable <- sqldf("SELECT BLM, cost, boundary from thetable")
          thetable$BLM <- as.character(thetable$BLM)
          iColumns <- 3
      }
      if (swhichparam == "SPF")
      {
          colnames(thetable)[4] <- "shortfall"
          thetable <- sqldf("SELECT SPF, cost, shortfall from thetable")
          thetable$SPF <- as.character(thetable$SPF)
          iColumns <- 3
      }
      if (swhichparam == "Targ")
      {
          colnames(thetable)[2] <- "target"
          thetable <- sqldf("SELECT target, cost from thetable")
          iColumns <- 2
      }
      for (i in (1:nrow(thetable)))
      {
          if (i == iwhichmap)
          {
              for (j in (1:iColumns))
              {
                  thetable[i,j] <- HTML(paste0("<FONT COLOR='blue'>",thetable[i,j],"</FONT>"))
              }
          }
      }
      return(thetable)
  })

  output$ptmap <- renderPlot({
      print(outputptmap())
      
      if (!is.na(puoutline))
      {
          addLines(puoutline,col="black")
      }
      
  }, height=600,width=round(600/iAspectY*iAspectX))

  output$ptplot <- renderPlot({
      print(outputptplot())
  })

  output$pttable <- renderTable({
      data.frame(outputpttable())
  }, sanitize.text.function = function(x) x)

  output$marxanplot <- renderPlot({

      input$refreshinput
      input$m

      if (!is.null(input$displaywhat))
      {
          AppendLogFile(paste0("output$marxancluster ",input$displaywhat," ",input$cluster))

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
          AppendLogFile(paste0("output$marxanplot ",input$displaywhat," ",input$map," ",input$m))

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

  output$feedbackupload = renderText({
      if (acceptclicked() == 1)
      {
         sprintf("Accepted")
      } else {
          if (acceptclicked() == 2)
          {
             sprintf("Duplicate Database Name")
          } else {
              sprintf("")
          }
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

  output$textfeedback = renderText({
      runclicked()
      sprintf("Finished")
  })
  
  output$paramtestfeedback = renderText({
      runparamtest()
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
                          
          AppendLogFile(sText)
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
