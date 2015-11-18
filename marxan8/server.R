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

# Set the file size limit for uploads here in megabytes
iMegabytes <- 500
options(shiny.maxRequestSize = iMegabytes*1024^2)

iCores <- 10
iRepsPerCore <- 10

registerDoMC(10)  # the number of CPU cores

Logged = FALSE;
load(file=paste0(sShinyPath,"/passwd.Rdata"))
PASSWORD <- passwd

#source(paste0(sAppDir,"/ingest_marxan_data.R"),  local = TRUE)
#source(paste0(sAppDir,"/server_pre_marxan.R"),  local = TRUE)

x__ <<- 1
y__ <<- 1
iAspectX <<- x__
iAspectY <<- y__

shinyServer(function(input, output, session) {

  # user encryption of data files

  system(paste0("touch ",sAppDir,"/restart.txt"))
  source(paste0(sAppDir,"/www/Login.R"),  local = TRUE)
  values = list()
  setHot = function(x) values[["hot"]] <<- x  

  source(paste0(sAppDir,"/ingest_marxan_data.R"),  local = TRUE)
  source(paste0(sAppDir,"/server_pre_marxan.R"),  local = TRUE)

  observe({
    if (USER$Logged == TRUE)
    {
        # user authenticated
        output$sidebarui <- renderUI({
          sidebarPanel(
              textOutput("usermessage"),
              selectInput("userinterface", "Choose User Interface:",
                          choices = c("Upload","Edit Species","Run Marxan","Parameter Testing","Download")),
              conditionalPanel(condition = "input.userinterface == 'Upload'",
                  fileInput('file1', 'Choose file to upload',accept = c('.zip')),
                  textInput("uploadname","Database Name:",value=""),
                  actionButton("acceptupload","Accept Database"),
                  textOutput("feedbackupload")
              ),
              conditionalPanel(condition = "(input.userinterface == 'Edit Species') || (input.userinterface == 'Run Marxan') || (input.userinterface == 'Parameter Testing')",
                  selectInput("database","Database:",
                              choices = c(list.dirs(sUserHome)),
                              selected = sSelectDb),
                  textOutput("textfeedback")
              ),
              conditionalPanel(condition = "input.userinterface == 'Download'",
                  HTML("Download")
              ),
              conditionalPanel(condition = "input.userinterface == 'Edit Species'",
                               br(),
                               actionButton("saveBtn", "Save")
              ),
              conditionalPanel(condition = "input.userinterface == 'Run Marxan'",
                  br(),
                  actionButton("mrun","Run"),
                  br(),
                  numericInput("blm", "Boundary Length Modifier:",sBLM,min=0),
                  br(),
                  radioButtons("displaywhat", "Display output:",
                               list("Map" = "map",
                                    "Cluster" = "cluster")),
                  conditionalPanel(condition = "input.displaywhat == 'map'",
                      radioButtons("map", "Map to display:",
                                   list("Best solution" = "bestmap",
                                        "Solution M" = "runMmap",
                                        "Selection frequency" = "ssolnNmap"))
                  ),
                  conditionalPanel(condition = "input.displaywhat == 'cluster'",
                      radioButtons("cluster", "Cluster to display:",
                                   list("2d" = "cluster2ds",
                                        "3d" = "cluster3ds",
                                        "Dendogram" = "clusterdendogram"))
                  ),
                  conditionalPanel(condition = "input.map == 'runMmap' & input.displaywhat == 'map'",
                      br(),
                      sliderInput("m", "Solution M:",
                                  value = 1,
                                  min = 1,
                                  max = 100, step = 1)
                  )
              ),
              conditionalPanel(condition = "input.userinterface == 'Parameter Testing'",
                  HTML("Parameter Testing")
              )
          ) # sidebarPanel
        }) # renderUI
        output$mainui <- renderUI({
          mainPanel(
              conditionalPanel(condition = "input.userinterface == 'Upload'",
                   tableOutput('contents')
              ),
              conditionalPanel(condition = "input.userinterface == 'Edit Species'",
                               rHandsontableOutput("hot")
              ),
              conditionalPanel(condition = "input.userinterface == 'Run Marxan'",
                   plotOutput("marxanplot")
              )
          )
        })
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
        sMarxanDir <<- paste0(sUserHome,"/",sSelectDb)
        cat(paste0("sMarxanDir ",sMarxanDir,"\n"))
        AppendLogFile(paste0("sSelectDb ",sSelectDb))
        AppendLogFile(paste0("sMarxanDir ",sMarxanDir))
        if (sPrevious != sMarxanDir)
        {
            if (sSelectDb != "")
            {
                ChangeDatabase()

                # trigger an update of the relevant UI components
                # update BLM
                updateNumericInput(session,"blm",value=as.numeric(sBLM))
                # trigger a refresh of the UI
                irefreshinput <<- irefreshinput + 1
                updateNumericInput(session, "refreshinput", value = irefreshinput)
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

  output$contents <- renderTable({
    # input$file1 is the zip file containing the Marxan database.
    # summarytable lists info gleaned from parsing the Marxan database.

    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)

    file.copy(as.character(inFile$datapath),paste0(sUserSession,"/",as.character(inFile$name)),overwrite=TRUE)

    ptm <- proc.time()
    ParseResult <- ParseMarxanZip(paste0(sUserSession,"/",as.character(inFile$name)),sUserSession,sShinyUserPath,sShinyDataPath,sUserName)
    iElapsed <- (proc.time() - ptm)[3]
    summarytable <- rbind(c("name",as.character(inFile$name)),
                          c("size",paste0(as.character(inFile$size)," bytes")),
                          c("elapsed",paste0(as.character(iElapsed)," seconds")),
                          c("type",as.character(inFile$type))) #,
                          #c("datapath",as.character(inFile$datapath)))
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
          sDatabasePath <- paste0(sUserHome,"/",sDatabase)
          cat(paste0("sDatabasePath ",sDatabasePath,"\n"))
          # create directory
          if (!file.exists(sDatabasePath))
          {
              AddDatabase(sDatabasePath)

              # trigger a refresh of the relevant UI components
              updateSelectInput(session, "database",choices = c(list.dirs(sUserHome)))
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
          iAspectX <<- x__
          iAspectY <<- y__
      }
      if (input$displaywhat == "cluster")
      {
          iAspectX <<- 1
          iAspectY <<- 1
      }
    }
  })

  output$marxanplot <- renderPlot({

      input$refreshinput
      input$m

      if (!is.null(input$displaywhat))
      {
          AppendLogFile(paste0("output$marxanplot ",input$displaywhat," ",input$map," ",input$m))

          if (input$displaywhat == "map")
          {
              if (input$map == "ssolnNmap") { map_ssolnNmap() }
              if (input$map == "bestmap") { map_bestmap() }
              if (input$map == "runMmap") { map_runMmap() }

              if (!is.na(puoutline))
              {
                  addLines(puoutline,col="black")
              }
          }
          if (input$displaywhat == "cluster")
          {
              if (input$cluster == "cluster2ds") { cluster_2ds() }
              if (input$cluster == "cluster3ds") { cluster_3ds() }
              if (input$cluster == "clusterdendogram") { cluster_dendogram() }
          }
      }

  }, height=600,width=round(600/iAspectY*iAspectX)) # renderPlot

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
          sprintf(paste0("Welcome ",sUserName))
      } else {
          sprintf("")
      }
  })

  output$textfeedback = renderText({
      runclicked()
      sprintf("Finished")
  })

})
