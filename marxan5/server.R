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

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 500*1024^2)

iCores <- 10
iRepsPerCore <- 10

registerDoMC(10)  # the number of CPU cores

Logged = FALSE;
load(file=paste0(sShinyPath,"/passwd.Rdata"))
PASSWORD <- passwd

source(paste0(sAppDir,"/ingest_marxan_data.R"),  local = TRUE)
source(paste0(sAppDir,"/server_pre_marxan.R"),  local = TRUE)

y__ <<- 1
x__ <<- 1

shinyServer(function(input, output, session) {

  system(paste0("touch ",sAppDir,"/restart.txt"))
  source(paste0(sAppDir,"/www/Login.R"),  local = TRUE)

  observe({
    if (USER$Logged == TRUE)
    {
        # user authenticated
        output$sidebarui <- renderUI({
          sidebarPanel(
              textOutput("usermessage"),
              selectInput("userinterface", "Choose User Interface:",
                          choices = c("Upload","Select Database","Run Marxan","Parameter Testing","Download")),
              conditionalPanel(condition = "input.userinterface == 'Upload'",
                  fileInput('file1', 'Choose file to upload',accept = c('.zip')),
                  textInput("uploadname","Database Name:",value=""),
                  actionButton("acceptupload","Accept Database"),
                  textOutput("feedbackupload")
              ),
              conditionalPanel(condition = "input.userinterface == 'Select Database'",
                  selectInput("database","Database:",
                              choices = c(list.dirs(sUserHome)))
              ),
              conditionalPanel(condition = "input.userinterface == 'Download'",
                  HTML("Download")
              ),
              conditionalPanel(condition = "input.userinterface == 'Run Marxan'",
                  textOutput("textfeedback"),
                  br(),
                  actionButton("mrun","Run"),
                  br(),
                  numericInput("blm", "Boundary Length Modifier:",0.1,min=0),
                  br(),
                  selectInput("feature", "Choose a species to edit:",
                              choices = c("All features")), #,specnames)),
                  numericInput("prop", "Target:",0.3,min=0,max=1,step=0.1),
                  numericInput("spf", "SPF:",1,min=0),
                  br(),
                  actionButton("savetargetspf","Save Target and SPF"),
                  br(),
                  radioButtons("displaywhat", "Display output:",
                               list("Map" = "map",
                                    "Cluster 2ds" = "cluster2ds",
                                    "Cluster Dendogram" = "clusterdendogram")),
                  conditionalPanel(condition = "input.displaywhat == 'map'",
                      radioButtons("map", "Map to display:",
                                   list("Best solution" = "bestmap",
                                        "Solution M" = "runMmap",
                                        "Selection frequency" = "ssolnNmap"))
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

      cat("prop\n")

      if (USER$Logged == TRUE)
      if (!is.null(input$prop))
      {
          rprop <<- input$prop
          cat(paste0("rprop ",rprop,"\n"))
          AppendLogFile(paste0("input$prop ",input$prop))
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

  observe({

      cat("spf\n")

      if (USER$Logged == TRUE)
      if (!is.null(input$spf))
      {
          rspf <<- input$spf
          cat(paste0("rspf ",rspf,"\n"))
          AppendLogFile(paste0("input$spf ",input$spf))
      }

  })

  observe({

      cat("feature\n")

      if (USER$Logged == TRUE)
      if (!is.null(input$feature))
      {
          sfeature <<- input$feature

          cat(paste0("sfeature ",sfeature,"\n"))

          # change the target text control for the selected feature
          specdat <- read.csv(paste0(sMarxanDir,"/input/spec.dat"),stringsAsFactors=FALSE)
          for (j in 1:nrow(specdat))
          {
              if (specdat$name[j] == sfeature)
              {
                  updateNumericInput(session, "prop", value = specdat$prop[j])
                  updateNumericInput(session, "spf", value = specdat$spf[j])
              }
          }
          AppendLogFile(paste0("input$feature ",input$feature))
      }
  })

  observe({

      cat("savetargetspf\n")

      if (USER$Logged == TRUE)
      if (!is.null(input$savetargetspf))
      if (input$savetargetspf > 0)
      {
          isavetargetspf <<- input$savetargetspf
          cat(paste0("click savetargetspf ",isavetargetspf,"\n"))

          rtarget <- rprop
          if (rtarget < 0) { rtarget <- 0 }
          if (rtarget > 1) { rtarget <- 1 }
          if (rspf < 0) { rspf <- 0 }

          # save target/spf to spec.dat
          specdat <- read.csv(paste0(sMarxanDir,"/input/spec.dat"),stringsAsFactors=FALSE)
          # change the value only for the row with name == input$feature
          for (j in 1:nrow(specdat))
          {
              if (sfeature == "All features")
              {
                  cat("saving all features...\n")

                  specdat$prop[j] <- rtarget
                  specdat$spf[j] <- rspf
              } else {
                  if (specdat$name[j] == sfeature)
                  {
                      cat(paste0("saving feature ",sfeature,"\n"))
                      specdat$prop[j] <- rtarget
                      specdat$spf[j] <- rspf
                  }
              }
          }
          write.csv(specdat,paste0(sMarxanDir,"/input/spec.dat"),quote=FALSE,row.names=FALSE)
          AppendLogFile(paste0("input$savetargetspf ",input$savetargetspf))
      }
  })

  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.

    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)

    file.copy(as.character(inFile$datapath),paste0(sUserSession,"/",as.character(inFile$name)),overwrite=TRUE)

    ptm <- proc.time()
    ParseResult <- ParseMarxanZip(paste0(sUserSession,"/",as.character(inFile$name)),sUserSession,sShinyUserPath,sShinyDataPath,sUserName)
    iElapsed <- (proc.time() - ptm)[3]
    summarytable <- rbind(c("name",as.character(inFile$name)),
                          c("size",as.character(inFile$size)),
                          c("elapsed",as.character(iElapsed)),
                          c("type",as.character(inFile$type)),
                          c("datapath",as.character(inFile$datapath)))
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
              dir.create(sDatabasePath)
              dir.create(paste0(sDatabasePath,"/input"))
              dir.create(paste0(sDatabasePath,"/output"))
              dir.create(paste0(sDatabasePath,"/pulayer"))
              # copy the marxan files to new directory
              file.copy(paste0(sUserSession,"/marxan/input.dat"),paste0(sDatabasePath,"/input.dat"))
              file.copy(paste0(sUserSession,"/marxan/input/pu.dat"),paste0(sDatabasePath,"/input/pu.dat"))
              file.copy(paste0(sUserSession,"/marxan/input/puorder.dat"),paste0(sDatabasePath,"/input/puorder.dat"))
              file.copy(paste0(sUserSession,"/marxan/input/spec.dat"),paste0(sDatabasePath,"/input/spec.dat"))
              file.copy(paste0(sUserSession,"/marxan/input/sporder.dat"),paste0(sDatabasePath,"/input/sporder.dat"))
              file.copy(paste0(sUserSession,"/marxan/input/bound.dat"),paste0(sDatabasePath,"/input/bound.dat"))
              file.copy(paste0(sUserSession,"/marxan/pulayer/pulayer.Rdata"),paste0(sDatabasePath,"/pulayer/pulayer.Rdata"))
              file.copy(paste0(sUserSession,"/marxan/pulayer/pulayer.dbf"),paste0(sDatabasePath,"/pulayer/pulayer.dbf"))
              file.copy(paste0(sUserSession,"/marxan/pulayer/pulayer.prj"),paste0(sDatabasePath,"/pulayer/pulayer.prj"))
              file.copy(paste0(sUserSession,"/marxan/pulayer/pulayer.shp"),paste0(sDatabasePath,"/pulayer/pulayer.shp"))
              file.copy(paste0(sUserSession,"/marxan/pulayer/pulayer.shx"),paste0(sDatabasePath,"/pulayer/pulayer.shx"))
              file.copy(paste0(sUserSession,"/marxan/pulayer/puoutline.dbf"),paste0(sDatabasePath,"/pulayer/puoutline.dbf"))
              file.copy(paste0(sUserSession,"/marxan/pulayer/puoutline.prj"),paste0(sDatabasePath,"/pulayer/puoutline.prj"))
              file.copy(paste0(sUserSession,"/marxan/pulayer/puoutline.shp"),paste0(sDatabasePath,"/pulayer/puoutline.shp"))
              file.copy(paste0(sUserSession,"/marxan/pulayer/puoutline.shx"),paste0(sDatabasePath,"/pulayer/puoutline.shx"))

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
          if (input$displaywhat == "cluster2ds")
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
          if (input$displaywhat == "clusterdendogram")
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
      }

  }, height=600,width=round(600/y__*x__)) # renderPlot

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
