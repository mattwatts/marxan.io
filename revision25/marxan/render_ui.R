        # user authenticated
        output$sidebarui <- renderUI({
          sidebarPanel(
              textOutput("usermessage"),
              br(),
              radioButtons("userinterface","User Interface:",
                           list("Run Marxan","Edit Species","Parameter Testing")),
              #a("Upload dataset", href=paste0("http://marxan.io/rshiny/apps/",sUserName,"/",sUploadApp,"/?session=",sUserSessionKey)),
              #br(),
              br(),
              conditionalPanel(condition = "(input.userinterface == 'Edit Species') || (input.userinterface == 'Run Marxan') || (input.userinterface == 'Parameter Testing')",
                  selectInput("database","Database:",
                              choices = c(list.dirs(sMarxanHome)),
                              selected = sSelectDb)
              ),
              conditionalPanel(condition = "input.userinterface == 'Edit Species'",
                               br(),
                               actionButton("saveBtn", "Save")
              ),
              conditionalPanel(condition = "input.userinterface == 'Run Marxan'",
                  textOutput("textfeedback"),
                  br(),
                  actionButton("mrun","Run"),
                  br(),
                  numericInput("blm", "BLM:",sBLM,min=0),
                  br(),
                  radioButtons("displaywhat", "Display output:",
                               list("Map" = "map",
                                    "Cluster" = "cluster")),
                                    #"Cluster 2d" = "cluster",
                                    #"Cluster 3d" = "cluster3d")),
                  conditionalPanel(condition = "input.displaywhat == 'map'",
                      radioButtons("map", "Map to display:",
                                   list("Best solution" = "bestmap",
                                        "Solution M" = "runMmap",
                                        "Selection frequency" = "ssolnNmap")),
                      conditionalPanel(condition = "(input.map == 'bestmap' | input.map == 'runMmap')",
                          HTML("<img src='http://marxan.io/images/white.png' /></a>"),
                          HTML("Available"),
                          br(),
                          HTML("<img src='http://marxan.io/images/blue_5.png' /></a>"),
                          HTML("Selected"),
                          br(),
                          HTML("<img src='http://marxan.io/images/turquoise.png' /></a>"),
                          HTML("Existing Reserve"),
                          br(),
                          HTML("<img src='http://marxan.io/images/grey.png' /></a>"),
                          HTML("Excluded")
                      ),
                      conditionalPanel(condition = "input.map == 'ssolnNmap'",
                          HTML("Selection frequency"),
                          br(),
                          HTML("<img src='http://marxan.io/images/blue_5.png' /></a>"),
                          HTML("100"),
                          br(),
                          HTML("<img src='http://marxan.io/images/blue_4.png' /></a>"),
                          HTML("70-99"),
                          br(),
                          HTML("<img src='http://marxan.io/images/blue_3.png' /></a>"),
                          HTML("30-69"),
                          br(),
                          HTML("<img src='http://marxan.io/images/blue_2.png' /></a>"),
                          HTML("1-29"),
                          br(),
                          HTML("<img src='http://marxan.io/images/white.png' /></a>"),
                          HTML("0"),
                          br(),
                          HTML("<img src='http://marxan.io/images/turquoise.png' /></a>"),
                          HTML("Existing Reserves"),
                          br(),
                          HTML("<img src='http://marxan.io/images/grey.png' /></a>"),
                          HTML("Excluded")
                      )
                  ),
                  conditionalPanel(condition = "input.displaywhat == 'cluster'",
                      radioButtons("cluster", "Cluster to display:",
                                   list("NMDS" = "cluster2ds",
                                        #"3d" = "cluster3ds",
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
                  textOutput("paramtestfeedback"),
                  br(),
                  actionButton("mptrun","Run"), 
                  br(),
                  selectInput("whichparam", "Parameter to test:",
                              choices = c("BLM Calibration","SPF Calibration","Target Sensitivity")),
                  conditionalPanel(condition = "input.whichparam == 'SPF Calibration' | input.whichparam == 'Target Sensitivity'",
                      numericInput("userblm", "BLM:",0,min=0)
                  ),
                  conditionalPanel(condition = "input.whichparam == 'BLM Calibration' | input.whichparam == 'Target Sensitivity'",
                      numericInput("userspf", "SPF:",1,min=0)
                  ),
                  conditionalPanel(condition = "input.whichparam == 'BLM Calibration' | input.whichparam == 'SPF Calibration'",
                      numericInput("usertarg", "Target:",0.3,min=0,max=1)
                  ),
                  conditionalPanel(condition = "input.whichparam == 'BLM Calibration'",
                      numericInput("rampBLMmin", "BLM min:",0,min=0),
                      numericInput("rampBLMmax", "BLM max:",10000000000000,min=0)
                  ),
                  conditionalPanel(condition = "input.whichparam == 'SPF Calibration'",
                      numericInput("rampSPFmin", "SPF min:",0.0001,min=0),
                      numericInput("rampSPFmax", "SPF max:",10000000000000,min=0)
                  ),
                  conditionalPanel(condition = "input.whichparam == 'Target Sensitivity'",
                      numericInput("targetmin", "Target min:",0,min=0,max=1),
                      numericInput("targetmax", "Target max:",1,min=0,max=1)
                  ),
                  br(),        
                  selectInput("whichmap", "Value to display:",
                      choices = as.character(rep(1:iCores))),
                  selectInput("whichrun", "Map to display:",
                      choices = c("Best Solution",paste0("Run ",rep(1:iRepsPerCore)),"Selection Frequency")),
                  checkboxInput("plotvalues", "Plot Parameter Values", value = TRUE)
              )
          ) # sidebarPanel
        }) # renderUI
        output$mainui <- renderUI({
            mainPanel(
                conditionalPanel(condition = "input.userinterface == 'Edit Species'",
                    rHandsontableOutput("hot")
                ),
                conditionalPanel(condition = "input.userinterface == 'Parameter Testing'",
                    tableOutput('pttable'),
                    plotOutput('ptplot'),
                    plotOutput('ptmap')
                ),
                conditionalPanel(condition = "input.userinterface == 'Run Marxan'",
                    conditionalPanel(condition = "input.displaywhat == 'cluster3d'",
                        webGLOutput("marxan3d")
                    ),
                    conditionalPanel(condition = "input.displaywhat == 'cluster'",
                        plotOutput("marxanplot")
                    ),
                    conditionalPanel(condition = "input.displaywhat == 'map'",
                        plotOutput("marxanmap")
                    )
                )
          )
        })
