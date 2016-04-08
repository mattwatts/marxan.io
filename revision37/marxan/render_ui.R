    # user authenticated
    output$sidebarui <- renderUI({
      sidebarPanel(
          textOutput("usermessage"),
          br(),
          a("marxan.io user guide", href=paste0("http://marxan.net/downloads/",sUserGuide), target="_blank"),
          br(),
          br(),
          radioButtons("userinterface","Interface:",
                       list("Run Marxan","Edit species")),
          br(),
          selectInput("database","Database:",
                      choices = c(list.dirs(sAppHome)),
                      selected = sSelectDb),
          conditionalPanel(condition = "input.userinterface == 'Edit species'",
                           br(),
                           actionButton("saveBtn", "Save")
          ),
          conditionalPanel(condition = "input.userinterface == 'Run Marxan'",
              textOutput("textfeedback"),
              br(),
              actionButton("mrun","Run"),
              br(),
              br(),
              numericInput("blm", "BLM:",sBLM,min=0),
              br(),
              radioButtons("displaywhat", "Display:",
                           list("Map" = "map",
                                "Cluster" = "cluster")),
              conditionalPanel(condition = "input.displaywhat == 'map'",
                  radioButtons("map", "Map:",
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
                  radioButtons("cluster", "Cluster:",
                               list("NMDS" = "cluster2ds",
                                    "Dendogram" = "clusterdendogram"))
              ),
              conditionalPanel(condition = "input.map == 'runMmap' & input.displaywhat == 'map'",
                  br(),
                  sliderInput("m", "Solution M:",
                              value = 1,
                              min = 1,
                              max = 100, step = 1)
              )
          )
      ) # sidebarPanel
    }) # renderUI
    output$mainui <- renderUI({
        mainPanel(
            conditionalPanel(condition = "input.userinterface == 'Edit species'",
                rHandsontableOutput("hot")
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
