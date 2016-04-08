# user authenticated
output$sidebarui <- renderUI({
    sidebarPanel(
        textOutput("usermessage"),
        br(),
        a("marxan.io user guide", href=paste0("http://marxan.net/downloads/",sUserGuide), target="_blank"),
        br(),
        br(),
        selectInput("database","Database:",
                    choices = c(list.dirs(sAppHome)),
                    selected = sSelectDb),
        textOutput("textfeedback"),
        br(),
        actionButton("mrun","Run"),
        br(),
        br(),
        radioButtons("displaywhat", "Display:",
                     list("Map" = "map",
                          "Table" = "table",
                          "Cluster" = "cluster")),
        conditionalPanel(condition = "input.displaywhat == 'table'",
            radioButtons("tabletype", "Table type:",
                         list("Inputs" = "input",
                              "Outputs" = "output")),
            conditionalPanel(condition = "input.tabletype == 'input'",
                radioButtons("table_i", "Table:",
                             #testx()),
                             generate_input_files_list()),
                             #c("spec","zones","costs","zonecost",
                             #  "zoneboundcost","zonecontrib","zonetarget")),
                             #c("Features" = "spec",
                             #     "Zones" = "zones",
                             #     "Costs" = "costs",
                             #     "Zone cost" = "zonecost",
                             #     "Zone boundary cost" = "zoneboundcost",
                             #     "Zone contribution" = "zonecontrib",
                             #     "Zone target" = "zonetarget")),
                conditionalPanel(condition = "input.table_i == 'spec'",
                    actionButton("saveBtn", "Save features")
                )
            ),
            conditionalPanel(condition = "input.tabletype == 'output'",
                radioButtons("table_o", "Table:",
                             list("Summary" = "sumtable",
                                  "Best solution Missing values" = "mvbesttable",
                                  "Solution M Missing values" = "mvNtable"))
            )
        ),
        conditionalPanel(condition = "input.displaywhat == 'map'",
            radioButtons("map", "Map:",
                         list("Best solution" = "bestmap",
                              "Solution M" = "runMmap",
                              "Selection frequency zone N" = "ssolnNmap"))
        ),
        conditionalPanel(condition = "(input.displaywhat == 'map' & input.map == 'runMmap') | (input.displaywhat == 'table' & input.tabletype == 'output' & input.table_o == 'mvNtable')",
            sliderInput("m", "Solution M:",value = 1,min = 1,max = 100,step = 1)
        ),
        conditionalPanel(condition = "input.displaywhat == 'cluster'",
            radioButtons("cluster", "Cluster:",
                         list("NMDS" = "cluster2ds",
                              "Dendogram" = "clusterdendogram"))
        ),
        conditionalPanel(condition = "(input.displaywhat == 'map' & (input.map == 'bestmap' | input.map == 'runMmap')) | input.displaywhat == 'cluster'",
            HTML(generate_ssoln_html_legend())
        ),
        conditionalPanel(condition = "input.displaywhat == 'map'",
            conditionalPanel(condition = "input.map == 'ssolnNmap'",
                sliderInput("n", "Zone N:",value = iZones,min = 1,max = iZones,step = 1),
                textOutput("zonename"),
                HTML("<img src='http://marxan.net/images/blue_5.png' /></a>"),
                HTML("100"),
                br(),
                HTML("<img src='http://marxan.net/images/blue_4.png' /></a>"),
                HTML("70-99"),
                br(),
                HTML("<img src='http://marxan.net/images/blue_3.png' /></a>"),
                HTML("30-69"),
                br(),
                HTML("<img src='http://marxan.net/images/blue_2.png' /></a>"),
                HTML("1-29"),
                br(),
                HTML("<img src='http://marxan.net/images/white.png' /></a>"),
                HTML("0")
            )
        )
    ) # sidebarPanel
}) # renderUI
output$mainui <- renderUI({
    mainPanel(
        conditionalPanel(condition = "input.displaywhat == 'map'",
            plotOutput("marzonemap")
        ),
        conditionalPanel(condition = "input.displaywhat == 'table'",
            conditionalPanel(condition = "input.tabletype == 'input'",
                conditionalPanel(condition = "input.table_i == 'spec'",
                    rHandsontableOutput("hot")
                ),
                conditionalPanel(condition = "(input.table_i == 'zones' | input.table_i == 'zoneboundcost' | input.table_i == 'zonecontrib' | input.table_i == 'zonecost' | input.table_i == 'costs' | input.table_i == 'zonetarget')",
                    tableOutput("marzoneinputtable")
                )
            ),
            conditionalPanel(condition = "input.tabletype == 'output'",
                tableOutput("marzoneoutputtable")
            )
        ),
        conditionalPanel(condition = "input.displaywhat == 'cluster'",
            plotOutput("marzoneplot")
        )
    )
})
