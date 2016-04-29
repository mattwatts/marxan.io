output$sidebarui <- renderUI({
    sidebarPanel(
        textOutput("usermessage"),
        br(),
        a("marxan.io user guide",href=paste0("http://marxan.net/downloads/",sUserGuide),target="_blank"),
        br(),
        br(),
        selectInput("database","Database:",choices=c(list.dirs(sAppHome)),selected=sSelectDb),
        textOutput("textfeedback"),
        br(),
        actionButton("mrun","Run"),
        br(),
        br(),
        radioButtons("displaywhat", "Display:",list("Map"="map","Table"="table","Cluster"="cluster")),
        conditionalPanel(condition="input.displaywhat == 'table'",
            radioButtons("tabletype","Table type:",list("Inputs"="input","Outputs"="output")),
            conditionalPanel(condition = "input.tabletype == 'input'",
                radioButtons("table_i","Table:",generate_input_files_list()),
                conditionalPanel(condition = "input.table_i == 'feat'",
                    actionButton("saveSpecBtn", "Save features")
                ),
                conditionalPanel(condition = "input.table_i == 'zonecost'",
                    actionButton("saveZoneCostBtn", "Save zone cost")
                ),
                conditionalPanel(condition = "input.table_i == 'zonetarget'",
                    actionButton("saveZoneTargetBtn", "Save zone target")
                ),
                conditionalPanel(condition = "input.table_i == 'zonecontrib'",
                    actionButton("saveZoneContribBtn", "Save zone contrib")
                ),
                conditionalPanel(condition = "input.table_i == 'zonebound'",
                    actionButton("saveZoneBoundCostBtn", "Save zone bound")
                )
            ),
            conditionalPanel(condition = "input.tabletype == 'output'",
                radioButtons("table_o","Table:",
                             list("Summary"="sumtable","Best solution Missing values"="mvbesttable",
                                  "Solution M Missing values"="mvNtable"))
            )
        ),
        conditionalPanel(condition = "input.displaywhat == 'map'",
            conditionalPanel(condition = "input.displayleaflet == 1",
                radioButtons("map_service","Map service:",list("Open Street Map"="OpenStreetMap","ESRI World Imagery"="ESRI")),
                sliderInput("opacity", "Opacity:",value=0.6,min=0,max=1)
            ),
            radioButtons("map", "Map:",
                         list("Best solution"="bestmap","Solution M"="runMmap",
                              "Selection frequency zone N"="ssolnNmap"))
        ),
        conditionalPanel(condition="(input.displaywhat == 'map' & input.map == 'runMmap') | (input.displaywhat == 'table' & input.tabletype == 'output' & input.table_o == 'mvNtable')",
            sliderInput("m","Solution M:",value=1,min=1,max=100,step=1)
        ),
        conditionalPanel(condition = "input.displaywhat == 'cluster'",
            radioButtons("cluster", "Cluster:",list("NMDS"="cluster2ds","Dendogram"="clusterdendogram"))
        ),
        conditionalPanel(condition = "(input.displaywhat == 'map' & (input.map == 'bestmap' | input.map == 'runMmap')) | input.displaywhat == 'cluster'",
            HTML(generate_ssoln_html_legend())
        ),
        conditionalPanel(condition = "input.displaywhat == 'map'",
            conditionalPanel(condition = "input.map == 'ssolnNmap'",
                sliderInput("n","Zone N:",value=iZones,min=1,max=iZones,step=1),
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
                HTML("1-29")
            )
        )
    ) # sidebarPanel
}) # renderUI
output$mainui <- renderUI({
    mainPanel(
        conditionalPanel(condition="input.displaywhat == 'map'",
            #conditionalPanel(condition="input.displayleaflet == 1",
                #leafletOutput("leafletmap",width="100%",height=750)
            #),
            #conditionalPanel(condition="input.displayleaflet == 0",
                plotOutput("marzonemap",width="100%",height=750)
            #)
        ),
        conditionalPanel(condition="input.displaywhat == 'table'",
            conditionalPanel(condition="input.tabletype == 'input'",
                conditionalPanel(condition="input.table_i == 'feat'",
                    rHandsontableOutput("hot")
                ),
                conditionalPanel(condition="input.table_i == 'zonecost'",
                    rHandsontableOutput("hot_zonecost")
                ),
                conditionalPanel(condition="input.table_i == 'zonetarget'",
                    rHandsontableOutput("hot_zonetarget")
                ),
                conditionalPanel(condition="input.table_i == 'zonecontrib'",
                    rHandsontableOutput("hot_zonecontrib")
                ),
                conditionalPanel(condition="input.table_i == 'zonebound'",
                    rHandsontableOutput("hot_zoneboundcost"),
                    br(),
                    HTML(paste0("The matrix is symmetrical and ", tags$span(style="color:blue", "blue")," cells are ignored"))
                ),
                conditionalPanel(condition="(input.table_i == 'zones' | input.table_i == 'costs')",
                    tableOutput("marzoneinputtable")
                )
            ),
            conditionalPanel(condition="input.tabletype == 'output'",
                tableOutput("marzoneoutputtable")
            )
        ),
        conditionalPanel(condition="input.displaywhat == 'cluster'",
            plotOutput("marzoneplot")
        )
    )
})
