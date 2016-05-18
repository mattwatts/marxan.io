# user authenticated
output$sidebarui <- renderUI({
    sidebarPanel(
        textOutput("usermessage"),
        br(),
        a("marxan.io user guide", href=paste0("http://marxan.net/downloads/",sUserGuide), target="_blank"),
        br(),
        br(),
        conditionalPanel(condition = "input.tabs == 'My data'",
            selectInput("database","My database:",
                        choices = c(list.dirs(sMarxanHome),list.dirs(sMarZoneHome)),
                        selected = sSelectDb),
            conditionalPanel(condition = "input.renamemydata == '0'",
                actionButton("renameData","Rename")
            ),
            conditionalPanel(condition = "input.renamemydata == '1'",
                textInput("renameName","New name for dataset"),
                actionButton("acceptName","Ok"),
                actionButton("cancelName","Cancel")
            ),
            br(),
            wellPanel(
                HTML("Share my data:"),
                br(),
                br(),
                conditionalPanel(condition = "input.sharemydata == '1'",
                    actionButton('sharePublic', 'Public'),
                    #actionButton('sharePrivate', 'Private'),
                    #actionButton('editShare', 'Edit share')
                    actionButton('removeShare', 'Remove share')
                ),
                conditionalPanel(condition = "input.sharemydatapublic == '1'",
                    HTML("Share data publicly to all users?"),
                    br(),
                    br(),
                    actionButton("publicOk","Yes"),
                    actionButton("publicCancel","Cancel")
                ),
                conditionalPanel(condition = "input.sharemydataremove == '1'",
                    HTML("Remove publicly shared data?"),
                    br(),
                    br(),
                    actionButton("removeOk","Yes"),
                    actionButton("removeCancel","Cancel")
                ),
                conditionalPanel(condition = "input.sharemydataprivate == '1'",
                    HTML("Share data privately to selected users?"),
                    br(),
                    br(),
                    textInput("publicAccUserName","Username"),
                    actionButton("publicAddUser","Add user"),
                    br(),
                    br(),
                    actionButton("privateOk","Ok"),
                    actionButton("privateCancel","Cancel")
                ),
                conditionalPanel(condition = "input.sharemydataedit == '1'",
                    HTML("Edit dataset share?"),
                    br(),
                    br(),
                    actionButton("editOk","Ok"),
                    actionButton("editCancel","Cancel")
                )
            ),
            downloadButton('downloadData', 'Download'),
            checkboxInput("windowseoln", "Windows text files", value = FALSE),
            br(),
            conditionalPanel(condition = "input.areyousure == '1'",
                actionButton("yesimsure","Delete: are you sure?"),
                actionButton("cancelDelete","Cancel")
            ),
            conditionalPanel(condition = "input.areyousure == '0'",
                actionButton("deletedb","Delete")
            ),
            br()
        ),
        conditionalPanel(condition = "input.tabs == 'Public data'",
            selectInput("publicdb","Public database:",
                        choices = generate_public_datasets(),
                        selected = generate_1st_public_dataset()),
            conditionalPanel(condition = "input.copypublicdata == '0'",
                actionButton("copyPublic","Copy dataset")
            ),
            conditionalPanel(condition = "input.copypublicdata == '1'",
                textInput("publicName","Name for dataset"),
                actionButton("acceptPublic","Copy"),
                actionButton("cancelPublic","Cancel")
            ),
            br(),
            downloadButton('downloadPublic', 'Download'),
            checkboxInput("windowseolnPublic", "Windows text files", value = FALSE)
        ),
        conditionalPanel(condition = "input.tabs == 'Private data shared with me'",
            selectInput("shareddb","Private database:",
                        choices = c("c","d"),
                        selected = "c"),
            conditionalPanel(condition = "input.copyprivatedata == '0'",
                actionButton("copyPrivate","Copy dataset")
            ),
            conditionalPanel(condition = "input.copyprivatedata == '1'",
                textInput("privateName","Name for dataset"),
                actionButton("acceptPrivate","Copy"),
                actionButton("cancelPrivate","Cancel")
            ),
            br(),
            downloadButton('downloadPrivate', 'Download'),
            checkboxInput("windowseolnPrivate", "Windows text files", value = FALSE)
        )
    ) # sidebarPanel
}) # renderUI
output$mainui <- renderUI({
    mainPanel(
        tabsetPanel(id="tabs",
            tabPanel("My data",tableOutput("mydatatable")),
            tabPanel("Public data",tableOutput("publictable"))#,
            #tabPanel("Private data shared with me",tableOutput("sharedwithmetable"))
        )
    )
})
