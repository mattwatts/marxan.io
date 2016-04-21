    # user authenticated
    output$sidebarui <- renderUI({
      sidebarPanel(
          textOutput("usermessage"),
          br(),
          a("marxan.io user guide", href=paste0("http://marxan.net/downloads/",sUserGuide), target="_blank"),
          br(),
          br(),
          selectInput("database","Database:",
                      choices = c(list.dirs(sMarxanHome),list.dirs(sMarZoneHome)),
                      selected = sSelectDb),
          textOutput("textfeedback"),
          br(),
          downloadButton('downloadData', 'Download'),
          br(),
          checkboxInput("windowseoln", "Windows text files", value = FALSE),
          br(),
          conditionalPanel(condition = "input.areyousure == '1'",
              actionButton("yesimsure","Delete: are you sure?")
          ),
          conditionalPanel(condition = "input.areyousure == '0'",
              actionButton("deletedb","Delete")
          ),
          br()
      ) # sidebarPanel
    }) # renderUI
    output$mainui <- renderUI({
        mainPanel(
                tableOutput("downloadtable")
      )
    })
