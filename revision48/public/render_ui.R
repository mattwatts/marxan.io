# user authenticated
output$sidebarui <- renderUI({
    sidebarPanel(
        a("marxan.io user guide", href=paste0("http://marxan.net/downloads/",sUserGuide), target="_blank"),
        br(),
        br(),
        selectInput("publicdb","Public database:",
                    choices = generate_public_datasets(),
                    selected = generate_1st_public_dataset()),
        br(),
        downloadButton('downloadPublic', 'Download'),
        checkboxInput("windowseolnPublic", "Windows text files", value = FALSE)
    ) # sidebarPanel
}) # renderUI
output$mainui <- renderUI({
    mainPanel(
        tableOutput("publictable")
    )
})
