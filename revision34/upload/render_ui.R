        # user authenticated
        output$sidebarui <- renderUI({
          sidebarPanel(
              textOutput("usermessage"),
              #textOutput("userLocation"),
              #textOutput("lastLogin"),
              br(),
              a("marxan.io user guide", href="http://marxan.net/downloads/Marxan_io_rev25_user_guide.pdf", target="_blank"),
              br(),
              br(),
              fileInput('file1', 'Choose Marxan zip file to upload',accept = c('.zip')),
              HTML("Upload a zip file containing your Marxan dataset and planning unit shapefile."),
              HTML("When you upload your file, there will be a delay while the file. A blue info box appears in top right of screen during processing. When this is done an information grid will appear."),
              conditionalPanel(condition = "input.showacceptcontrols == 1",
                  br(),
                  br(),
                  textInput("uploadname","Database Name:",value=""),
                  actionButton("acceptupload","Accept Database"),
                  br(),
                  br(),
                  HTML("Give your analysed database a name and accept it for it to stored and made available for you to use.")
              ),
              #conditionalPanel(condition = "input.showacceptfeedback == 1",
                  #br(),
                  #br(),
                  textOutput("feedbackupload")
              #)
              #a("Run Marxan", href=paste0("http://marxan.io/rshiny/apps/",sUserName,"/",sMarxanApp,"/?session=",sUserSessionKey))
          ) # sidebarPanel
        }) # renderUI
        output$mainui <- renderUI({
            mainPanel(
                tableOutput('contents')
            )
        })
