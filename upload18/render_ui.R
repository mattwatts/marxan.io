        # user authenticated
        output$sidebarui <- renderUI({
          sidebarPanel(
              textOutput("usermessage"),
              textOutput("userLocation"),
              textOutput("lastLogin"),
              br(),
              fileInput('file1', 'Choose Marxan zip file to upload',accept = c('.zip')),
              HTML("Upload a zip file containing your Marxan dataset, planning unit shapefile, and study region outline shapefile."),
              HTML("When you upload your file, there will be a delay while the file is processed and your data is extracted and ingested. When this is done an information grid will be presented to you."),
              br(),
              br(),
              textInput("uploadname","Database Name:",value=""),
              actionButton("acceptupload","Accept Database"),
              br(),
              br(),
              HTML("Give your analysed database a name and accept it for it to stored and made available for you to use."),
              HTML("When you accept your database, there will be a delay before you can use it while preprocessing occurs."),
              textOutput("feedbackupload")
          ) # sidebarPanel
        }) # renderUI
        output$mainui <- renderUI({
            mainPanel(
                tableOutput('contents')
            )
        })
