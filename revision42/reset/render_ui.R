    # user authenticated
    output$mainui <- renderUI({
        mainPanel(
            h2("Enter new password for marxan.io"),
            br(),
            textOutput("usermessage"),
            br(),
            a("marxan.io user guide", href=paste0("http://marxan.net/downloads/",sUserGuide), target="_blank"),
            br(),
            br(),
            conditionalPanel(condition = "input.showenter == '1'",
                HTML("Enter your new password twice for verification"),
                br(),
                br(),
                br(),
                conditionalPanel(condition = "input.showhelp == '1'",
                    HTML(paste0("", tags$span(style="color:red", "Passwords need to match")))
                ),
                passwdInput("passwd1", "Password:"),
                br(),
                br(),
                passwdInput("passwd2", "Password:"),
                br(),
                br(),
                actionButton("acceptpwd","Accept new password")
            ),
            conditionalPanel(condition = "input.showok == '1'",
                HTML("Your password has been changed.")
            )
      )
    })
