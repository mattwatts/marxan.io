output$mainui <- renderUI({
    mainPanel(
        h2("Reset password for marxan.io"),
        br(),
        a("marxan.io user guide", href=paste0("http://marxan.net/downloads/",sUserGuide), target="_blank"),
        br(),
        br(),
        conditionalPanel(condition = "input.showform == '1'",
            HTML("To reset the password for your account, please enter the email address you registered with below."),
            br(),
            br(),
            conditionalPanel(condition = "input.showhint == '1'",
                br(),
                HTML(paste0("", tags$span(style="color:red", "Enter email")))
            ),
            conditionalPanel(condition = "input.showinvalid == '1'",
                br(),
                HTML(paste0("", tags$span(style="color:red", "Account doesn't exist")))
            ),
            textInput("email","Your email address:"),
            br(),
            actionButton('registerUser', 'Reset password'),
            br()
        ),
        conditionalPanel(condition = "input.showok == '1'",
            HTML("A password reset email has been sent to you. The password reset link will expire in 12 hours."),
            br()
        )
  )
})
