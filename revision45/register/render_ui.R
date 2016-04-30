    output$mainui <- renderUI({
        mainPanel(
            h1("Register user for marxan.io"),
            br(),
            conditionalPanel(condition = "input.showform == '1'",
                HTML("To register for this free service, please fill in your details and agree to the conditions set out below."),
                br(),
                conditionalPanel(condition = "input.showhint == '1'",
                    br(),
                    HTML(paste0("", tags$span(style="color:red", "Enter name, organisation, email")))
                ),
                textInput("name","Your name:"),
                textInput("organisation","Your organisation:"),
                selectInput("country","Your country:",choices=countrycode_data$country.name,selected="Australia"),
                textInput("email","Your email address:"),
                checkboxInput("subscribe","Subscribe to the Marxan mailing list?",value=TRUE),
                selectInput("industry","Your industry:",choices=industry_list),
                #textInput("otherindustry","Other industry:"),
                selectInput("researchinterest","Your research interest:",choices=research_interest_list),
                #textInput("otherresearchinterest","Other research interest:"),
                conditionalPanel(condition = "input.showagree == '1'",
                    br(),
                    HTML(paste0("", tags$span(style="color:red", "You need to agree")))
                ),            
                checkboxInput("informauthors","You agree to inform the the authors of any publications, applications for funding, funding acquired and all other applications associated with this software",value=FALSE),
                checkboxInput("acknowledgeip","You agree to acknowledge the intellectual property of the authors in all published work, applications and dealings with this software",value=FALSE),
                br(),
                actionButton('registerUser', 'Register'),
                br()
            ),
            conditionalPanel(condition = "input.showok == '1'",
                HTML("Registration accepted."),
                HTML("A password reset email has been sent to you. The password reset link will expire in 12 hours."),
                br(),
                br(),
                actionButton('registerAnotherUser', 'Register another user'),
                br(),
                br(),
                tableOutput("registertable"),
                br()
            )
      )
    })
