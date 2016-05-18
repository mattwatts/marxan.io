# marxan.io

library(shiny)
library(iptools)

Logged = FALSE;
load(file=paste0(sShinyPath,"/passwd.Rdata"))
PASSWORD <- passwd

shinyServer(function(input, output, session, clientData) {

    # restart login app for each http get
    system(paste0("touch ",sAppDir,"/restart.txt"))

    source(paste0(sShinySourcePath,"/server_pre_marxan.R"),  local = TRUE)

    sAnonLogFile <<- CreateLogFile(sShinyTempPath,"anonymous","login")
    AppendLogFile(sAnonLogFile,"login app start")

    source(paste0(sAppDir,"/login.R"),  local = TRUE)

    observe({
        if (USER$Logged == TRUE)
        {
            InitialiseUserSession()
            #iLogout <<- 0
        }
        # user authenticated - render the user interface
        output$sidebarui <- renderUI({
          if (USER$Logged == TRUE)
          {
              sidebarPanel(
                  textOutput("usermessage"),
                  br(),
                  textOutput("userLocation"),
                  textOutput("lastLogin"),
                  br(),
                  a("marxan.io user guide", href=paste0("http://marxan.net/downloads/",sUserGuide), target="_blank"),
                  br(),
                  br(),
                  a("Run Marxan", href=paste0("http://",sAppServer,"/rshiny/apps/",sUserName,"/",sMarxanApp,"/?session=",sUserSessionKey), target="_blank"),
                  br(),
                  br(),
                  a("Parameter testing", href=paste0("http://",sAppServer,"/rshiny/apps/",sUserName,"/",sMarxanParamTestApp,"/?session=",sUserSessionKey), target="_blank"),
                  br(),
                  br(),
                  a("Run MarZone", href=paste0("http://",sAppServer,"/rshiny/apps/",sUserName,"/",sMarZoneApp,"/?session=",sUserSessionKey), target="_blank"),
                  br(),
                  br(),
                  a("Upload a dataset", href=paste0("http://",sAppServer,"/rshiny/apps/",sUserName,"/",sUploadApp,"/?session=",sUserSessionKey), target="_blank"),
                  br(),
                  br(),
                  a("Manage datasets", href=paste0("http://",sAppServer,"/rshiny/apps/",sUserName,"/",sManageApp,"/?session=",sUserSessionKey), target="_blank"),
                  br(),
                  br(),
                  h4("Housekeeping"),
                  br(),
                  HTML("Please report all problems to <a href='mailto:m.watts@uq.edu.au?subject=marxan.io problems'>Matt Watts</a>."),
                  br(),
                  br(),
                  HTML("Do not start more than one instance of 'Run Marxan', 'Parameter testing' or 'Run MarZone'. This saturates the system for all users and could result in a ban."),
                  br(),
                  br(),
                  HTML("Do not press the 'Run' button in 'Run Marxan', 'Parameter testing' or 'Run MarZone' again until your runs finish. This saturates the system for all users and could result in a ban.")
              ) # sidebarPanel
          }
        }) # renderUI
    }) # observe

    observe({
        if (USER$Logged == TRUE)
        {
            if (!is.null(input$logout))
            {
                cat(paste0("input$logout ",input$logout," iLogout ",iLogout,"\n"))

                if (input$logout > iLogout)
                {
                    iLogout <<- input$logout
                    # user clicked logout
                    cat(paste0("click logout ",input$logout," iLogout ",iLogout,"\n"))
                    LogoutSession()
                }
            }
        }
    })

    output$usermessage = renderText({
        if (USER$Logged == TRUE)
        {
            sprintf(paste0("Hello ",sUserName))
        } else {
            sprintf("")
        }
    })

    observe({
        sUserIP <<- as.character(input$ipid)
        UserGeoIP <<- freegeoip(sUserIP)
        Hostname <- ip_to_hostname(sUserIP)
        sUserHostname <<- Hostname[[1]]
    })

    output$userLocation <- renderText({
        paste0("Login from ",sUserHostname)
    })

    observe({
        if (USER$Logged == TRUE)
        {
            # User has logged in. Record details about the HTTP session.
            query <- parseQueryString(session$clientData$url_search)
            sText <- paste0("fingerprint: ", input$fingerprint,"\n",
                            "ip: ", sUserIP,"\n",
                            "userhostname: ",sUserHostname,"\n",
                            "protocol: ", session$clientData$url_protocol, "\n",
                            "hostname: ", session$clientData$url_hostname, "\n",
                            "pathname: ", session$clientData$url_pathname, "\n",
                            "port: ",     session$clientData$url_port,     "\n",
                            "search: ",   session$clientData$url_search,   "\n",
                            "queries: ",paste(names(query), query, sep = "=", collapse=", "),"\n")

            AppendLogFile(sLogFile,sText)
            cat(paste0(sText,"\n"))
        }
    })

    output$lastLogin <- renderText({
        if (USER$Logged == TRUE)
        {
            sLastLogin <- paste0(sUserHome,"/lastLogin.Rdata")
            if (file.exists(sLastLogin))
            {
                load(file=sLastLogin)
                sMessage <- paste0("Last login ",as.character(LastLoginDate)," from ",sUserLastHostname)
            } else {
                sMessage <- "First login"
            }

            LastLoginDate <- date()
            sUserLastIP <- sUserIP
            sUserLastHostname <- sUserHostname
            UserLastGeoIP <- UserGeoIP
            save(LastLoginDate,sUserLastIP,sUserLastHostname,UserLastGeoIP,file=sLastLogin)

            sMessage
        }
    })
})
