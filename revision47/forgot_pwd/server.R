# marxan.io

library(shiny)
library(shinyBS)
library(iptools)
library(mailR)

load(file=paste0(sShinyPath,"/accountGmail.Rdata"))

send_email <- function(sEmail,sSubject,sBody)
{
    sender <- sGmailAddress
    recipients <- c(sEmail)
    send.mail(from = sender,
              to = recipients,
              subject = sSubject,
              body = sBody,
              smtp = list(host.name = "smtp.gmail.com", port = 465,
                          user.name = sGmailAddress,
                          passwd = sGmailPassword, ssl = TRUE),
              authenticate = TRUE,
              send = TRUE)
}

append_log_table <- function()
{
    col_names <- c("date","sessionkey","fingerprint","ip","userhostname","email")
    sDate <- date()

    # append csv file
    sLogTable <- paste0(sShinyPath,"/forgot_pwd_log.csv")
    if (!file.exists(sLogTable))
    {
        # write file header
        sLine <- paste0(col_names,collapse=",")
        write(sLine,file=sLogTable)
    }
    sLine <- paste(sDate,sUserSessionKey,sFingerprint,sUserIP,sUserHostname,sEmail,sep=",")
    write(sLine,file=sLogTable,append=TRUE)
    
    # append data frame
    sLogRdata <- paste0(sShinyPath,"/forgot_pwd_log.Rdata")
    if (file.exists(sLogRdata))
    {
        load(file=sLogRdata)
        
        arow <- cbind(sDate,sUserSessionKey,sFingerprint,sUserIP,sUserHostname,sEmail)
        colnames(arow) <- col_names
        forgot_pwd_df <- rbind(forgot_pwd_df,arow)
        
    } else {
        arow <- cbind(sDate,sUserSessionKey,sFingerprint,sUserIP,sUserHostname,sEmail)
        forgot_pwd_df <- as.data.frame(arow)
        colnames(forgot_pwd_df) <- col_names
    }
    save(forgot_pwd_df,file=sLogRdata)
}

shinyServer(function(input, output, session, clientData) {

    source(paste0(sShinySourcePath,"/server_pre_marxan.R"),  local = TRUE)

    sLogFile <<- CreateLogFile(sShinyTempPath,"anonymous","forgot_pwd")
    AppendLogFile(sLogFile,"forgot_pwd app start")

    observe({
        sUserIP <<- as.character(input$ipid)
        cat(paste0("sUserIP ",sUserIP,"\n"))
    })

    observe({
        sFingerprint <<- as.character(input$fingerprint)
        cat(paste0("sFingerprint ",sFingerprint,"\n"))
    })

    observe({
            # render the user interface
            source(paste0(sAppDir,"/render_ui.R"),  local = TRUE)
    })

    observe({
        sEmail <<- input$email
    })

    observe({
        iRegisterUser <- input$registerUser

        cat(paste0("iRegisterUser ",iRegisterUser,"\n"))

        if (!is.null(input$registerUser))
        if (input$registerUser > 0)
        {
            # validate info entered by user
            fValid <- TRUE
            fShowHint <- FALSE

            # show messages if invalid
            if (sEmail == "")        { fShowHint <- TRUE }

            updateNumericInput(session, "showhint", value = 0)
            if (fShowHint)
            {
                updateNumericInput(session, "showhint", value = 1)
                fValid <- FALSE
            }

            updateNumericInput(session, "showinvalid", value = 0)
            if (fValid)
            {
                sPwdFile <- paste0(sShinyPath,"/passwords/passwd_",sEmail,".Rdata")
                if (!file.exists(sPwdFile))
                {
                    fValid <- FALSE
                    updateNumericInput(session, "showinvalid", value = 1)
                }
            }

            if (fValid)
            {
                # accept and display message because valid response
                updateNumericInput(session, "showform", value = 0)
                updateNumericInput(session, "showok", value = 1)
                
                AppendLogFile(sLogFile,"reset accepted")
                
                # generate password reset session
                sUserName <<- sEmail
                InitialiseUserSession()

                # send them the password reset and notification email
                isendemail <<- isendemail + 1
                updateNumericInput(session, "sendemail", value = isendemail)
            }
        }
    })

    send_the_email <- function()
    {
        sSubject <- paste0("marxan.io reset password")
        sMessage <- paste0("Hi ",sEmail,"\n\n",
                           "Click here to reset the password for your account:\n\n",
                           "http://",sAppServer,"/rshiny/apps/",sPwdResetApp,"/?session=",sUserSessionKey,
                           "\n\n",
                           "The password reset link will expire in 12 hours. If you have any problems please email m.watts@uq.edu.au for support.\n\n",
                           "fingerprint: ",sFingerprint,"\n",
                           "ip: ",sUserIP,"\n",
                           "userhostname: ",sUserHostname,"\n",
                           "email: ",sEmail,"\n")

        AppendLogFile(sLogFile,sSubject)
        AppendLogFile(sLogFile,sMessage)
        append_log_table()

        send_email(sEmail,sSubject,sMessage)
        send_email("m.watts@uq.edu.au",sSubject,sMessage)
    }

    observe({
        if (!is.null(input$sendemail))
        {
            cat(paste0("input$sendemail ",input$sendemail,"\n"))
            if (input$sendemail > 0)
            {
                send_the_email()
            }
        }
    })

    output$usermessage = renderText({
        sprintf("Hello")
    })

    observe({
        sUserIP <<- as.character(input$ipid)
        #UserGeoIP <<- freegeoip(sUserIP)
        Hostname <- ip_to_hostname(sUserIP)
        sUserHostname <<- Hostname[[1]]
    })

    observe({

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
     })
})
