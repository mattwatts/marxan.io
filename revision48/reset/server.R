# marxan.io

library(shiny)
library(iptools)

Logged = FALSE;

append_log_table <- function()
{
    col_names <- c("date","sessionkey","fingerprint","ip","userhostname","username","passwd")
    sDate <- date()

    # append csv file
    sLogTable <- paste0(sShinyPath,"/reset_log.csv")
    if (!file.exists(sLogTable))
    {
        # write file header
        sLine <- paste0(col_names,collapse=",")
        write(sLine,file=sLogTable)
    }
    sLine <- paste(sDate,sUrlSessionKey,sFingerprint,sUserIP,sUserHostname,sUserName,sPasswd,sep=",")
    write(sLine,file=sLogTable,append=TRUE)
    
    # append data frame
    sLogRdata <- paste0(sShinyPath,"/reset_log.Rdata")
    if (file.exists(sLogRdata))
    {
        load(file=sLogRdata)
        
        arow <- cbind(sDate,sUrlSessionKey,sFingerprint,sUserIP,sUserHostname,sUserName,sPasswd)
        colnames(arow) <- col_names
        reset_df <- rbind(reset_df,arow)
        
    } else {
        arow <- cbind(sDate,sUrlSessionKey,sFingerprint,sUserIP,sUserHostname,sUserName,sPasswd)
        reset_df <- as.data.frame(arow)
        colnames(reset_df) <- col_names
    }
    save(reset_df,file=sLogRdata)
}

shinyServer(function(input, output, session, clientData) {

    observe({
        sUserIP <<- as.character(input$ipid)
        cat(paste0("sUserIP ",sUserIP,"\n"))
    })

    observe({
        sFingerprint <<- as.character(input$fingerprint)
        cat(paste0("sFingerprint ",sFingerprint,"\n"))
    })

    source(paste0(sAppDir,"/authenticate.R"),  local = TRUE)
    values = list()
    setHot = function(x) values[["hot"]] <<- x  

    source(paste0(sShinySourcePath,"/server_pre_marxan.R"),  local = TRUE)

    observe({

        if (USER$Logged == TRUE)
        {
            # render the user interface
            source(paste0(sAppDir,"/render_ui.R"),  local = TRUE)
        } # if
    })
    
    observe({
        iAcceptPwd <<- input$acceptpwd

        cat(paste0("iAcceptPwd ",iAcceptPwd,"\n"))

        if (!is.null(input$acceptpwd))
        if (input$acceptpwd > 0)
        {
        
            pwd1 <- isolate(input$passwd1)
            pwd2 <- isolate(input$passwd2)

            cat(paste0("pwd1 ",pwd1," pwd2 ",pwd2,"\n"))
            
            fValid <- TRUE
            # ensure passwords match
            if (pwd1 != pwd2)
            {
                fValid <- FALSE
            }
            # d41d8cd98f00b204e9800998ecf8427e is blank: don't allow blank passwords
            if ((pwd1 == "d41d8cd98f00b204e9800998ecf8427e") | (pwd2 == "d41d8cd98f00b204e9800998ecf8427e"))
            {
                fValid <- FALSE
            }
            
            if (fValid)
            {        
                sPwdDir <- paste0(sShinyPath,"/passwords")
                dir.create(sPwdDir)
                # 600 read write self only
                #system(paste0("chmod 600 ",sPwdDir))
                sPwdFile <- paste0(sPwdDir,"/passwd_",sUserName,".Rdata")
                # save the users password file
                file.remove(sPwdFile)
                updateNumericInput(session,"showenter",value=0)
                updateNumericInput(session,"showok",value=1)
                passwd <- pwd1
                sPasswd <<- passwd
                save(passwd,file=sPwdFile)
                # 600 read write self only
                #system(paste0("chmod 600 ",sPwdFile))
                
                append_log_table()

            } else {
                updateNumericInput(session,"showhelp",value=1)
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
})
