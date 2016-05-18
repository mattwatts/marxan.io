# marxan.io

library(shiny)
library(shinyBS)
library(countrycode)
library(iptools)
library(mailR)

load(file=paste0(sShinyPath,"/accountGmail.Rdata"))

industry_list <- c("Academic","Bilateral agency","Community group","Consultancy","Indigenous group","Industry","Inter-government organisation","Local government","Multilateral agency","National government","NGO - International","NGO - National","NGO - Regional","Private sector","Research","State Government")

research_interest_list <- c("Agriculture and rural development","Agronomy","Aquaculture","Aquaculture and fisheries","Architecture","Arid Regions","Art Awareness","Arts Festival","Biologica and ecological research","Biological Research","Biomass","Blog site","Cetaceans","Clean oceans"," beaches and inland waters","coastal ecosystem restoration","Coastal zone management","Community conservation and world hertiage sites","Community participation ","conservation and NRM","Construction","Coral reefs","Custom programming"," database and GIS software","Desert Ecosystem Northern Mexico","Development","Development and business sector","Dispute resolution service","Drylands","Durban","Ecological","Ecological Management"," Forestry","Ecosystem-based conservation planning","Electricity","Energy","Energy and environment","Engineering","Engineering ","Engineering and architecture","Engineering and construction","Engineering and Ecology","Engineering and environment","Entomology","Environment and Water","Environmental","Environmental and chemical engineering","Environmental and social issues","Environmental campaigns","Environmental Education","Environmental management","Environmental mitigation","Environmental Science and spatial planning","Environmental Services","Environmental"," health and safety services","EU research centre","Fish friendly farming practices","Fisheries","Fisheries and bio-monitoring","Fisheries Science","Forest industry","Forestry","Forestry and Ecology","Forests","Freshwater and Marine Biology and Ecology","Geological and Nuclear Sciences","Geospatial and information technology","GIS","GIS and ecology","GIS and mapping","GIS data provider","GIS services","GIS software","Government","Grassroots capacity","Host national and international seminars","Humanitarian aid","GIS and Image Processing Software","Indigenous rights","Institute for agricultural sciences","Integrated circuit design centers","International issues of agriculture and development","IT services","IT services to environment sector","Land and water","landscape and sustainabilty","Law and legal services","Legal Services","Lumber and newsprint","Lumber and paper pulp","Management","Management and Accounting","Mapping","Mapping and GIS","Mapping social values","Mappiong and landscape analysis","Marine","Marine acoustic imaging","Marine and coasts","Marine and Seafood","Marine fisheries","marine fisheries research"," mariculture and marine fishery resource.","Marine mammals","Marine planning","Marine Research","Marine surveys and mapping","Market-based mechanisms","Marketing and Design","Media Design","Mining","Mobile Applications","Natural resource planning and management","Non-government","Non-profit; sustainable development","Ocean and coastal marine work","Ocean conservation advocacy","Oceanography","Oceans","Oilsands","Opensource technologies","Planning Urban Design Environment Mgmt","Ponds","Prawn farming","Public Administration through ICT technologies","Renewable Energy","Renewable Energy Solutions","Resource management","Riparian science","Rock Fishery","Rural development","Science and technology","Sciences including agronomy and env sciences","Seabed mapping","Seafood","Silvaculture","Software","Software Development","Solar lighting","Stakeholder negotiation with land use","Stream assessment","Sustainable and rural development","Sustainable development","Sustainable development of agriculture","Sustainable Ecosystems","Sustainable management of western US wildlands","Technology","Technology sector","Telecommunications","Thermal power solutions","Urban design Environment","Urban development sector","Volunteer marine conservation expeditions","Waste treatment and env mgmt","Water and Sewage Authority","Water","Environment and health","Wetlands","Wetlands and Endangered Species","Wildlife management","Wildlife research","Wind farm development")

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
    col_names <- c("date","sessionkey","fingerprint","ip","userhostname","name","organisation","country","email","subscribe","registerio","industry","research interest")
    sDate <- date()

    # append csv file
    sLogTable <- paste0(sShinyPath,"/download_log.csv")
    if (!file.exists(sLogTable))
    {
        # write file header
        sLine <- paste0(col_names,collapse=",")
        write(sLine,file=sLogTable)
    }
    sLine <- paste(sDate,sUserSessionKey,sFingerprint,sUserIP,sUserHostname,sName,sOrganisation,
                   sCountry,sEmail,sSubscribe,sRegisterIO,sIndustry,sResearchInterest,sep=",")
    write(sLine,file=sLogTable,append=TRUE)
    
    # append data frame
    sLogRdata <- paste0(sShinyPath,"/download_log.Rdata")
    if (file.exists(sLogRdata))
    {
        load(file=sLogRdata)
        
        arow <- cbind(sDate,sUserSessionKey,sFingerprint,sUserIP,sUserHostname,sName,sOrganisation,
                      sCountry,sEmail,sSubscribe,sRegisterIO,sIndustry,sResearchInterest)
        colnames(arow) <- col_names
        register_df <- rbind(register_df,arow)
        
    } else {
        arow <- cbind(sDate,sUserSessionKey,sFingerprint,sUserIP,sUserHostname,sName,sOrganisation,
                      sCountry,sEmail,sSubscribe,sRegisterIO,sIndustry,sResearchInterest)
        register_df <- as.data.frame(arow)
        colnames(register_df) <- col_names
    }
    save(register_df,file=sLogRdata)
}

shinyServer(function(input, output, session, clientData) {

    source(paste0(sShinySourcePath,"/server_pre_marxan.R"),  local = TRUE)

    sLogFile <<- CreateLogFile(sShinyTempPath,"anonymous","register")
    AppendLogFile(sLogFile,"register app start")

    observe({
    
        if (!is.null(input$GetScreenWidth))
        {
            if (iWidthChange == 0)
            {
                iWidth <<- input$GetScreenWidth
                cat(paste0("screen width ",iWidth,"\n"))

                iWidthChange <<- 1
            }
        }
    })

    generate_screen_width <- reactive({

        input$GetScreenWidth
        cat(paste0("generate_screen_width iWidth ",iWidth,"\n"))
        return(iWidth)
    })
    
    generate_registerio_msg <- reactive({

        #input$registerio
        input$submitUser

        if (fRegisterIO)
        {
            sMsg <- "A password reset email has been sent to you. The password reset link will expire in 12 hours."
        } else {
            sMsg <- ""
        }

        cat(paste0("generate_registerio_msg sMsg >",sMsg,"<\n"))
        return(sMsg)
    })

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

    output$registertable <- renderTable({

        if (!is.null(input$processdownload))
        {
        
            input$processdownload

            the_table <- as.data.frame(rbind(c("name",sName),
                                         c("organisation",sOrganisation),
                                         c("country",sCountry),
                                         c("email",sEmail),
                                         c("subscribe",sSubscribe),
                                         c("register marxan.io",sRegisterIO),
                                         c("industry",sIndustry),
                                         c("research interest",sResearchInterest)))
            colnames(the_table) <- c("field","value")

            the_table
        }
    })

    observe({
        if (!is.null(input$registerio))
        {
            fRegisterIO <<- input$registerio

            if (fRegisterIO)
            {
                updateNumericInput(session, "showregisterio", value = 1)
            } else {
                updateNumericInput(session, "showregisterio", value = 0)
            }
        }
        sRegisterIO <<- as.character(fRegisterIO)
    })

    observe({
        fInformAuthors <<- input$informauthors
    })

    observe({
        fAcknowledgeIP <<- input$acknowledgeip
    })

    observe({
        sName <<- input$name
    })

    observe({
        sOrganisation <<- input$organisation
    })

    observe({
        sEmail <<- input$email
    })

    observe({
        sCountry <<- input$country
    })

    observe({
        sIndustry <<- input$industry
    })

    observe({
        sResearchInterest <<- input$researchinterest
    })

    observe({
        sSubscribe <<- as.character(input$subscribe)
    })
    
    observe({
        iRegisterAnotherUser <- input$registerAnotherUser

        cat(paste0("iRegisterAnotherUser ",iRegisterAnotherUser,"\n"))

        if (!is.null(input$registerAnotherUser))
        if (input$registerAnotherUser > 0)
        {
            # reset the user interface controls so another user can register
            updateTextInput(session,"name",value="")
            updateTextInput(session,"organisation",value="")
            updateTextInput(session,"email",value="")
            updateSelectInput(session,"country",selected="Australia")
            updateCheckboxInput(session,"subscribe",value=FALSE)
            updateCheckboxInput(session,"registerio",value=FALSE)
            updateCheckboxInput(session,"informauthors",value=FALSE)
            updateCheckboxInput(session,"acknowledgeip",value=FALSE)
            
            updateNumericInput(session, "showagree", value = 0)
            updateNumericInput(session, "showhint", value = 0)
            updateNumericInput(session, "showok", value = 0)
            updateNumericInput(session, "showform", value = 1)
            
            sLogFile <<- CreateLogFile(sShinyTempPath,"anonymous","register")
            AppendLogFile(sLogFile,"register app registerAnotherUser")
        }
    })

    observe({
        iSubmitUser <- input$submitUser

        cat(paste0("iSubmitUser ",iSubmitUser,"\n"))

        if (!is.null(input$submitUser))
        if (input$submitUser > 0)
        {
            # validate info entered by user
            fValid <- TRUE
            fShowAgree <- FALSE
            fShowHint <- FALSE

            if (!fInformAuthors) { fShowAgree <- TRUE }
            if (!fAcknowledgeIP) { fShowAgree <- TRUE }
            
            if (sName == "")         { fShowHint <- TRUE }
            if (sOrganisation == "") { fShowHint <- TRUE }
            if (sEmail == "")        { fShowHint <- TRUE }

            if (fShowAgree)
            {
                updateNumericInput(session, "showagree", value = 1)
                fValid <- FALSE
            } else {
                updateNumericInput(session, "showagree", value = 0)
            }

            if (fShowHint)
            {
                updateNumericInput(session, "showhint", value = 1)
                fValid <- FALSE
            } else {
                updateNumericInput(session, "showhint", value = 0)
            }

            # show messages if invalid
            if (fValid)
            {
                # accept and display table/message because valid response
                updateNumericInput(session, "showform", value = 0)
                updateNumericInput(session, "showok", value = 1)
                
                AppendLogFile(sLogFile,"registration accepted")

                # register the user
                #
                {
                    # send them the password reset and notification email
                    sUserName <<- sEmail
                    if (fRegisterIO) { InitialiseUserSession() }

                    iprocessdownload <<- iprocessdownload + 1
                    updateNumericInput(session, "processdownload", value = iprocessdownload)

                    # create the password file so it exists for reset_pwd
                    dir.create(paste0(sShinyPath,"/passwords"))
                    sPwdFile <- paste0(sShinyPath,"/passwords/passwd_",sUserName,".Rdata")
                    writeLines("",con=sPwdFile)
                }
            }
        }
    })
    
    process_user_download <- function()
    {
        sSubject <<- paste0("marxan.io register user ",sName)
        sMessage <<- paste0("Hi ",sName,"\n\n",
                           "Thankyou for registering for the marxan.io service.\n\n",
                           "Your username is the email address you registered with: ",sEmail,"\n\n",
                           "Click here to reset the password for your new account:\n\n",
                           "http://",sAppServer,"/rshiny/apps/",sPwdResetApp,"/?session=",sUserSessionKey,
                           "\n\n",
                           "The password reset link will expire in 12 hours. If you have any problems please email m.watts@uq.edu.au for support.\n\n",
                           "fingerprint: ",sFingerprint,"\n",
                           "ip: ",sUserIP,"\n",
                           "userhostname: ",sUserHostname,"\n",
                           "name: ",sName,"\n",
                           "organisation: ",sOrganisation,"\n",
                           "country: ",sCountry,"\n",
                           "email: ",sEmail,"\n",
                           "subscribe: ",sSubscribe,"\n",
                           "register marxan.io: ",sRegisterIO,"\n",
                           "industry: ",sIndustry,"\n",
                           "research interest: ",sResearchInterest,"\n")

        AppendLogFile(sLogFile,sSubject)
        AppendLogFile(sLogFile,sMessage)
        append_log_table()

        # send email to the admin
        send_email("m.watts@uq.edu.au",paste0("software download user ",sName),sMessage)

        if (fRegisterIO)
        {
            # send email to the user
            send_email(sEmail,sSubject,sMessage)
        }
    }

    observe({
        if (!is.null(input$processdownload))
        {
            cat(paste0("input$processdownload ",input$processdownload,"\n"))
            if (input$processdownload > 0)
            {
                process_user_download()
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
