# marxan.io

library(shiny)
library(shinyBS)
library(iptools)
library(NCmisc)

shinyServer(function(input, output, session, clientData) {

    source(paste0(sShinySourcePath,"/server_pre_marxan.R"),  local = TRUE)

    sLogFile <<- CreateLogFile(sShinyTempPath,"anonymous","usage")
    AppendLogFile(sLogFile,"usage app start")

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
        sHoseName <<- strsplit(session$clientData$url_hostname,as.character("."),fixed=TRUE)[[1]][1]
        #sHoseName <<- session$clientData$url_hostname
    })
    
    autoInvalidate <- reactiveTimer(2000,session=session)

    output$usageplot <- renderPlot({
    
        autoInvalidate()
    
        if (.Platform$pkgType == "source")
        {
            x <- top()
            rCPU <- x$CPU$total # % CPU used
            rRAM <- (x$RAM$used*1000)/x$RAM$total*100 # % RAM used
            
            y <- system("df /mnt",intern=TRUE,ignore.stdout = FALSE,ignore.stderr=TRUE)
            z <- strsplit(y[2],split=" ")[[1]]
            for (i in 1:length(z))
            {
              if (length(grep("%",z[i]))>0)
              {
                percent <<- z[i]
                cat(z[i])
              }
            }
            rDISK <- as.numeric(strsplit(percent,split="%")[[1]])
            
        } else {
            rCPU <- runif(1,0,100)
            rRAM <- runif(1,0,100)
            rDISK <- runif(1,0,100)
        }
        
        usage_df <- c(cpu=rCPU,ram=rRAM,disk=rDISK)
        barplot(usage_df,ylim=c(0,100),main=sHoseName)
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
