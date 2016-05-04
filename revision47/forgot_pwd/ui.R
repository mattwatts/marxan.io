# marxan.io

inputUserid <- function(inputId, value='')
{
    tagList(
        singleton(tags$head(tags$script(src = "js/md5.js", type='text/javascript'))),
        singleton(tags$head(tags$script(src = "js/shinyBindings.js", type='text/javascript'))),
        tags$body(onload="setvalues()"),
        tags$input(id = inputId, class = "userid", value=as.character(value), type="text", style="display:none;")
    )
}

inputIp <- function(inputId, value='')
{
    tagList(
        singleton(tags$head(tags$script(src = "js/md5.js", type='text/javascript'))),
        singleton(tags$head(tags$script(src = "js/shinyBindings.js", type='text/javascript'))),
        tags$body(onload="setvalues()"),
        tags$input(id = inputId, class = "ipaddr", value=as.character(value), type="text", style="display:none;")
    )
}

shinyUI(bootstrapPage(

    uiOutput("mainui"),

    conditionalPanel(condition = "input.prop == -1",
        inputIp("ipid"),
        inputUserid("fingerprint"),
        numericInput("refreshtable", "Refresh Input", 0),
        numericInput("showform","show form",1),
        numericInput("showhint","show hint",0),
        numericInput("showok","show ok",0),
        numericInput("showinvalid","show invalid",0),
        numericInput("sendemail","send email",0)
    )
))

