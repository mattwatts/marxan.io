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

shinyUI(fluidPage(
    # Add custom CSS & Javascript;
    tagList(
        tags$head(
            tags$link(rel="stylesheet", type="text/css",href="style.css"),
            tags$script(type="text/javascript", src = "js/md5.js"),
            tags$script(type="text/javascript", src = "js/passwdInputBinding.js")
        )
    ),

    # displays banner icons and text
    div(class = "banner",
        uiOutput("uiBanner")
    ),

    ## Login module;
    div(class = "login",
        uiOutput("uiLogin"),
        textOutput("pass")
    ),

    div(class = "span4",uiOutput("sidebarui")),
    div(class = "span8", uiOutput("mainui")),

    conditionalPanel(condition = "input.prop == -1",
        inputIp("ipid"),
        inputUserid("fingerprint"),
        numericInput("refreshmap", "Refresh Input", 0),
        numericInput("refreshcluster", "Refresh Input", 0),
        numericInput("refreshtable", "Refresh Input", 0),
        numericInput("displayleaflet","Display leaflet",0),
        numericInput("disableleafletmap","Disable leaflet",0)
    )
))
