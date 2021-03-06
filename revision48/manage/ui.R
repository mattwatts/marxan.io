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

    tags$script('document.title = "Manage datasets";'),

    uiOutput("sidebarui"),
    uiOutput("mainui"),

    conditionalPanel(condition = "input.prop == -1",
        inputIp("ipid"),
        inputUserid("fingerprint"),
        numericInput("refreshtable", "Refresh Input", 0),
        numericInput("areyousure","Are you sure?",0),
        numericInput("copypublicdata","Copy public data",0),
        numericInput("copyprivatedata","Copy private data",0),
        numericInput("renamemydata","Rename my data",0),
        numericInput("sharemydata","Share my data",1),
        numericInput("sharemydatapublic","Share my data public",0),
        numericInput("sharemydataremove","Share my data remove",0),
        numericInput("sharemydataprivate","Share my data private",0),
        numericInput("sharemydataedit","Share my data edit",0)
    )
))
