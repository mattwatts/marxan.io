# marxan.io

shinyUI(bootstrapPage(
  # Add custom CSS & Javascript;
  tagList(
    tags$head(
      tags$link(rel="stylesheet", type="text/css",href="style.css"),
      tags$script(type="text/javascript", src = "md5.js"),
      tags$script(type="text/javascript", src = "passwdInputBinding.js")
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
                   numericInput("refreshinput", "Refresh Input", 0))
))
