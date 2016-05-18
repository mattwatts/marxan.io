output$mainui <- renderUI({
    mainPanel(
        plotOutput("usageplot",width=200,height=200)
    )
})
