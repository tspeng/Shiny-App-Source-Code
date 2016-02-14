library(shiny)
shinyUI(pageWithSidebar(
    #Application title
    headerPanel("Linear Model (y~ x1, x2)"),
    
    sidebarPanel(
        checkboxGroupInput("vn","Checkbox",
                            c("x1"="x1",
                             "x2"="x2")),
        sliderInput("level","Confidence Interval Level",value=0.8,min=0.6,max=0.99,step=0.01),
        
        radioButtons("CIplot","Confidence Interval Plot",
                     c("Yes"=1,
                       "No"=0))
    ),
    mainPanel(
        plotOutput("newfig")
    )
))