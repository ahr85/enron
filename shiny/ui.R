library(shiny)

shinyUI(
  pageWithSidebar(
  
    headerPanel("Enron plots"),
    
    # choose which mails should be displayed
    sidebarPanel(
      selectInput("chosen_emails", h4("Select network"),
                  choices = c("all emails", "direct emails", "CC emails", "BCC emails")),
      h4("Change details"),
      
      # all emails
      sliderInput("degree", "Shown nodes by degree (in & out):",
                    min = 0, max = 66, value = c(0,66), step = 2),
      sliderInput("edge_weight", "Shown edge weights:",
                    min = 14, max = 500, value = c(14, 500)),
      helpText("All edge weights (sent emails between two persons) below 14 were removed to provide more clarity."),
      
      # Legend
      h4("Legend"),
      img(src = "legend.png")
      
    ),
    
    # output
    mainPanel(
      plotOutput("network"),
      plotOutput("communities")
    )
    
  )
)