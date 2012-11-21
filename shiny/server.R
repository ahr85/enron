library(shiny)
library(igraph)
library(gplots)

# load data
load("data/networks.RData")

shinyServer(
  function(input, output) {
    
    #switch chosen data
    dataset <- reactive(function() {
      switch(input$chosen_emails,
        "all emails" = n_all,
        "direct emails" = n_to,
        "CC emails" = n_cc,
        "BCC emails" = n_bcc)    
    })
    
    #degrees <- reactive(function() {
    #  
    #})
    
    # generate chosen plot
    output$network <- reactivePlot(function() {
      data <- dataset()
      
      # color the edges (see igraph.R)
      edge_col <- colorpanel(length(table(E(data)$count)), low = "#2C7BB6", high = "#FFFFBF")  
      E(data)$color <- edge_col[factor(E(data)$count)]
      
      # all edge_weight above 500 cut down to 500
      #E(data)$count[E(data)$count > 500] <- 500
      
      # select only nodes with given degrees and edge weights
      data <- delete.vertices(data, V(data)[V(data)$degree < input$degree[1] | V(data)$degree > input$degree[2]])
      data <- delete.edges(data, E(data)[E(data)$count < input$edge_weight[1] | E(data)$count > input$edge_weight[2]])
      
      # let's make a picture ;-)
      par(bg = "#F5F5F5", mar = c(1,1,1,1), oma= c(1,1,1,1))
      plot(data, main = input$chosen_emails, layout = layout.fruchterman.reingold(data, params= list(niter = 1000, area = vcount(data)^4, weights = E(data)$count), repulserad= vcount(data)^3),
        vertex.label = NA, vertex.size = 3+3*log10(V(data)$degree), 
        edge.arrow.size = E(data)$count/150, edge.width = 1.5*log10(E(data)$count), edge.curved = T, edge.color = E(data)$color)
      
    })
    
    output$communities <- reactivePlot(function() {
      data <- dataset()
      
      #E(data)$count[E(data)$count > 500] <- 500
      
      # select only nodes with given degrees and edge weights
      data <- delete.vertices(data, V(data)[V(data)$degree < input$degree[1] | V(data)$degree > input$degree[2]])
      data <- delete.edges(data, E(data)[E(data)$count < input$edge_weight[1] | E(data)$count > input$edge_weight[2]])
      
      # Community detection
      com <- edge.betweenness.community(data, E(data)$counts)
      
      par(bg = "#F5F5F5", mar = c(1,1,1,1), oma= c(1,1,1,1))
      plot(com, data, main = "Communities")
    })
  }  
)