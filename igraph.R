## loading libraries
library(igraph)
library(RColorBrewer)
library(gplots)

## load data
load("data/enron.RData")

## color positions
vertex_col <- brewer.pal(length(unique(V(network)$status)), "RdYlBu")
V(network)$color <- vertex_col[factor(V(network)$status)]

## mails, but at least 20 send messages
n_all <- delete.edges(network, E(network)[E(network)$count < 14])
#n_all <- network

V(n_all)$outdegree <- degree(n_all, mode = "out")
V(n_all)$indegree <- degree(n_all, mode = "in")
V(n_all)$degree <- degree(n_all, mode = "all")

## TO-Mails
n_to <- delete.edges(n_all, E(n_all)[E(n_all)$type == "CC" | E(n_all)$type == "BCC"])

V(n_to)$outdegree <- degree(n_to, mode = "out")
V(n_to)$indegree <- degree(n_to, mode = "in")
V(n_to)$degree <- degree(n_to, mode = "all")


## CC-Mails
n_cc <- delete.edges(n_all, E(n_all)[E(n_all)$type == "TO" | E(n_all)$type == "BCC"])

V(n_cc)$outdegree <- degree(n_cc, mode = "out")
V(n_cc)$indegree <- degree(n_cc, mode = "in")
V(n_cc)$degree <- degree(n_cc, mode = "all")


## BCC-Mails
n_bcc <- delete.edges(n_all, E(n_all)[E(n_all)$type == "TO" | E(n_all)$type == "CC"])

V(n_bcc)$outdegree <- degree(n_bcc, mode = "out")
V(n_bcc)$indegree <- degree(n_bcc, mode = "in")
V(n_bcc)$degree <- degree(n_bcc, mode = "all")



### visualizing ###

## all plots as single ones

set.seed(13)

netze <- c("n_all", "n_to", "n_cc", "n_bcc")
main <- c("All emails", "TO email", "CC email", "BCC email")
count <- c(1)

for (i in netze) {
  temp <- get(i)

  edge_col <- colorpanel(length(table(E(temp)$count)), low = "#2C7BB6", high = "#FFFFBF")  
  E(temp)$color <- edge_col[factor(E(temp)$count)]
  
  #pdf(file = paste("plot_",i,".pdf", sep = ""), width = 5, height = 5)
  #png(file = paste("plot_",i,".png", sep = ""), width = 480, height = 480, type = "cairo")
  jpeg(file = paste("plot_",i,".jpg", sep = ""), width = 480, height = 480, type = "cairo", quality = 95)
  par(bg = "grey60", mar = c(0,0.25,1.5,0))
  layout(matrix(c(1,2), 2, 1, byrow = T), height = c(4, 1))
  plot(temp, main = main[count], layout = layout.fruchterman.reingold(temp, params= list(niter = 1000, area = vcount(temp)^4, weights = E(temp)$count), repulserad= vcount(temp)^3),#(temp, weights = E(temp)$count),#, area = vcount(temp)^4),
       vertex.label = NA, vertex.size = 3+3*log10(V(temp)$degree), 
       edge.arrow.size = E(temp)$count/150, edge.width = 1.5*log10(E(temp)$count), edge.curved = T, edge.color = E(temp)$color       )
  
  plot.new()
  legend("bottom", legend = unique(V(network)$status), col = vertex_col, pch = 19,cex = 1, pt.cex= 1.25, ncol = 4, box.lwd = 0, box.col = "grey60")
  
  dev.off()
  
  count <- count + 1
}


## all plots in one graphic

set.seed(13)

netze <- c("n_all", "n_to", "n_cc", "n_bcc")
main <- c("All emails", "TO emails", "CC emails", "BCC emails")
count <- c(1)

cairo_pdf(file = "all.pdf", width = 10, height = 10,pointsize=14)
#png(file = "all.png", width = 1200, height = 1200)
par(bg = "grey60", mar = c(1,1,1,1), oma= c(1,1,1,1))
layout(matrix(c(1,2,3,4,5,5), 3, 2, byrow = T), height = c(4,4,1))

for (i in netze) {
  temp <- get(i)
  
  edge_col <- colorpanel(length(table(E(temp)$count)), low = "#2C7BB6", high = "#FFFFBF")  
  E(temp)$color <- edge_col[factor(E(temp)$count)]
  
  
  plot(temp, main = main[count], layout = layout.fruchterman.reingold(temp, params= list(niter = 1000, area = vcount(temp)^4, weights = E(temp)$count), repulserad= vcount(temp)^3),
       vertex.label = NA, vertex.size = 3+3*log10(V(temp)$degree),
       edge.arrow.size = E(temp)$count/150, edge.width = 1.25*log10(E(temp)$count), edge.color = E(temp)$color, edge.curved = T)
  
}

plot.new()
legend("bottom", legend = unique(V(network)$status), col = vertex_col, pch = 19,cex = 1.5, pt.cex= 2, ncol = 4)

dev.off()


## save networks
save(n_all, n_to, n_cc, n_bcc, file= "data/networks.RData")
