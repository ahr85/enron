## Verzeichnis setzen
setwd("C:/Users/ahschulz/Dropbox/R/enron")

## Library laden
library(igraph)
#library(RColorBrewer)
library(gplots)

## Laden
load("data/enron.RData")

## alle Mails, aber mindestens 20 Mails geschickt
n_all <- delete.edges(network, E(network)[E(network)$count < 14])

outdegree <- degree(n_all, mode = "out")
V(n_all)$outdegree <- scale(outdegree, min(outdegree), max(outdegree)-min(outdegree))*10+1
indegree <- degree(n_all, mode = "in")
V(n_all)$indegree <- scale(indegree, min(indegree), max(indegree)-min(indegree))*10+1
degree <- degree(n_all, mode = "all")
V(n_all)$degree <- scale(degree, min(degree), max(degree)-min(degree))*10+1

## TO-Mails
n_to <- delete.edges(n_all, E(n_all)[E(n_all)$type == "CC" | E(n_all)$type == "BCC"])

outdegree <- degree(n_to, mode = "out")
V(n_to)$outdegree <- scale(outdegree, min(outdegree), max(outdegree)-min(outdegree))*10+1
indegree <- degree(n_to, mode = "in")
V(n_to)$indegree <- scale(indegree, min(indegree), max(indegree)-min(indegree))*10+1
degree <- degree(n_to, mode = "all")
V(n_to)$degree <- scale(degree, min(degree), max(degree)-min(degree))*10+1


## CC-Mails
n_cc <- delete.edges(n_all, E(n_all)[E(n_all)$type == "TO" | E(n_all)$type == "BCC"])

outdegree <- degree(n_cc, mode = "out")
V(n_cc)$outdegree <- scale(outdegree, min(outdegree), max(outdegree)-min(outdegree))*10+1
indegree <- degree(n_cc, mode = "in")
V(n_cc)$indegree <- scale(indegree, min(indegree), max(indegree)-min(indegree))*10+1
degree <- degree(n_cc, mode = "all")
V(n_cc)$degree <- scale(degree, min(degree), max(degree)-min(degree))*10+1


## BCC-Mails
n_bcc <- delete.edges(n_all, E(n_all)[E(n_all)$type == "TO" | E(n_all)$type == "CC"])

outdegree <- degree(n_bcc, mode = "out")
V(n_bcc)$outdegree <- scale(outdegree, min(outdegree), max(outdegree)-min(outdegree))*10+1
indegree <- degree(n_bcc, mode = "in")
V(n_bcc)$indegree <- scale(indegree, min(indegree), max(indegree)-min(indegree))*10+1
degree <- degree(n_bcc, mode = "all")
V(n_bcc)$degree <- scale(degree, min(degree), max(degree)-min(degree))*10+1




### visusalisieren ###

## alle Plots einzeln

set.seed(13)

netze <- c("n_all", "n_to", "n_cc", "n_bcc")
main <- c("all email", "TO email", "CC email", "BCC email")
count <- c(1)

for (i in netze) {
  temp <- get(i)

  temp2 <- data.frame(table(E(temp)$count),colorpanel(length(table(E(temp)$count)), low = "#2C7BB6", high = "#FFFFBF"))
  #temp2 <- data.frame(table(E(temp)$count),colorpanel(length(table(E(temp)$count)), low = "#2C7BB6", mid = "#FFFFBF", high = "#D7191C"))
  names(temp2) <- c("count", "var", "color")
  
  temp3 <- data.frame(E(temp)$count, seq(1:length(E(temp))))
  names(temp3) <- c("count", "id")
  
  temp4 <- merge(temp2, temp3)
  temp4 <- temp4[order(temp4$id),]
  
  E(temp)$color <- as.character(temp4$color)
  
  pdf(file = paste("plot_",i,".pdf", sep = ""), width = 5, height = 5)
  png(file = paste("plot_",i,".png", sep = ""), width = 480, height = 480)
  par(bg = "grey60", mar = c(0,0,2,0))
  plot(temp, main = main[count], layout = layout.fruchterman.reingold(temp, weights = E(temp)$count, area = vcount(temp)^4),
       vertex.label = NA, vertex.size = V(temp)$degree, vertex.color = rainbow(10),
       edge.arrow.size = E(temp)$count/150, edge.width = E(temp)$count/75, edge.color = E(temp)$color, edge.curved = T,
       )
  dev.off()
  dev.off()
  
  count <- count + 1
}


## alle Plots in einer Grafik

set.seed(13)

netze <- c("n_all", "n_to", "n_cc", "n_bcc")
main <- c("all email", "TO email", "CC email", "BCC email")
count <- c(1)

pdf(file = "alle.pdf", width = 10, height = 10)
#png(file = "alle.png", width = 1200, height = 1200)
par(bg = "grey60", mar = c(0,0,2,0), mfrow = c(2, 2))


for (i in netze) {
  temp <- get(i)
  
  temp2 <- data.frame(table(E(temp)$count),colorpanel(length(table(E(temp)$count)), low = "#2C7BB6", high = "#FFFFBF"))
  #temp2 <- data.frame(table(E(temp)$count),colorpanel(length(table(E(temp)$count)), low = "#2C7BB6", mid = "#FFFFBF", high = "#D7191C"))
  names(temp2) <- c("count", "var", "color")
  
  temp3 <- data.frame(E(temp)$count, seq(1:length(E(temp))))
  names(temp3) <- c("count", "id")
  
  temp4 <- merge(temp2, temp3)
  temp4 <- temp4[order(temp4$id),]
  
  E(temp)$color <- as.character(temp4$color)
  
  plot(temp, main = main[count], layout = layout.fruchterman.reingold(temp, weights = E(temp)$count, area = vcount(temp)^4),
       vertex.label = NA, vertex.size = V(temp)$degree, vertex.color = rainbow(10),
       edge.arrow.size = E(temp)$count/150, edge.width = E(temp)$count/75, edge.color = E(temp)$color, edge.curved = T)
  
  count <- count + 1
}

#dev.off()
dev.off()

legend(bottom, legend = c("CEO", "Boss"))