## libraries
library(igraph)
library(RMySQL)

con <- dbConnect(MySQL(), host="localhost", user="root", password="domoke74", dbname="enron")

## get emails
edges <- dbGetQuery(con,
  "select *, count(*) as count from
    (select sender, rvalue as reciever, rtype as type, date from message as a
      left join recipientinfo as b on a.mid = b.mid
      where sender in (select Email_id from employeelist) and rvalue in (select Email_id from employeelist) and sender != rvalue) as x
    group by sender, reciever, type;")

nodes <- dbGetQuery(con,
                    "select Email_id, lastName, status from employeelist;")

## data management
edges$sender <- as.factor(edges$sender)
edges$reciever <- as.factor(edges$reciever)
edges$type <- as.factor(edges$type)
edges$date <- as.Date(edges$date)
edges$count <- as.numeric(edges$count)

nodes$status[is.na(nodes$status)] <- "N/A"

## make network
network <- graph.data.frame(edges, directed = T, vertices = nodes)

## save
write.graph(network, file = "data/enron.graphML", format = "graphml")
save(edges, nodes, network, file = "data/enron.RData")