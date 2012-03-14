
## Pfad setzen
#setwd("C:/Users/ahschulz/Dropbox/R/enron")

## Bibs laden
library(igraph)
library(RMySQL)

con <- dbConnect(MySQL(), host="localhost", user="root", password="domoke74", dbname="enron")

## alles nach R
employeelist <- dbGetQuery(con,
                       "select * from employeelist;")
message <- dbGetQuery(con,
                      "select * from message;")
recipientinfo <- dbGetQuery(con,
                            "select * from recipientinfo;")
referenceinfo <- dbGetQuery(con,
                            "select * from referenceinfo;")

## Datengefummel
employeelist$status <- as.factor(employeelist$status)

message$sender <- as.factor(message$sender)
message$date <- as.Date(message$date)
message$folder <- as.factor(message$folder)

recipientinfo$rtype <- as.factor(recipientinfo$rtype)
recipientinfo$rvalue <- as.factor(recipientinfo$rvalue)
recipientinfo$dater <- NULL

## speichern
save(employeelist, message, recipientinfo, referenceinfo, file = "data/enron_mysqldump.RData")