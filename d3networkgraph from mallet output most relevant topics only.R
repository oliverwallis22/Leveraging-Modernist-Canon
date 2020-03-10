#make sure composition file name is "composition.txt" and keys file is "keys.txt"
#open RStudio
#setwd to the directory containing composition and keys for example if you are on a mac working on the desktop, remove the pound from the next line
#setwd("~/Desktop/")
#requires igraph and networkD3 if you do not have these libraries remove the pound sign from the next line
#install.packages("igraph", "networkD3")
#click code from drop down menu and select source file and select this file, or in case you want to run the code one line at a time select open file and find this file and then press the run button. This second option is best if you want to learn how the code is working, especially if you are getting an error.
#from here every line beginning with the pound is a comment meant to describe what the following lines of code are meant to do. This is so that you can customize the code in case it is not working or in case you want it to do something else
#reads composition document
comp<-read.table(dir(pattern="*composition.txt")[1], header=F)
#removes first two columns
comp[1:2]<-list(NULL)
#focuses on most prominent topics
topcomp<- comp[, colMeans(comp) > median(colMeans(comp))]
#makes a correlation table of topics to topics
correlation<-cor(topcomp)
#for each topic finds which topic has the highest correlation
maxn<- function(n) function (x) order (x, decreasing = TRUE) [n]
highest<-apply(correlation, 1, maxn(2))
#for each topic finds which topic has the second highest correlation
second<-apply(correlation, 1, maxn(3))
value<-apply(correlation, 1, function(x)x[maxn(3)(x)])
#determines which fo the second highest correlations are more than .25
over<-which(value>.25)
#fixes numbers to match those on correlation
highest<-as.data.frame(highest)
correlation<-as.data.frame(correlation)
highest$highest<-names(correlation[highest$highest])

#writes the correlation table to a file for future reference
write.csv(correlation, file="topcor1.csv")


#builds edges from the second highest correlations over .25
second<-as.data.frame(second)
over<-as.data.frame(over)
second$second<-names(correlation[second$second])
second<-data.frame(to=row.names(second), second)
over<-data.frame(edge=row.names(over), over)
desired<-second[match(over$edge, second$to, nomatch=0),]
names(desired)[2]<-"from"

#sets weight of second highest correlation edges to .5
desired$weight<-rep(0.5,nrow(desired))
#sets weight of highest correlation to 1 and adjusts names of columns to work in igraph
highest<-data.frame(row.names(highest), highest)
names(highest)<-c("to", "from")
highest$weight<-rep(1,nrow(highest))
#combines edges from highest correlations and second highest over .25 to one edge list
edges<-rbind(highest,desired)
#names columns of edgelist for readability in igraph
colnames(edges)[1]<-"to"
colnames(edges)[2]<-"from"
#removes a v to make edges purely numeric and removes decimals added during match
edges[] <- lapply(edges, gsub, pattern = "V", replacement = "", fixed = TRUE)
edges$from<-as.numeric(edges$from)-2
edges$to<-as.numeric(edges$to)-2
edges$from<-trunc(edges$from)
#writes edgelist to a file for reference or to use in a different program
write.csv(edges, file="topedges.csv", row.names=F)
#reads keys
nodes<-read.table(dir(pattern = "*keys.txt")[1], header=F, fill=T)
#adds 1 to topic id number to correspond with Rs counting that begins with 1 instead of Mallet's 0
nodes[,1]<-nodes[,1]+1
#removes columns other than the id number and the most prominent token from each topic
nodes[,4:22]<-list(NULL)
#assigns column names for readability in igraph
colnames(nodes)<-c("id", "size", "label")
#deletes empty topics
nodes<-nodes[!(nodes$label==""), ]
#delete edges to empty topics
edges<-edges[edges$to %in% nodes$id,]
edges<-edges[edges$from %in% nodes$id,]
#loads igraph
library(igraph)
#assures that edge weight is numeric
edges$weight<-as.numeric(edges$weight)
#composes network graph from edges and nodes
net<-graph_from_data_frame(d=edges, vertices=nodes, directed=F)
#sums the weight of identical edges
net<-simplify(net, edge.attr.comb=list(weight="sum", "ignore"))

#deletes isolated nodes
isolates<-which(degree(net) == 0)
net<-delete.vertices(net, isolates)

#calculates node communities based on betweenness
ceb<-cluster_edge_betweenness(net)
#calculates degree centrality of each node
deg <- degree(net, mode="all")
#sets node size to correspond to degree
V(net)$size <- 1+deg*1.5

#plots graph
plot(ceb,net,layout=layout_with_fr,  vertex.label.cex=.5)
#loads networkD3 library
library(networkD3)
#transforms edgelist to work with networdD3
netd3<-igraph_to_networkD3(net)
#adjusts nodes to work with networkD3's numeric scheme
nodes$id<-nodes$id-1
#composes D3 graph
netD3b<-forceNetwork(Links=netd3$links, Nodes=nodes, Source='source', Target='target', NodeID='label', Group='label', fontSize=12, opacity=1, zoom=T, opacityNoHover=.3)
#writes network to file
saveNetwork(netD3b, file="~/Desktop/d3network.html")