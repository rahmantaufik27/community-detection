#Community detection using R
#call library igraph
> library("igraph")
#import data table nodes dan edges
> nodes <- read.csv(file.choose(), header=T)
> links <- read.csv(file.choose(), header=T)
#check nodes and edges data table
> head(nodes)
> head(links)
#calculate the number of nodes and edges
> nrow(nodes)
> nrow(links)
#generate directed network
> net <- graph_from_data_frame(d=links, directed=T)
#edges in network class
> class(net)
> net
#nodes and edges attributes
> E(net)
> V(net)
#delete unconnected nodes
> V(net)$comp <- components(net)$membership
> main <- induced_subgraph(net,V(net)$comp==1)
#plot network visualisation
> plot(main, edge.size=1, edge.arrow.size=1, vertex.label=NA, 
+ vertex.size=3, margin=c(0.1,0.1), layout=layout_randomly)
#calculate density
> ecount(main)/(vcount(main)*(vcount(main)-1))
#calculate diameter
> diameter(main, directed=T, weights=NA)
#calculate average path length
> mean_distance(main, directed=T)
#calculate clustering coefficient
> transitivity(main, type="global")
#generate degree distribution
> plot( x=0:max(deg.dist), y=0:max(deg.dist), pch=19, cex=1.2, 
+ col="orange", xlab="Degree", ylab="Cumulative Frequency")
#calculate degree centrality
> degree(main, mode="in") 
> centr_degree(main, mode="in", normalized=T)
#calculate closeness centrality
> closeness(main, mode="all", weights=NA)
> centr_clo(main, mode="all", normalized=T)
#calculate eigen centrality
> eigen_centrality(as.undirected(main), directed=T, weights=NA)
> centr_eigen(as.undirected(main), directed=T, normalized=T)
#calculate betweenness centrality
> betweenness(main, directed=T, weights=NA)
> edge_betweenness(main, directed=T, weights=NA)
> centr_betw(main, directed=T, normalized=T)
#Girvan Newman community detection
> ebc <- edge.betweenness.community(as.undirected(main), directed=F)
> plot(main, edge.size=1, edge.arrow.size=1, vertex.label=NA, 
+ vertex.size=3, vertex.color=rainbow(44,alpha=0.6)[ebc$membership])
#Louvain community detection
> clo <- cluster_louvain(main3)
> plot(main3, edge.size=1, edge.arrow.size=1, vertex.label=NA, 
+ vertex.size=3, vertex.color=rainbow(22,alpha=0.6)[clo$membership])
#Clique Percolation community detection
> max <- largest.cliques(main3)
>clique1 <-  max[[1]]
> g3 <- induced.subgraph(graph=main3, vids=clique1)
> plot(g3)

