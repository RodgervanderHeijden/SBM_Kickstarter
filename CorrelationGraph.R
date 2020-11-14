#Make a network graph and save it as pdf

#Compute the correlations between the topics. 
#With $theta you access the number of documents by numer of topics matrix of topic proportions from a model estimated with stm()
cor_matrix <- cor(model$theta) 

#Determine threshold for correlations to be shown, e.g. 0.5. Other values in the correlation matrix are set to 0
cor_matrix[cor_matrix < 0.5] <- 0
diag(cor_matrix) <- 0 #Set the diagonal values from 1 to 0

set.seed(8) #For reproducabilitiy

g <- graph.adjacency(cor_matrix, mode="undirected", weighted=T) #Create an undirected graph with weighted edges
E(g) #Edge sequence of edge IDs
edges <- get.edgelist(g) #Create the adjacency list of the edges
edge_correlations <- rep(NA, nrow(edges)) #Empty placeholder
#Loop through all edge combinations and fill the placeholders with the values
for(i in 1:nrow(edges)){
  edge_correlations[i] <- cor(model$theta)[edges[i,1],edges[i,2]]
}

#Some settings of the edges that you can adjust
E(g)$weight
E(g)$size <- 1
E(g)$lty <- 1
E(g)$color <- "black"

#Use the mean for each topic from the plot.estimateEffect() function
#Use the feature you want to use to color the vertices (e.g. rockstar/flop projects)
#So first run estimateEffect(), then use its output as input for plot.estimateEffect()
effects<- unlist(lapply(estimations$means,function(x){return(x[1])}))
#Set the colors that you want to use. The last number determines the amount of intervals created (currently 5)
colors <- rev(colorRampPalette(c("red", "white", "blue"), bias=1)(5)) 

#Now set the sequence related to the colors
#Sequence is 1 value longer than the value above (6 in case of 5)
#E.g. effect sizes are between -0.3 and 03
sequence <- c(-0.3, -0.2, -0.1, 0.1, 0.2, 0.3)

color_category <- rep(NA,length(effects)) #Placeholder for color of each topic

#Loop through the topics and assign a color based on effect size
for(i in 1:length(color_category)){
  colcat[i] <- max(which(effects[i] > s))
}

#Create the visual
#Replace 20 by your number of topics
plot(effects,1:20,pch=19,cex=2,col=colors[color_category]);abline(v=0)
topicnames <- c() #Assign your topic labels
V(g)$label=topicnames #Label vertices in graph with topic names
topic_proportions <- c() #Assign the mean proportion of each topic
V(g)$size <- topic_proportions*200 #Make size of vertices proportional to topic proportion in corpus, e.g. by factor 200

#Some additional layout settings
vertex.color = colors[color_category]
vertex.label.cex = 1
vertex.label.color = "black"
edge.color = "gray60"
weights <- E(g)$weight

graph_layout <- layout.fruchterman.reingold(g,weight=weights) #Graph layout function
#Save the graph as pdf in current directory
pdf("correlationNetwork.pdf",16,16) 
plot(g, layout=graph_layout,edge.color=edge.color,vertex.color=vertex.color, 
     +vertex.label.cex=vertex.label.cex, vertex.label.color=vertex.label.color,edge.width=edge.width)
dev.off()


