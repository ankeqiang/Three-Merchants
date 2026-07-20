
library(histtext)
library(lubridate)
library(ggplot2)
library(tidygraph)
library(igraph)
library(tidyverse)
library(tidytext)

# save objects in .RData file
save.image('zbsnetbio.RData')


# Re-upload saved RData file
load(file = "zbsnetbio.RData")

# Create graph from data frames
g <- graph_from_data_frame(d=zbs_Book_Edges, vertices=zbs_Book_Nodes, directed=FALSE)



# Ensure all columns are of type character
zbs_Book_Nodes[] <- lapply(zbs_Book_Nodes, as.character)
zbs_Book_Edges[] <- lapply(zbs_Book_Edges, as.character)

# Identify nodes that are in the edge list but not in the node list
edge_nodes <- unique(c(zbs_Book_Edges$Source, zbs_Book_Edges$Target))
missing_nodes <- setdiff(edge_nodes, zbs_Book_Nodes$Nodes)

# Print missing nodes
print(missing_nodes)

# Add missing nodes to the node list with a default type (e.g., 'unknown')
if(length(missing_nodes) > 0) {
  missing_nodes_df <- data.frame(Nodes = missing_nodes, Type = "unknown")
  zbs_Book_Nodes <- rbind(zbs_Book_Nodes, missing_nodes_df)
}

# Create the graph
g <- graph_from_data_frame(d=zbs_Book_Edges, vertices=zbs_Book_Nodes, directed=FALSE)

# Set colors for node types
V(g)$color <- ifelse(V(g)$Type == "individual", "skyblue", "lightgreen")

# Plot the graph
plot(g, vertex.size=15, vertex.label.cex=0.8, edge.width=1, edge.color="gray",
     vertex.label.color="black", vertex.label.family="Arial", main="Combined Network: Individuals and Organizations")

# Improved version (visually)


# Create the graph
g <- graph_from_data_frame(d=zbs_Book_Edges, vertices=zbs_Book_Nodes, directed=FALSE)

# Set colors for node types
V(g)$color <- ifelse(V(g)$Type == "individual", "skyblue", "lightgreen")

# Set node size based on degree
V(g)$size <- degree(g) * 2
V(g)$size[V(g)$name == "Zhu Baosan"] <- 10  # Set Zhu Baosan's size manually to 10

# Define a color mapping for edge relationships
relation_colors <- c("family" = "red", "marriage" = "pink", "association" = "blue", 
                     "employment" = "green", "service" = "purple", "membership" = "cyan",
                     "event" = "orange", "friend" = "yellow", "collaborator" = "darkgreen", 
                     "employee" = "darkblue", "associate" = "lightblue", "dispute" = "black",
                     "manager" = "brown", "business partner" = "gold", "founder" = "darkred",
                     "investor" = "lightgreen", "comprador" = "darkpurple", "documenter" = "gray",
                     "supported" = "lightgray", "advocacy" = "lightpink", "negotiation" = "lightyellow",
                     "leader" = "darkcyan", "member" = "lightpurple", "predecessor" = "lightorange",
                     "successor" = "lightblue", "vice-president" = "lightgreen", "finance minister" = "lightred")

# Assign colors to edges based on relationship
E(g)$color <- relation_colors[zbs_Book_Edges$Relationship]

# Use a different layout
layout <- layout_with_fr(g)

# Increase the size of the plotting area
plot(g, layout=layout, vertex.size=V(g)$size, vertex.label.cex=0.7, edge.width=0.8, edge.color="gray",
     vertex.label.color="black", vertex.label.family="Arial", main="Combined Network: Individuals and Organizations")

# Save the improved plot to a file
png("path/to/improved_combined_network.png", width=2000, height=2000)
plot(g, layout=layout, vertex.size=V(g)$size, vertex.label.cex=0.7, edge.width=0.5, edge.color="gray",
     vertex.label.color="black", vertex.label.family="Arial", main="Combined Network: Individuals and Organizations")
dev.off()

