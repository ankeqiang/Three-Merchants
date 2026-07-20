# First we load the necessary packages 

library(histtext)
library(lubridate)
library(ggplot2)
library(tidygraph)
library(igraph)
library(tidyverse)
library(tidytext)

# list all available corpora 
histtext::list_corpora()

# save objects in .RData file
save.image('zbsdfzz.RData')


# Re-upload saved RData file
load(file = "zbsdfzz.RData")


# Search ZBS in the corpus, trying various possible spellings, using the function search_documents() : 
zbs_dfzzall <- search_documents('"朱葆三"|"朱佩珍"', "dongfangzz")
zbs_dfzzall <- unique(zbs_dfzzall)


# Search dfzz by individual name
zbs_dfzz1 <- search_documents('"朱葆三"', "dongfangzz")
zbs_dfzz2 <- search_documents('"朱佩珍"', "dongfangzz")
zbs_dfzzall2 <- bind_rows(zbs_dfzz1, zbs_dfzz2)
zbs_dfzzall2 <- unique(zbs_dfzzall2)

# Search DFZZ
zbs_dfzz1 <- search_documents('"朱葆三"', "dongfangzz")
zbs_dfzz2 <- search_documents('"朱佩珍"', "dongfangzz")
zbs_dfzz <- bind_rows(zbs_dfzz1, zbs_dfzz2)

# Retrieve documents in dfzz
zbs_dfzz_ft <- get_documents(zbs_dfzz, "dongfangzz")
zbs_dfzz_ft <- unique(zbs_dfzz_ft)

library(readr)
getwd()
zbs_dfzz_ft2 <- read_csv(chenriot/wxl/ZBS_Data/zbs_dfzz_ft.csv)


write_csv(zbs_dfzzall, "zbs_dfzzall.csv")
write_csv(zbs_dfzz, "zbs_dfzz.csv")
write_csv(zbs_dfzz_ft, "zbs_dfzz_ft.csv")


# Check article length in each corpus
zbs_dfzz_ft <- zbs_dfzz_ft %>% mutate(Length = nchar(Text))
zbs_dfzz_ft <- zbs_dfzz_ft %>% mutate(Length = nchar(Text))
zbs_proq_ft <- zbs_proq_ft %>% mutate(Length = nchar(Text))

# Visualize distribution of length in dataset

# Shenbao
# Arrange the data by Length from shortest to longest
zbs_dfzz_ft_sort <- zbs_dfzz_ft %>%
  arrange(Length)
# Plot the data
ggplot(zbs_dfzz_ft_sort, aes(x = reorder(DocId, Length), y = Length)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "DocId", y = "Length", title = "Article Length from Shortest to Longest in Shenbao") +
  theme_minimal()

# Proquest
# Arrange the data by Length from shortest to longest
zbs_proq_ft_sort <- zbs_proq_ft %>%
  arrange(Length)
# Plot the data
ggplot(zbs_proq_ft_sort, aes(x = reorder(DocId, Length), y = Length)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "DocId", y = "Length", title = "Article Length from Shortest to Longest in Proquest") +
  theme_minimal()


# Examine temporal distribution of articles in dfzz
histtext::count_documents('"朱葆三"|"朱佩珍"', "dongfangzz") %>% 
  mutate(Date=lubridate::as_date(Date,"%y%m%d")) %>% 
  mutate(Year= year(Date)) %>%  
  group_by(Year) %>% summarise(N=sum(N)) %>% 
  filter (Year>=1888 & Year<=1949) %>% 
  ggplot(aes(Year,N)) + geom_col() + 
  labs(title = "朱葆三 in the Shenbao",
       subtitle = "Number of articles mentioning 朱葆三",
       x = "Year",
       y = "Number of articles")



# Search with concordance  for each name
zbs_dfzzconc <- search_concordance('"朱葆三"|"朱佩珍"', corpus = "dongfangzz", context_size = 400)


# I reconstitute a Text column from the Before/Match/After columns
zbs_dfzzconc <- zbs_dfzzconc %>% mutate(Text = paste(Before, Matched, After))
zbs_dfzzconc <- zbs_dfzzconc %>% mutate(Text = paste(Before, Matched, After))
zbs_proqconc <- zbs_proqconc %>% mutate(Text = paste(Before, Matched, After))

zbs_dfzzconc <- zbs_dfzzconc %>% relocate(Text, .before = "Source")
zbs_dfzzconc <- zbs_dfzzconc %>% relocate(Text, .before = "Source")
zbs_proqconc <- zbs_proqconc %>% relocate(Text, .before = "Source")

write_csv(zbs_dfzzconc, "zbs_dfzzconc.csv")
write_csv(zbs_dfzzconc, "zbs_dfzzconc.csv")
write_csv(zbs_proqconc, "zbs_proqconc.csv")


# I implement NER on the concordance files 
zbs_dfzzconc_ner <- histtext::ner_on_df(zbs_dfzzconc, "Text", id_column="DocId", model = "trftc_nopunct:zh:ner") 

write_csv(zbs_dfzzconc_ner, "zbs_dfzzconc_ner.csv")


# Count NEs by Type
zbs_dfzzconc_nerCnt <- zbs_dfzzconc_ner %>% group_by(Type) %>% count()
zbs_dfzzconc_nerCnt <- zbs_dfzzconc_ner %>% group_by(Type) %>% count()
zbs_proqconc_nerCnt <- zbs_proqconc_ner %>% group_by(Type) %>% count()


# This concludes the first step of querying all three corpora with the name(s) of ZBS 

## NETWORK ANALYSIS

# Build a two-mode network (person-organization) with igraph/tidygraph

# Step 1: Load necessary libraries
library(dplyr)
library(igraph)
library(tidygraph)

# Remove unnecessary columns
zbs_dfzzconc_ner<-zbs_dfzzconc_ner %>% select(-Start, -End)

# Step 2: Filter data for 'PERSON' and 'ORG' types
filtered_dfzz <- zbs_dfzzconc_ner %>%
  filter(Type %in% c("PERSON", "ORG"))

# Step 3: Remove one-character texts
clean_dfzz <- filtered_dfzz %>%
  filter(nchar(Text) > 1)

# Step 4: Create edge list
# Split the dataframe by Type
persons_dfzz <- clean_dfzz %>% filter(Type == "PERSON")
orgs_dfzz <- clean_dfzz %>% filter(Type == "ORG")

# Join the two dataframes on DocId to create edges between PERSON and ORG within the same document
edge_dfzz <- persons_dfzz %>%
  inner_join(orgs_dfzz, by = "DocId") %>%
  select(Source = Text.x, Target = Text.y) %>%
  distinct()

# Step 6: Create node list
node_dfzz <- clean_dfzz %>%
  select(Text, Type) %>%
  distinct() %>%
  rename(id = Text)

# Step 7: Construct network with igraph
ig <- graph_from_data_frame(d = edge_dfzz, vertices = node_dfzz, directed = FALSE)


write_csv(edge_dfzz, "edge_dfzz.csv")
write_csv(node_dfzz, "node_dfzz.csv")


# Add other node attributes from 'zbs_dfzzconc_nodes'
# For example, if there's a 'type' attribute in your nodes data frame
V(ig)$type <- node_dfzz$Type[match(V(ig)$name, node_dfzz$id)]


# index nodes shape/color on nodes type 
V(ig)[node_dfzz$Type == "PERSON"]$shape <- "circle"
V(ig)[node_dfzz$Type == "ORG"]$shape <- "square"
V(ig)[node_dfzz$Type == "PERSON"]$color <- "red"
V(ig)[node_dfzz$Type == "ORG"]$color <- "light blue"

# plot with igraph 
plot.igraph(ig, vertex.size = 3, 
            vertex.label.color = "black", 
            vertex.label.cex = 0.3, 
            main="DFZZ Affiliation network")

# convert into a tidy graph object
tg <- tidygraph::as_tbl_graph(ig) %>% 
  activate(nodes) %>% 
  mutate(label=name) 

# project in padagraph
tg %>% histtext::in_padagraph("dfzzNetwork") 

# get the URL 
tg %>% histtext::get_padagraph_url("dfzzNetwork")


# Analyze the two-mode network
# Two methods to obtain the basic parameters of the network
# Order = number of nodes (nodes)
# Size = number of edges (links = ties)
# Diameter = the length of the longest geodesic 
# Average Distance = the average length between nodes
# 

ig
summary(ig)
# 
diameter(ig)
average.path.length(ig)

#  Measures of Interconnectedness
## Density: How densely connected are the nodes in these networks (two methods)
graph.density(ig) # [1] 0.1325758
edge_density(ig) # [1] 0.1325758
## Average Degree : How many ties do the nodes in these networks have, on average? 
mean(degree(ig)) # [1] 4.242424

## Number and size of components: to understand the connectivity structure of the graph ig:

# The 'no.clusters' function returns the number of connected components in the graph ig. 
no.clusters(ig)

# The clusters(ig) function computes the connected components of the graph
# It returns an object that includes various information about these components
clusters(ig)$csize


## cut points = articulation points : 
# Articulation points (or cut points) are points in a connected space (e.g. nodes in a network) such that 
# their removal cause the resulting space (network) to be disconnected

# Compute and identify the cutpoints

# Explanation:
# The 'articulation.points' function call finds all the articulation points (also known as cut nodes) in the graph ig
# An articulation point is a vertex whose removal increases the number of connected components in the graph
# A cutpoint is a point through which certain paths must pass, so its removal would disconnect parts of the graph
articulation.points(ig)


# Explanation:
# The 'articulation.points' function is called on the graph ig to find all articulation points
# The vector of articulation point ids returned by articulation_points() is converted into a data frame
# A data frame is a table or a 2-dimensional array-like structure in R that stores data in rows and columns

# Compute cutpoints
cutpoints <- articulation_points(ig)

# Convert to data frame
cutpoints_df <- data.frame(Cut.Points = V(ig)[cutpoints]$name)

# articulation_points(ig): The 'articulation.points' function is called on the graph ig to find all articulation points.
# V(ig)[cutpoints]$name: This extracts the names (or IDs, depending on your graph's vertex attributes) of the articulation points. V(ig) accesses all vertices in the graph, and [cutpoints] indexes into this list to get just the articulation points. 
# $name extracts the name attribute of these vertices, which you seem to be interested in based on your attempted renaming to "Cut.Points".
# data.frame(Cut.Points = ...): This creates a data frame with a single column named "Cut.Points", which contains the names (or IDs) of the articulation points.


# Explore the visualization possibilities
# list possible shapes in igraph 
names(igraph:::.igraph.shapes)
# You can also use the help command '?igraph::vertex.shape' for the list of supported shapes

# list possible colors in igraph
colors()

# Visualize cutpoints in the network (orange diamond)

# Explanation:
# V(ig): This retrieves the vertex sequence of the graph ig.
# %in%: This is a membership operator that checks if elements on the left are present in the right vector.
# articulation_points(ig): This function finds all articulation points in the graph
# ifelse(test, yes, no): This function works like a vectorized conditional statement
# For each element, if the test is TRUE, it assigns the value from yes, otherwise from no.
# The result of the ifelse function is assigned to the shape attribute of all nodes in ig
# Nodes that are articulation points are assigned the shape "square", and all other nodes are assigned the shape "circle"

V(ig)$shape = ifelse(V(ig) %in%
                       articulation_points(ig),   
                     "square", "circle")

# Explanation
# The operation are the same as in the script above, except that it applies to colors

V(ig)$color= ifelse(V(ig) %in%
                      articulation_points(ig),   
                    "red","orange")

plot(ig, vertex.label.color = "black", 
     vertex.label.cex = 0.3, 
     vertex.color = V(ig)$color, 
     vertex.shape = V(ig)$shape, 
     vertex.size =5, 
     main = "NRC network",
     sub = "Red squares refer to cutpoints")

# remove self loops and multiple edges
g1 <- simplify(ig)
# remove self loops only
g2 <- simplify(ig, remove.loops = TRUE) 
# Explanation
# The function is_simple(ig) in R's igraph library checks whether the provided graph ig is a simple graph
# A simple graph in graph theory has the following properties:
# No loops: There are no edges that connect nodes to themselves.
# No multiple edges: There are no duplicate edges; i.e., there is at most one edge between any pair of nodes.
# The is_simple(ig) function returns a logical value: TRUE if ig is a simple graph; FALSE if ig is not a simple graph

is_simple(ig)
is_simple(g1)
is_simple(g2)

# Explanation:
# line V(g2)[node_list$Type == "PERSON"]$shape <- "circle" sets the shape of all nodes in g2 that represent a "PERSON" 
# (as indicated by the Type column in node_list) to be a circle
# Breakdown of code
# V(g2): The V() function returns a vertex sequence, which represents all the nodes in the graph g2.
# [node_list$Type == "PERSON"]: The expression node_list$Type == "PERSON" creates a logical vector that is TRUE for rows where the Type column has the value "PERSON" and FALSE otherwise.
# $shape: This determines the shape attributed to the vertex when it is plotted
# <- "circle": This assigns the value "circle" to the shape attribute of all nodes that were indexed by the logical vector

V(g1)[node_dfzz$Type == "PERSON"]$shape <- "circle"
V(g1)[node_dfzz$Type == "ORG"]$shape <- "square"
V(g1)[node_dfzz$Type == "PERSON"]$color <- "red"
V(g1)[node_dfzz$Type == "ORG"]$color <- "light blue"

# plot with igraph 
plot.igraph(g1, vertex.size = 5, 
            vertex.label.color = "black", 
            vertex.label.cex = 0.9, 
            main="DFZZ Affiliation network")

# plot with additional visual attributes for large networks
plot(g1, vertex.size = 6, vertex.color = "tomato", vertex.frame.color = NA, vertex.label = NA, edge.curved = .1, edge.arrow.size = .3, edge.width = .7)

##### LOCAL METRICS 

Degree <- degree(ig) # degree centrality (number of edges)
Degree_norm <- degree(g1, normalized = TRUE) # number of edges divided by total number of possible edges
Eig <- evcent(g1)$vector # eigenvector
Betw <- betweenness(g1) # betweenness
Close <- closeness(g1)  # closeness

centralities <- cbind(Degree, Degree_norm, Eig, Betw, Close) # compile
centralities_df <- as.data.frame(centralities) # convert into dataframe
centralities_df_labels <- tibble::rownames_to_column(centralities_df, "id") # transform row names into column 

centralities_attributes <- inner_join(node_dfzz, centralities_df_labels)

# Visualize centralities

# Make nodes size proportionate to degree centrality

plot(g1,
     vertex.size = Degree*0.5,
     vertex.label.color = "black", 
     vertex.label.cex = V(g1)$size*0.025, 
     main="NRC network",
     sub = "Node size represents degree centrality")


# Make nodes size proportionate to eigenvector centrality

plot(g1,
     vertex.size = Eig*20,
     vertex.label.color = "black", 
     vertex.label.cex = Eig, 
     main="DFZZ network",
     sub = "Node size represents eigenvector centrality")

# nodes size proportionate to betweenness centrality

plot(g1,
     vertex.size = Betw*0.002,
     vertex.label.color = "black", 
     vertex.label=NA,
     main="DFZZ network",
     sub = "Node size represents betweenness centrality")



## Change network layout and curve edges

# Breakdown for the first line with "kk"
# the function layout_with_kk() computes the layout for a graph using the Kamada-Kawai algorithm for force-directed placement
# kk: This is the variable in which the result of the layout_with_kk() function will be stored
# layout_with_kk() is the function from the igraph package that implements the Kamada-Kawai layout algorithm

# Explanation: The Kamada-Kawai layout algorithm aims to position nodes in a graph in two-dimensional space 
# so that the distances between the nodes are as close as possible to the graph-theoretical distances (e.g., shortest path distances in the graph)
# The idea is to make the layout reflect the structure of the graph as accurately as possible.


kk <- layout_with_kk(g1)
nice <- layout_nicely(g1)
tree <- layout_as_tree(g1)
random <- layout_randomly(g1)

# Explanation:
# V(g1): This retrieves the vertex sequence (all nodes) of the graph g1
# [node_list$Type == "PERSON"]: selects only those nodes for which the corresponding entry in node_list$Type is equal to "PERSON" 
# $shape: this accesses the shape attribute of the nodes that have been filtered by the previous condition
# <- "circle": This assigns the value "circle" to the shape attribute of all the nodes that meet the condition (i.e., those nodes that represent persons)

V(g1)[node_dfzz$Type == "PERSON"]$shape <- "circle"
V(g1)[node_dfzz$Type == "ORG"]$shape <- "square"
V(g1)[node_dfzz$Type == "PERSON"]$color <- "red"
V(g1)[node_dfzz$Type == "ORG"]$color <- "light blue"

plot(g1, vertex.label.color = "black", 
     vertex.label.cex = 0.3, 
     vertex.color = V(g1)$color, 
     vertex.shape = V(g1)$shape, 
     vertex.size =5, edge.curved=0.1, layout=kk, 
     main = "DFZZ network",
     sub = "Red circles= PERS, blue squares= ORG")

plot(g1, vertex.label.color = "black", 
     vertex.label.cex = 0.3, 
     vertex.color = V(g1)$color, 
     vertex.shape = V(g1)$shape, 
     vertex.size =5, edge.curved=0.1, layout=nice, 
     main = "DFZZ network",
     sub = "Red circles= PERS, blue squares= ORG")

plot(g1, vertex.label.color = "black", 
     vertex.label.cex = 0.3, 
     vertex.color = V(g1)$color, 
     vertex.shape = V(g1)$shape, 
     vertex.size =5, edge.curved=0.1, layout=tree, 
     main = "DFZZ network",
     sub = "Red circles= PERS, blue squares= ORG")

plot(g1, vertex.label.color = "black", 
     vertex.label.cex = 0.3, 
     vertex.color = V(g1)$color, 
     vertex.shape = V(g1)$shape, 
     vertex.size =5, edge.curved=0.1, layout=random, 
     main = "DFZZ network",
     sub = "Red circles= PERS, blue squares= ORG")

# More layouts here: https://igraph.org/r/doc/layout_.html 


##COMMUNITY DETECTION 

# Subject to relevance, the purpose is to identify subgroups of nodes (persons and organizations) that are more densely connected together. 

# We proceed in four steps:

# We compare various clustering methods and select the most appropriate.
# We analyze the size of communities and their membership
# We extract, visualize and compare the largest communities (their global features)

# list of algorithms available in igraph: https://igraph.org/r/doc/communities.html 

# in this tuto, we will compare: Louvain, fast greedy, and Girvan-Newman (edge betweeness) (hierarchical clustering)
# we focus on g1 because some algorithms (fast_greedy) do not work with multiple edges 

## detect communities
lv <- cluster_louvain(g1)
fg <- cluster_fast_greedy(g1) 
eb <- cluster_edge_betweenness(g1)


ls(lv) # lists all the information packed in the result -> also names()
ls(fg)
ls(eb)
?communities # open the manual page related to communities object

## inspect results (short summary : number of communities, modularity score, membership)
print(lv)
print(fg) 
print(eb) 
# communities sizes
sizes(lv) # from 10 to 46
sizes(fg)
sizes(eb)

# compare sizes of communities in the three clustered networks
hist(sizes(lv))
hist(sizes(fg))
hist(sizes(eb))

# which nodes are in which group (community)? 

membership(lv)
membership(fg)
membership(eb) 

# plot communities 

V(g1)$group <- lv$membership # create a group for each community
V(g1)$color <- lv$membership # node color reflects group membership (1 cluster = 1 color)

plot(lv, g1, vertex.label=V(g1)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.3, 
     vertex.size=2,
     main="Communities in DFZZ network (Louvain method)",
     sub = "black ties = intra-cluster; red ties = inter-cluster")

V(g1)$group <- fg$membership # create a group for each community
V(g1)$color <- fg$membership # node color reflects group membership (1 cluster = 1 color)

plot(fg, g1, vertex.label=V(g1)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.3, 
     vertex.size=2,
     main="Communities in DFZZ network (fast greedy method)",
     sub = "black ties = intra-cluster; red ties = inter-cluster")

V(g1)$group <- eb$membership # create a group for each community
V(g1)$color <- eb$membership # node color reflects group membership (1 cluster = 1 color)

plot(eb, g1, vertex.label=V(g1)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.3, 
     vertex.size=2,
     main="Communities in DFZZ network (Girvan-Newman method)",
     sub = "black ties = intra-cluster; red ties = inter-cluster")

## Extract membership data

# Store membership data 
louvain <- data.frame(lv$membership,
                      lv$names) %>% 
  group_by(lv.membership) %>% 
  add_tally() %>% # add size of clusters
  rename(id = lv.names, cluster = lv.membership, size = n)


fastgreedy <- data.frame(fg$membership,
                         fg$names) %>% 
  group_by(fg.membership) %>% 
  add_tally() %>% # add size of clusters
  rename(id = fg.names, cluster = fg.membership, size = n)


girman <- data.frame(eb$membership,
                     eb$names) %>% 
  group_by(eb.membership) %>% 
  add_tally() %>% # add size of clusters
  rename(id = eb.names, cluster = eb.membership, size = n)

# join with nodes attributes (type of node)

louvain_attribute <- inner_join(louvain, node_dfzz, by = "id")
fastgreedy_attribute <- inner_join(fastgreedy, node_dfzz, by = "id")
girman_attribute <- inner_join(girman, node_dfzz, by = "id")

# extract communities (focus on Louvain method)
# This will create a subgraph from an existing graph
# lvg1 <- ...: This part of the code is assigning the result of the operation to the variable lvg1.
# induced_subgraph(g1, V(g1)$group==1): This function call is creating an induced subgraph from the graph g1.
# g1: This is the original graph from which you want to create a subgraph.
# V(g1): This returns the node sequence of g1, i.e., all the nodes in g1.
# $group: This accesses a node attribute named group. In this context, V(g1)$group refers to the group attribute for each node in g1.

lvg1 <- induced_subgraph(g1, V(g1)$group==1) 
lvg2 <- induced_subgraph(g1, V(g1)$group==2) 
lvg3 <- induced_subgraph(g1, V(g1)$group==3)


# VISUALIZE COMMUNITIES SEPARATELY

V(g1)[node_list$Type == "PERSON"]$shape <- "circle"
V(g1)[node_list$Type == "ORG"]$shape <- "square"
V(g1)[node_list$Type == "PERSON"]$color <- "red"
V(g1)[node_list$Type == "ORG"]$color <- "light blue"

plot(lvg1, vertex.label=V(lvg1)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.7, 
     vertex.size= 8,
     vertex.color = V(lvg1)$color, 
     vertex.shape = V(lvg1)$shape,
     edge.curved=0.8, 
     main="DFZZ Community 1 ")


plot(lvg2, vertex.label=V(lvg2)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.7, 
     vertex.size= 8,
     vertex.color = V(lvg2)$color, 
     vertex.shape = V(lvg2)$shape,
     edge.curved=0.8, 
     main="DFZZ Community 2 ")

plot(lvg3, vertex.label=V(lvg3)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.7, 
     vertex.size= 8,
     vertex.color = V(lvg3)$color, 
     vertex.shape = V(lvg3)$shape,
     edge.curved=0.8, 
     main="DFZZ Community 3")



# customize size of node (by degree centrality)

plot(lvg2, vertex.label=V(lvg2)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.7, 
     vertex.size= degree(lvg2)*2, # node size proportionate to node degree (in cluster)
     vertex.color = V(lvg2)$color, 
     vertex.shape = V(lvg2)$shape,
     edge.curved=0.8, 
     main="DFZZ Community 2 ")

plot(lvg2, vertex.label=V(lvg2)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.7, 
     vertex.size= degree(g1)*0.5, # node size proportionate to node degree (in the whole network)
     vertex.color = V(lvg2)$color, 
     vertex.shape = V(lvg2)$shape,
     edge.curved=0.8, 
     main="DFZZ Community 2 ")

# Extract and compile global metrics to compare the structures of communities 

order1 <- gorder(lvg1)
order2 <- gorder(lvg2)
order3 <- gorder(lvg3)


order <- c(order1, order2, order3)
order

size1 <- gsize(lvg1)
size2 <- gsize(lvg2)
size3 <- gsize(lvg3)


size <- c(size1, size2, size3)
size


diameter1 <- diameter(lvg1)
diameter2 <- diameter(lvg2)
diameter3 <- diameter(lvg3)

diameter <- c(diameter1, diameter2, diameter3)
diameter


edge_density1 <- edge_density(lvg1)
edge_density2 <- edge_density(lvg2)
edge_density3 <- edge_density(lvg3)

edge_density <- c(edge_density1, edge_density2, edge_density3)
edge_density


# compile 

louvain_metrics <- cbind(order, size, diameter, edge_density) 

louvain_metrics_df <- as.data.frame(louvain_metrics) # convert into dataframe
louvain_metrics_df_labels <- tibble::rownames_to_column(louvain_metrics_df, "cluster") # transform row names into column 



# Build a one-mode network (person-person through documents) 

# Now we want to project our two mode-network into a one-mode network linking persons to persons through documents. 
# First we create an edge list in the form of a table linking the source person (from) to the target person (to) - which is the standard format for igraph object:

zbs_dfzzconc_perdata <- persons_dfzz %>% 
  select(DocId, Text)

edges_dfzzconc_perdata <- inner_join(zbs_dfzzconc_perdata, zbs_dfzzconc_perdata, by = "DocId") %>%
  filter(Text.x < Text.y) %>%
  transmute(from=Text.x, to=Text.y) %>%
  distinct()

edges_dfzzconc_perdata %>% arrange(from, to)

# The inner_join() function joins the table with itself through DocID. It creates a link for each couple of relation. "Distinct()" is used to eliminate duplicates in documents. 
# Next we create the one-mode network Person-to-person using igraph and tidygraph:

edges__zbsdfzzconc_perdata_tg <- edges_dfzzconc_perdata %>% transmute(from=from, to=to) 
ig__zbsdfzzconc_perdata <- graph_from_data_frame(d=edges__zbsdfzzconc_perdata_tg, vertices=NULL, directed = FALSE)
tg__zbsdfzzconc_perdata <- tidygraph::as_tbl_graph(ig__zbsdfzzconc_perdata)

# plot with igraph 
plot.igraph(ig__zbsdfzzconc_perdata, vertex.size = 3, 
            vertex.label.color = "black", 
            vertex.label.cex = 0.9, 
            main="ZBS DFZZ  person-person DocId network")


# Build a one-mode network that link persons through organizations

# Create a bipartite graph
g_bipartite <- graph_from_data_frame(edge_dfzz, directed = FALSE)


V(g_bipartite)$type <- ifelse(V(g_bipartite)$name %in% edge_dfzz$Source, FALSE, TRUE)


# Project onto one mode (persons)
proj <- bipartite_projection(g_bipartite)

g_one_mode_person <- proj[[1]]

class(g_one_mode_person)


# Plot network
plot(g_one_mode_person,
     vertex.size=5,       # Adjust the size of the vertices
     vertex.label.cex=0.7, # Adjust the size of the vertex labels for readability
     main="Person-to-Person Network", # Title of the plot
     asp=0)               # Keep the aspect ratio at 0 to prevent distortion


# Create a layout
layout <- layout_with_fr(g_one_mode_person) # Fruchterman-Reingold layout

# Assuming you've assigned groups or any attribute to nodes and want to color them based on this
# V(g_one_mode)$group <- sample(1:3, vcount(g_one_mode), replace=TRUE) # Example attribute assignment

# Plot with custom layout and color vertices by group
plot(g_one_mode_person,
     layout=layout,
     vertex.size=5,
     vertex.color=rainbow(3)[V(g_one_mode_person)$group], # Coloring by group, replace 'group' as needed
     vertex.label.cex=0.7,
     edge.arrow.size=0.5,
     main="Person-to-Person Network",
     asp=0)




# save objects in .RData file
save.image('zbs.RData')

