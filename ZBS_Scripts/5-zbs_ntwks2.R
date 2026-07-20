# This is a script to build networks by sub-periods of ZBS's life

library(histtext)
library(lubridate)
library(ggplot2)
library(tidygraph)
library(igraph)
library(tidyverse)
library(tidytext)

# save objects in .RData file
save.image('zbsnetsb2.RData')


# Re-upload saved RData file
load(file = "zbsnetsb2.RData")

# Periods
# 1888-1899
# 1900-1912
# 1913-1919
# 1920-1926
# 1927-1949

# Creation of the subfiles from filtered_sb2


zbs_n2 <- filtered_sb2 %>% filter(Date >1899 & Date <1913)


# Examine temporal distribution of articles in SB
histtext::count_documents('"朱葆三"|"朱佩珍"', "shunpao-revised") %>% 
  mutate(Date=lubridate::as_date(Date,"%y%m%d")) %>% 
  mutate(Year= year(Date)) %>%  
  group_by(Year) %>% summarise(N=sum(N)) %>% 
  filter (Year>=1900 & Year<=1912) %>% 
  ggplot(aes(Year,N)) + geom_col() + 
  labs(title = "朱葆三 in the Shenbao",
       subtitle = "Number of articles mentioning 朱葆三 in 1900-1912",
       x = "Year",
       y = "Number of articles")



# Build a two-mode network (person-organization) for zbs_n2

# Step 2: Filter data for 'PERSON' and 'ORG' types and remove 1-character names
zbs_n2 <- zbs_n2 %>%
  filter(Type %in% c("PERSON", "ORG")) %>%
  filter(nchar(Text) > 1) %>%
  select(-Start, -End)

# Step 4: Create edge list
# Split the dataframe by Type
persons_sb2 <- zbs_n2 %>% filter(Type == "PERSON")
orgs_sb2 <- zbs_n2 %>% filter(Type == "ORG")

write_csv(persons_sb2, "persons_sb2.csv")
write_csv(orgs_sb2, "orgs_sb2.csv")

# Clean persons_sb2 and orgs_sb2 asthey serve as a basis for edge_sb2 and Cyto files


### 
# Create node list and edge for Cytoscpe
zbs_nodessb2_Cy <- bind_rows(persons_sb2, orgs_sb2)
zbs_nodessb2_Cy <- unique(zbs_nodessb2_Cy)
zbs_edgessb2_Cy <- inner_join(persons_sb2, orgs_sb2, by = c("DocId", "Date"))
zbs_edgessb2_Cy <- unique(zbs_edgessb2_Cy)
zbs_edgessb2_Cy <- zbs_edgessb2_Cy %>% rename(Name = Text.x)
zbs_edgessb2_Cy <- zbs_edgessb2_Cy %>% rename(Institution = Text.y)
zbs_edgessb2_Cy <- unique(zbs_edgessb2_Cy)

write_csv(zbs_edgessb2_Cy, "zbs_edgessb2_Cy.csv")
write_csv(zbs_nodessb2_Cy, "zbs_nodessb2_Cy.csv")



# Step 5: Join the two dataframes on DocId to create edges between PERSON and ORG within the same document
edge_sb2 <- persons_sb2 %>%
  inner_join(orgs_sb2, by = "DocId") %>%
  select(Source = Text.x, Target = Text.y) %>%
  distinct()




# Step 6: Create node list
node_sb2 <- zbs_n2 %>%
  select(Text, Type) %>%
  distinct() %>%
  rename(id = Text)

# Step 8: Construct network with igraph
ig <- graph_from_data_frame(d = edge_sb2, vertices = node_sb2, directed = FALSE)

# Check Duplicate vertex names (= Duplicate node names)
intersect(edge_sb2$Source, edge_sb2$Target)

# If you happen to have a limited number of Duplicate vertex names, save the Intersect data
# Download the node_sb file and correct manually
# Upload the node_sb_Ed file and re-run from 


# Add other node attributes from 'zbs_sbconc_nodes'
# For example, if there's a 'type' attribute in your nodes data frame
V(ig)$type <- node_sb2$Type[match(V(ig)$name, node_sb2$id)]


# index nodes shape/color on nodes type 
V(ig)[node_sb2$Type == "PERSON"]$shape <- "circle"
V(ig)[node_sb2$Type == "ORG"]$shape <- "square"
V(ig)[node_sb2$Type == "PERSON"]$color <- "red"
V(ig)[node_sb2$Type == "ORG"]$color <- "light blue"

# plot with igraph 
plot.igraph(ig, vertex.size = 3, 
            vertex.label.color = "black", 
            vertex.label.cex = 0.3, 
            main="ZBS Affiliation network in Shenbao (1900-1912)")

### Extract the main component

# Identify the connected components
comp <- clusters(ig)

# Find the index of the largest component
largest_comp_index <- which.max(comp$csize)

# Extract the vertex ids belonging to the largest component
largest_comp_vertices <- which(comp$membership == largest_comp_index)

# Create the subgraph corresponding to the largest component
main_component <- induced_subgraph(ig, vids = largest_comp_vertices)

# Explanation:
# clusters(ig): This function identifies all connected components in the graph ig. The result (comp) includes the size of each component (csize) and a membership vector indicating the component to which each vertex belongs.
# which.max(comp$csize): This finds the index of the largest component by looking for the maximum value in the csize vector.
# which(comp$membership == largest_comp_index): This line identifies all vertices that belong to the largest component by using the membership information.
#induced_subgraph(ig, vids = largest_comp_vertices): This function creates a subgraph of ig that includes only the vertices identified as part of the largest component. The result is your main component graph.

# The 'main_component' subgraph contains the largest (main) component of your original graph 'ig'

# When you extract main_component, it contains a subset of the vertices from ig. 
# If node_sb2 was created or related to the original graph ig, its indices or identifiers might not directly correspond to those in main_component. 
# You need to ensure that the filtering or matching criteria are valid for the vertices in main_component.
# Assuming 'name' is a vertex attribute in both 'ig' and 'main_component'
# and 'node_sb2' contains a matching identifier in a column named 'id' or similar


# Set default shapes and colors for all vertices
V(main_component)$shape <- "circle" # Default shape
V(main_component)$color <- "red"   # Default color


# Specifically update shapes and colors for PERSON and ORG types
# Ensure 'name' is the correct vertex attribute that matches identifiers in 'node_sb2$ID'
person2_ids <- node_sb2$ID[node_sb2$Type == "PERSON"]
org2_ids <- node_sb2$ID[node_sb2$Type == "ORG"]

# Update shapes and colors based on matching identifiers
V(main_component)$shape[V(main_component)$name %in% person2_ids] <- "circle"
V(main_component)$color[V(main_component)$name %in% person2_ids] <- "red"
V(main_component)$shape[V(main_component)$name %in% org2_ids] <- "square"
V(main_component)$color[V(main_component)$name %in% org2_ids] <- "light blue"


# plot with igraph 
plot.igraph(main_component, vertex.size = 3, 
            vertex.label.color = "black", 
            vertex.label.cex = 0.3, 
            main="ZBS Affiliation Network in Shenbao (1900-1912)")


## CUT POINTS on Main Component 
# Articulation points (or cut points) are points in a connected space (e.g. nodes in a network) such that 
# their removal cause the resulting space (network) to be disconnected

# Compute and identify the cutpoints

# Explanation:
# The 'articulation.points' function call finds all the articulation points (also known as cut nodes) in the graph ig
# An articulation point is a vertex whose removal increases the number of connected components in the graph
# A cutpoint is a point through which certain paths must pass, so its removal would disconnect parts of the graph
articulation.points(main_component)


# Explanation:
# The 'articulation.points' function is called on the graph main_component to find all articulation points
# The vector of articulation point ids returned by articulation_points() is converted into a data frame
# A data frame is a table or a 2-dimensional array-like structure in R that stores data in rows and columns

# Compute cutpoints
cutpoints <- articulation_points(main_component)

# Convert to data frame
cutpoints_df <- data.frame(Cut.Points = V(main_component)[cutpoints]$name)

write_csv(cutpoints_df, "cutpoints2_df.csv")

# articulation_points(main_component): The 'articulation.points' function is called on the graph main_component to find all articulation points.
# V(main_component)[cutpoints]$name: This extracts the names (or IDs, depending on your graph's vertex attributes) of the articulation points. V(main_component) accesses all vertices in the graph, and [cutpoints] indexes into this list to get just the articulation points. 
# $name extracts the name attribute of these vertices, which you seem to be interested in based on your attempted renaming to "Cut.Points".
# data.frame(Cut.Points = ...): This creates a data frame with a single column named "Cut.Points", which contains the names (or IDs) of the articulation points.


# Visualize cutpoints in the network (orange diamond)

# Explanation:
# V(main_component): This retrieves the vertex sequence of the graph main_component.
# %in%: This is a membership operator that checks if elements on the left are present in the right vector.
# articulation_points(main_component): This function finds all articulation points in the graph
# ifelse(test, yes, no): This function works like a vectorized conditional statement
# For each element, if the test is TRUE, it assigns the value from yes, otherwise from no.
# The result of the ifelse function is assigned to the shape attribute of all nodes in ig
# Nodes that are articulation points are assigned the shape "square", and all other nodes are assigned the shape "circle"

V(main_component)$shape = ifelse(V(main_component) %in%
                                   articulation_points(main_component),   
                                 "square", "circle")

# Explanation
# The operation are the same as in the script above, except that it applies to colors

V(main_component)$color= ifelse(V(main_component) %in%
                                  articulation_points(main_component),   
                                "red","orange")

plot(main_component, vertex.label.color = "black", 
     vertex.label.cex = 0.3, 
     vertex.color = V(main_component)$color, 
     vertex.shape = V(main_component)$shape, 
     vertex.size =5, 
     main = "ZBS Affiliation Network Cutpoints in 1900-1912",
     sub = "Red squares refer to cutpoints")

# Export graph for Cytoscape
write_graph(main_component, "zbsnwk2oneM.graphml", format = "graphml")


# remove self loops and multiple edges
gn2mc <- simplify(main_component)

# Explanation
# The function is_simple(main_component) in R's igraph library checks whether the provided graph main_component is a simple graph
# A simple graph in graph theory has the following properties:
# No loops: There are no edges that connect nodes to themselves.
# No multiple edges: There are no duplicate edges; i.e., there is at most one edge between any pair of nodes.
# The is_simple(main_component) function returns a logical value: TRUE if main_component is a simple graph; FALSE if main_component is not a simple graph

is_simple(main_component)
is_simple(gn2mc)


V(gn2mc)$color= ifelse(V(gn2mc) %in%
                         articulation_points(gn2mc),   
                       "red","orange")

plot(gn2mc, vertex.label.color = "black", 
     vertex.label.cex = 0.3, 
     vertex.color = V(gn2mc)$color, 
     vertex.shape = V(gn2mc)$shape, 
     vertex.size =5, 
     main = "ZBS Affiliation Network Cutpoints in 1900-1912 (neat)",
     sub = "Red squares refer to cutpoints")



plot(gn2mc, vertex.label.color = "black", 
     vertex.label.cex = 0.4, 
     vertex.color = V(gn2mc)$color, 
     vertex.shape = V(gn2mc)$shape, 
     vertex.size =5, 
     edge.curved = .1, 
     edge.arrow.size = .3, 
     edge.width = .7,
     main = "ZBS Affiliation Network Cutpoints in 1900-1912 (neat)",
     sub = "Red squares refer to cutpoints")


##### LOCAL METRICS 

mc2Degree <- degree(gn2mc) # degree centrality (number of edges)
mc2Degree_norm <- degree(gn2mc, normalized = TRUE) # number of edges divided by total number of possible edges
mc2Eig <- evcent(gn2mc)$vector # eigenvector
mc2Betw <- betweenness(gn2mc) # betweenness
mc2Close <- closeness(gn2mc)  # closeness

mc2centralities <- cbind(mc2Degree, mc2Degree_norm, mc2Eig, mc2Betw, mc2Close) # compile
mc2centralities_df <- as.data.frame(mc2centralities) # convert into dataframe
mc2centralities_df_labels <- tibble::rownames_to_column(mc2centralities_df, "id") # transform row names into column 

mc2centralities_attributes <- inner_join(node_sb2, mc2centralities_df_labels)

write_csv(mc2centralities_attributes, "mc2centralities.csv")

# Visualize centralities

# Make nodes size proportionate to degree centrality

plot(gn2mc,
     vertex.size = mc2Degree*0.2,
     vertex.label.color = "black", 
     vertex.label.cex = V(g1)$size*0.005, 
     main="ZBS Network in Shenbao (1900-1912)",
     sub = "Node size represents degree centrality")



## Build a one-mode network that link persons through organizations
# Create a bipartite graph
g_bipartite <- graph_from_data_frame(edge_sb2, directed = FALSE)
V(g_bipartite)$type <- ifelse(V(g_bipartite)$name %in% edge_sb2$Source, FALSE, TRUE)

# Project onto one mode (persons)
proj <- bipartite_projection(g_bipartite)
g_one_mode_person <- proj[[1]]

# Check class of graph
class(g_one_mode_person)


# Plot network
plot(g_one_mode_person,
     vertex.size=5,       # Adjust the size of the vertices
     vertex.label.cex=0.7, # Adjust the size of the vertex labels for readability
     main="ZBS Person-to-Person Network in Shenbao", # Title of the plot
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


# Extract data for Cytoscape

# Export graph for Cytoscape
write_graph(g_one_mode_person, "zbsnwk2oneM.graphml", format = "graphml")

# Extract Edge List
edge1M_zbs2 <- get.edgelist(g_one_mode_person)
# Convert edge list to a data frame
edge1M_list2_df <- data.frame(source = edge1M_zbs2[, 1], target = edge1M_zbs2[, 2])
# Write Edge List to CSV
write.csv(edge1M_list2_df, "edge1M_list2_df.csv", row.names = FALSE)


# Extract Node Names
node1M_nzbs2 <- V(g_one_mode_person)$name
# Convert node names to a data frame
node1M_nzbs2_df <- data.frame(name = node1M_nzbs2)
# Write Node Names to CSV
write.csv(node1M_nzbs1_df, "node1M_nzbs1_df.csv", row.names = FALSE)

# save objects in .RData file
save.image('zbsnetsb2.RData')
