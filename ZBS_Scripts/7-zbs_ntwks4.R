# This is a script to build networks by sub-periods of ZBS's life

library(histtext)
library(lubridate)
library(ggplot2)
library(tidygraph)
library(igraph)
library(tidyverse)
library(tidytext)

# save objects in .RData file
save.image('zbsnetsb4.RData')


# Re-upload saved RData file
load(file = "zbsnetsb4.RData")

# Periods
# 1888-1899
# 1900-1912
# 1913-1919
# 1920-1926
# 1927-1949

# Creation of the subfiles from filtered_sb4


zbs_n4 <- filtered_sb2 %>% filter(Date >1919 & Date <1927)


# Examine temporal distribution of articles in SB
histtext::count_documents('"朱葆三"|"朱佩珍"', "shunpao-revised") %>% 
  mutate(Date=lubridate::as_date(Date,"%y%m%d")) %>% 
  mutate(Year= year(Date)) %>%  
  group_by(Year) %>% summarise(N=sum(N)) %>% 
  filter (Year>=1920 & Year<=1926) %>% 
  ggplot(aes(Year,N)) + geom_col() + 
  labs(title = "朱葆三 in the Shenbao in 1920-1926",
       subtitle = "Number of articles",
       x = "Year",
       y = "Number of articles")


# Build a two-mode network (person-organization) for zbs_n4

# Step 2: Filter data for 'PERSON' and 'ORG' types and remove 1-character names
zbs_n4 <- zbs_n4 %>%
  filter(Type %in% c("PERSON", "ORG")) %>%
  filter(nchar(Text) > 1) %>%
  select(-Start, -End)

# Step 4: Create edge list
# Split the dataframe by Type
persons_sb4 <- zbs_n4 %>% filter(Type == "PERSON")
orgs_sb4 <- zbs_n4 %>% filter(Type == "ORG")

write_csv(persons_sb4, "persons_sb4.csv")
write_csv(orgs_sb4, "orgs_sb4.csv")

### 
# Create node list and edge for Cytoscpe
zbs_nodessb4_Cy <- bind_rows(persons_sb4, orgs_sb4)
zbs_nodessb4_Cy <- unique(zbs_nodessb4_Cy)
zbs_edgessb4_Cy <- inner_join(persons_sb4, orgs_sb4, by = c("DocId", "Date"))
zbs_edgessb4_Cy <- unique(zbs_edgessb4_Cy)
zbs_edgessb4_Cy <- zbs_edgessb4_Cy %>% rename(Name = Text.x)
zbs_edgessb4_Cy <- zbs_edgessb4_Cy %>% rename(Institution = Text.y)
zbs_edgessb4_Cy <- unique(zbs_edgessb4_Cy)
write_csv(zbs_edgessb4_Cy, "zbs_edgessb4_Cy.csv")
write_csv(zbs_nodessb4_Cy, "zbs_nodessb4_Cy.csv")

# save objects in .RData file
save.image('zbsnetsb4.RData')

# Step 5: Join the two dataframes on DocId to create edges between PERSON and ORG within the same document
edge_sb4 <- persons_sb4 %>%
  inner_join(orgs_sb4, by = "DocId") %>%
  select(Source = Text.x, Target = Text.y) %>%
  distinct()


# Step 6: Create node list
node_sb4 <- zbs_n4 %>%
  select(Text, Type) %>%
  distinct() %>%
  rename(id = Text)

# Step 8: Construct network with igraph
ig <- graph_from_data_frame(d = edge_sb4, vertices = node_sb4, directed = FALSE)



# Add other node attributes from 'zbs_sbconc_nodes'
# For example, if there's a 'type' attribute in your nodes data frame
V(ig)$type <- node_sb4$Type[match(V(ig)$name, node_sb4$id)]


# index nodes shape/color on nodes type 
V(ig)[node_sb4$Type == "PERSON"]$shape <- "circle"
V(ig)[node_sb4$Type == "ORG"]$shape <- "square"
V(ig)[node_sb4$Type == "PERSON"]$color <- "red"
V(ig)[node_sb4$Type == "ORG"]$color <- "light blue"

# plot with igraph 
plot.igraph(ig, vertex.size = 3, 
            vertex.label.color = "black", 
            vertex.label.cex = 0.3, 
            main="ZBS Affiliation network in Shenbao (1920-1926)")

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
# If node_sb4 was created or related to the original graph ig, its indices or identifiers might not directly correspond to those in main_component. 
# You need to ensure that the filtering or matching criteria are valid for the vertices in main_component.
# Assuming 'name' is a vertex attribute in both 'ig' and 'main_component'
# and 'node_sb4' contains a matching identifier in a column named 'id' or similar


# Set default shapes and colors for all vertices
V(main_component)$shape <- "circle" # Default shape
V(main_component)$color <- "red"   # Default color


# Specifically update shapes and colors for PERSON and ORG types
# Ensure 'name' is the correct vertex attribute that matches identifiers in 'node_sb4$ID'
person4_ids <- node_sb4$ID[node_sb4$Type == "PERSON"]
org4_ids <- node_sb4$ID[node_sb4$Type == "ORG"]

# Update shapes and colors based on matching identifiers
V(main_component)$shape[V(main_component)$name %in% person4_ids] <- "circle"
V(main_component)$color[V(main_component)$name %in% person4_ids] <- "red"
V(main_component)$shape[V(main_component)$name %in% org4_ids] <- "square"
V(main_component)$color[V(main_component)$name %in% org4_ids] <- "light blue"


# save objects in .RData file
save.image('zbsnetsb4.RData')

# plot with igraph 
plot.igraph(main_component, vertex.size = 3, 
            vertex.label.color = "black", 
            vertex.label.cex = 0.3, 
            main="ZBS Affiliation Network in Shenbao (1920-1926)")


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

write_csv(cutpoints_df, "cutpoints4_df.csv")

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
     main = "ZBS Affiliation Network Cutpoints in 1920-1926",
     sub = "Red squares refer to cutpoints")

# remove self loops and multiple edges
gn4mc <- simplify(main_component)

# Explanation
# The function is_simple(main_component) in R's igraph library checks whether the provided graph main_component is a simple graph
# A simple graph in graph theory has the following properties:
# No loops: There are no edges that connect nodes to themselves.
# No multiple edges: There are no duplicate edges; i.e., there is at most one edge between any pair of nodes.
# The is_simple(main_component) function returns a logical value: TRUE if main_component is a simple graph; FALSE if main_component is not a simple graph

is_simple(main_component)
is_simple(gn4mc)


V(gn4mc)$color= ifelse(V(gn4mc) %in%
                         articulation_points(gn4mc),   
                       "red","orange")

plot(gn4mc, vertex.label.color = "black", 
     vertex.label.cex = 0.3, 
     vertex.color = V(gn4mc)$color, 
     vertex.shape = V(gn4mc)$shape, 
     vertex.size =5, 
     main = "ZBS Affiliation Network Cutpoints in 1920-1926 (neat)",
     sub = "Red squares refer to cutpoints")



plot(gn4mc, vertex.label.color = "black", 
     vertex.label.cex = 0.4, 
     vertex.color = V(gn4mc)$color, 
     vertex.shape = V(gn4mc)$shape, 
     vertex.size =5, 
     edge.curved = .1, 
     edge.arrow.size = .3, 
     edge.width = .7,
     main = "ZBS Affiliation Network Cutpoints in 1920-1926 (neat)",
     sub = "Red squares refer to cutpoints")

# save objects in .RData file
save.image('zbsnetsb4.RData')

##### LOCAL METRICS 

mc4Degree <- degree(gn4mc) # degree centrality (number of edges)
mc4Degree_norm <- degree(gn4mc, normalized = TRUE) # number of edges divided by total number of possible edges
mc4Eig <- evcent(gn4mc)$vector # eigenvector
mc4Betw <- betweenness(gn4mc) # betweenness
mc4Close <- closeness(gn4mc)  # closeness

mc4centralities <- cbind(mc4Degree, mc4Degree_norm, mc4Eig, mc4Betw, mc4Close) # compile
mc4centralities_df <- as.data.frame(mc4centralities) # convert into dataframe
mc4centralities_df_labels <- tibble::rownames_to_column(mc4centralities_df, "id") # transform row names into column 

mc4centralities_attributes <- inner_join(node_sb4, mc4centralities_df_labels)

write_csv(mc4centralities_attributes, "mc4centralities.csv")


# save objects in .RData file
save.image('zbsnetsb4.RData')


# Visualize centralities

# Make nodes size proportionate to degree centrality

plot(gn4mc,
     vertex.size = mc4Degree*0.2,
     vertex.label.color = "black", 
     vertex.label.cex = V(g1)$size*0.005, 
     main="ZBS Network in Shenbao (1920-1926)",
     sub = "Node size represents degree centrality")




## Build a one-mode network that link persons through organizations
# Create a bipartite graph
g_bipartite <- graph_from_data_frame(edge_sb4, directed = FALSE)
V(g_bipartite)$type <- ifelse(V(g_bipartite)$name %in% edge_sb4$Source, FALSE, TRUE)

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

# Extract Edge List
edge_zbs4<- get.edgelist(g_one_mode_person)
# Convert edge list to a data frame
edge_list_df <- data.frame(source = edge_zbs4[, 1], target = edge_zbs4[, 2])
# Write Edge List to CSV
write.csv(edge_list_df, "edges1M_zbs4.csv", row.names = FALSE)


# Extract Node Names
node_nzbs4<- V(g_one_mode_person)$name
# Convert node names to a data frame
node_zbs4_df <- data.frame(name = node_nzbs4)
# Write Node Names to CSV
write.csv(node_zbs4_df, "nodes1M_zbs4.csv", row.names = FALSE)

# save objects in .RData file
save.image('zbsnetsb4.RData')
