# This is a script to build networks by sub-periods of ZBS's life

library(histtext)
library(lubridate)
library(ggplot2)
library(tidygraph)
library(igraph)
library(tidyverse)
library(tidytext)

# save objects in .RData file
save.image('zbsnetsb.RData')


# Re-upload saved RData file
load(file = "zbsnetsb.RData")

# Periods
# 1888-1899
# 1900-1912
# 1913-1919
# 1920-1926
# 1927-1949

# Creation of the subfiles from filtered_sb2

zbs_n1 <- filtered_sb2 %>% filter(Date <1900)
zbs_n2 <- filtered_sb2 %>% filter(Date >1899 & Date <1913)
zbs_n3 <- filtered_sb2 %>% filter(Date >1912 & Date <1920)
zbs_n4 <- filtered_sb2 %>% filter(Date >1919 & Date <1927)
zbs_n5 <- filtered_sb2 %>% filter(Date >1926)


# Examine temporal distribution of articles in SB
histtext::count_documents('"朱葆三"|"朱佩珍"', "shunpao-revised") %>% 
  mutate(Date=lubridate::as_date(Date,"%y%m%d")) %>% 
  mutate(Year= year(Date)) %>%  
  group_by(Year) %>% summarise(N=sum(N)) %>% 
  filter (Year>=1888 & Year<=1899) %>% 
  ggplot(aes(Year,N)) + geom_col() + 
  labs(title = "朱葆三 in the Shenbao",
       subtitle = "Number of articles mentioning 朱葆三 in 1888-1899",
       x = "Year",
       y = "Number of articles")

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

# Examine temporal distribution of articles in SB
histtext::count_documents('"朱葆三"|"朱佩珍"', "shunpao-revised") %>% 
  mutate(Date=lubridate::as_date(Date,"%y%m%d")) %>% 
  mutate(Year= year(Date)) %>%  
  group_by(Year) %>% summarise(N=sum(N)) %>% 
  filter (Year>=1913 & Year<=1919) %>% 
  ggplot(aes(Year,N)) + geom_col() + 
  labs(title = "朱葆三 in the Shenbao",
       subtitle = "Number of articles mentioning 朱葆三 in 1913-1919",
       x = "Year",
       y = "Number of articles")

# Examine temporal distribution of articles in SB
histtext::count_documents('"朱葆三"|"朱佩珍"', "shunpao-revised") %>% 
  mutate(Date=lubridate::as_date(Date,"%y%m%d")) %>% 
  mutate(Year= year(Date)) %>%  
  group_by(Year) %>% summarise(N=sum(N)) %>% 
  filter (Year>=1920 & Year<=1926) %>% 
  ggplot(aes(Year,N)) + geom_col() + 
  labs(title = "朱葆三 in the Shenbao",
       subtitle = "Number of articles mentioning 朱葆三 in 1920-1926",
       x = "Year",
       y = "Number of articles")

# Examine temporal distribution of articles in SB
histtext::count_documents('"朱葆三"|"朱佩珍"', "shunpao-revised") %>% 
  mutate(Date=lubridate::as_date(Date,"%y%m%d")) %>% 
  mutate(Year= year(Date)) %>%  
  group_by(Year) %>% summarise(N=sum(N)) %>% 
  filter (Year>=1927 & Year<=1949) %>% 
  ggplot(aes(Year,N)) + geom_col() + 
  labs(title = "朱葆三 in the Shenbao",
       subtitle = "Number of articles mentioning 朱葆三 in 1927-1949",
       x = "Year",
       y = "Number of articles")


# Build a two-mode network (person-organization) for zbs_n1

# Step 2: Filter data for 'PERSON' and 'ORG' types and remove 1-character names
zbs_n1 <- zbs_n1 %>%
  filter(Type %in% c("PERSON", "ORG")) %>%
  filter(nchar(Text) > 1) %>%
  select(-Start, -End)

# Step 4: Create edge list
# Split the dataframe by Type
persons_sb1 <- zbs_n1 %>% filter(Type == "PERSON")
orgs_sb1 <- zbs_n1 %>% filter(Type == "ORG")

write_csv(persons_sb1, "persons_sb1.csv")
write_csv(orgs_sb1, "orgs_sb1.csv")

### 
# Create node list and edge for Cytoscpe
zbs_nodessb1_Cy <- bind_rows(persons_sb1, orgs_sb1)
zbs_edgessb1_Cy <- inner_join(persons_sb1, orgs_sb1, by = c("DocId", "Date"))
zbs_nodessb1_Cy <- unique(zbs_nodessb1_Cy)
zbs_edgessb1_Cy <- unique(zbs_edgessb1_Cy)
zbs_edgessb1_Cy <- zbs_edgessb1_Cy %>% rename(Name = Text.x)
zbs_edgessb1_Cy <- zbs_edgessb1_Cy %>% rename(Institution = Text.y)
zbs_edgessb1_Cy <- unique(zbs_edgessb_Cy)
write_csv(zbs_edgessb1_Cy, "zbs_edgessb1_Cy.csv")
write_csv(zbs_nodes1_Cy, "zbs_nodessb1_Cy.csv")



# Step 5: Join the two dataframes on DocId to create edges between PERSON and ORG within the same document
edge_sb1 <- persons_sb1 %>%
  inner_join(orgs_sb1, by = "DocId") %>%
  select(Source = Text.x, Target = Text.y) %>%
  distinct()


# Step 6: Create node list
node_sb1 <- zbs_n1 %>%
  select(Text, Type) %>%
  distinct() %>%
  rename(id = Text)

# Step 8: Construct network with igraph
ig <- graph_from_data_frame(d = edge_sb1, vertices = node_sb1, directed = FALSE)

# Check Duplicate vertex names (= Duplicate node names)
intersect(edge_sb1$Source, edge_sb1$Target)

# If you happen to have a limited number of Duplicate vertex names, save the Intersect data
# Download the node_sb file and correct manually
# Upload the node_sb_Ed file and re-run from 


# Add other node attributes from 'zbs_sbconc_nodes'
# For example, if there's a 'type' attribute in your nodes data frame
V(ig)$type <- node_sb1$Type[match(V(ig)$name, node_sb1$id)]


# index nodes shape/color on nodes type 
V(ig)[node_sb1$Type == "PERSON"]$shape <- "circle"
V(ig)[node_sb1$Type == "ORG"]$shape <- "square"
V(ig)[node_sb1$Type == "PERSON"]$color <- "red"
V(ig)[node_sb1$Type == "ORG"]$color <- "light blue"

# plot with igraph 
plot.igraph(ig, vertex.size = 3, 
            vertex.label.color = "black", 
            vertex.label.cex = 0.3, 
            main="ZBS Affiliation network in Shenbao (1888-1899)")

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
# If node_sb1 was created or related to the original graph ig, its indices or identifiers might not directly correspond to those in main_component. 
# You need to ensure that the filtering or matching criteria are valid for the vertices in main_component.
# Assuming 'name' is a vertex attribute in both 'ig' and 'main_component'
# and 'node_sb1' contains a matching identifier in a column named 'id' or similar


# Set default shapes and colors for all vertices
V(main_component)$shape <- "circle" # Default shape
V(main_component)$color <- "red"   # Default color


# Specifically update shapes and colors for PERSON and ORG types
# Ensure 'name' is the correct vertex attribute that matches identifiers in 'node_sb1$ID'
person1_ids <- node_sb1$ID[node_sb1$Type == "PERSON"]
org1_ids <- node_sb1$ID[node_sb1$Type == "ORG"]

# Update shapes and colors based on matching identifiers
V(main_component)$shape[V(main_component)$name %in% person1_ids] <- "circle"
V(main_component)$color[V(main_component)$name %in% person1_ids] <- "red"
V(main_component)$shape[V(main_component)$name %in% org1_ids] <- "square"
V(main_component)$color[V(main_component)$name %in% org1_ids] <- "light blue"


# plot with igraph 
plot.igraph(main_component, vertex.size = 3, 
            vertex.label.color = "black", 
            vertex.label.cex = 0.3, 
            main="ZBS Affiliation Network in Shenbao (1888-1899)")


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
     main = "ZBS Affiliation Network Cutpoints in 1888-1899",
     sub = "Red squares refer to cutpoints")

# remove self loops and multiple edges
gn1mc <- simplify(main_component)

# Explanation
# The function is_simple(main_component) in R's igraph library checks whether the provided graph main_component is a simple graph
# A simple graph in graph theory has the following properties:
# No loops: There are no edges that connect nodes to themselves.
# No multiple edges: There are no duplicate edges; i.e., there is at most one edge between any pair of nodes.
# The is_simple(main_component) function returns a logical value: TRUE if main_component is a simple graph; FALSE if main_component is not a simple graph

is_simple(main_component)
is_simple(gn1mc)


V(gn1mc)$color= ifelse(V(gn1mc) %in%
                        articulation_points(gn1mc),   
                      "red","orange")

plot(gn1mc, vertex.label.color = "black", 
     vertex.label.cex = 0.3, 
     vertex.color = V(gn1mc)$color, 
     vertex.shape = V(gn1mc)$shape, 
     vertex.size =5, 
     main = "ZBS Affiliation Network Cutpoints in 1888-1899 (neat)",
     sub = "Red squares refer to cutpoints")




# plot with additional visual attributes for large networks
plot(gn1mc, vertex.size = 6, vertex.color = "tomato", vertex.frame.color = NA, vertex.label = NA, edge.curved = .1, edge.arrow.size = .3, edge.width = .7)

plot(gn1mc, vertex.label.color = "black", 
     vertex.label.cex = 0.4, 
     vertex.color = V(gn1mc)$color, 
     vertex.shape = V(gn1mc)$shape, 
     vertex.size =5, 
     edge.curved = .1, 
     edge.arrow.size = .3, 
     edge.width = .7,
     main = "ZBS Affiliation Network Cutpoints in 1888-1899 (neat)",
     sub = "Red squares refer to cutpoints")


##### LOCAL METRICS 

mc1Degree <- degree(gn1mc) # degree centrality (number of edges)
mc1Degree_norm <- degree(gn1mc, normalized = TRUE) # number of edges divided by total number of possible edges
mc1Eig <- evcent(gn1mc)$vector # eigenvector
mc1Betw <- betweenness(gn1mc) # betweenness
mc1Close <- closeness(gn1mc)  # closeness

mc1centralities <- cbind(mc1Degree, mc1Degree_norm, mc1Eig, mc1Betw, mc1Close) # compile
mc1centralities_df <- as.data.frame(mc1centralities) # convert into dataframe
mc1centralities_df_labels <- tibble::rownames_to_column(mc1centralities_df, "id") # transform row names into column 

mc1centralities_attributes <- inner_join(node_sb, mc1centralities_df_labels)

# Visualize centralities

# Make nodes size proportionate to degree centrality

plot(gn1mc,
     vertex.size = mc1Degree*0.5,
     vertex.label.color = "black", 
     vertex.label.cex = V(g1)$size*0.005, 
     main="ZBS Network in Shenbao (1888-1899)",
     sub = "Node size represents degree centrality")
