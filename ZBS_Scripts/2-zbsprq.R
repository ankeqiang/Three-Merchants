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
save.image('zbsprq.RData')


# Re-upload saved RData file
load(file = "zbsprq.RData")

# Search ZBS in the corpus, trying various possible spellings, using the function search_documents() : 
zbs_prqall <- search_documents('"Chu Pao-san"|"Chu Pei-chen"|"Chu Pei-chên"|"Chu Pao San"|"Chu Péi-chên"', "proquest")
zbs_prqall <- unique(zbs_prqall)

zbs_prqall <- zbs_prqall %>% 
  mutate(Date=lubridate::as_date(Date,"%y%m%d")) %>% 
  mutate(Year= year(Date))

# Search prq by individual name
zbs_prq1 <- search_documents('"Chu Pao-san"', "proquest")
zbs_prq2 <- search_documents('"Chu Pei-chen"', "proquest")
zbs_prq3 <- search_documents('"Chu Pei-chên"', "proquest")
zbs_prq4 <- search_documents('"Chu Pao San"', "proquest")
zbs_prq5 <- search_documents('"Chu Péi-chên"', "proquest")
zbs_prq6 <- search_documents('"Chu P\'ei-chen"', "proquest")
zbs_prqbind <- bind_rows(zbs_prq1, zbs_prq2)
zbs_prqbind <- unique(zbs_prqbind)

# Retrieve documents in proquest
zbs_proq_ft <- get_documents(zbs_prqall, "proquest")
zbs_proq_ft <- unique(zbs_proq_ft)
# Add Year column
zbs_proq_ft <- zbs_proq_ft %>% 
  mutate(Date=lubridate::as_date(Date,"%y%m%d")) %>% 
  mutate(Year= year(Date))


write_csv(zbs_prqall, "zbs_prqall.csv")
write_csv(zbs_proq_ft, "zbs_prq_ft.csv")


# Check article length in corpus
zbs_proq_ft <- zbs_proq_ft %>% mutate(Length = nchar(Text))

# Visualize distribution of length in dataset

# Proquest
# Arrange the data by Length from shortest to longest
zbs_proq_ft_sort <- zbs_proq_ft %>%
  arrange(Length)
# Plot the data
ggplot(zbs_proq_ft_sort, aes(x = reorder(DocId, Length), y = Length)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "DocId", y = "Length", title = "Article Length from Shortest to Longest in Shenbao") +
  theme_minimal()


# The data set contains extra-long articles and irrelevant content
# I remove content based on the title column

zbs_proq_ft_sort2 <- zbs_proq_ft_sort %>% 
  filter(!str_detect(Title, "Classified Ad")) %>% 
  filter(!str_detect(Title, "Display Ad")) %>% 
  filter(!str_detect(Title, "H.B.M.'S CIVIL SUMMARY COURT")) %>% 
  filter(!str_detect(Text, "(?i)rue chu pao-san")) %>% 
  filter(!str_detect(Text, "(?i)rue chu pao- san")) %>% 
  filter(!str_detect(Text, "(?i)rue chu pao san")) %>% 
  filter(!str_detect(Text, "(?i)rue du chu pao san")) %>% 
  filter(!str_detect(Text, "(?i)rue chu pao-shan")) %>% 
  filter(!str_detect(Text, "(?i)ru chu pao-shan")) %>% 
  filter(!str_detect(Text, "(?i)ruet chu pao san")) %>% 
  filter(!str_detect(Text, "(?i)ue chu pao san"))  %>% 
  filter(!str_detect(Text, "(?i)rue 1 chu pao san")) %>% 
  filter(!str_detect(Text, "(?i)chu pao san lu")) %>% 
  filter(!str_detect(Text, "(?i)chu pao-san lu")) %>% 
  filter(!str_detect(Text, "(?i) route chu pao-san")) %>% 
  filter(!str_detect(Text, "(?i) route chu pao san")) %>% 
  filter(Length > 300) %>% 
  filter(Year < 1936)

write_csv(zbs_proq_ft_sort2, "zbs_proq_ft_sort2.csv")


# From the original 1,426 articles, filtering reduces the number of articles to 547
# ZBS was often not the topic of the article, but the street named after him

# Arrange the data by Length from shortest to longest
zbs_proq_ft_sort2 <- zbs_proq_ft_sort2 %>%
  arrange(Length)
# Plot the data
ggplot(zbs_proq_ft_sort2, aes(x = reorder(DocId, Length), y = Length)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "DocId", y = "Length", title = "Article Length from Shortest to Longest in Proquest") +
  theme_minimal()


# Examine temporal distribution of articles in SB
zbs_proq_temp <- zbs_proq_ft_sort2 %>%
  mutate(Year = as.integer(str_sub(Date, 1, 4)))
# Add a column 'N' with each value set to 1
zbs_proq_temp <- zbs_proq_temp %>%
  mutate(N = 1) %>%
  group_by(Year) %>%
  summarise(N = sum(N)) %>%
  mutate(Source = "ProQuest")

# Plot the data
ggplot(zbs_proq_temp, aes(x = Year, y = N)) + 
  geom_col(fill = "darkgreen") + 
  labs(title = "Zhu Baosan in the English-language Press",
       subtitle = "Number of mentions",
       x = "Year",
       y = "Number of articles") +
  theme(panel.background = element_rect(fill = "lightgrey")) 

write_csv(zbs_proq_temp, "zbs_proq_temp.csv")

# Examine temporal distribution of articles across publications from direct search
search_documents('"Chu Pao-san"|"Chu Pei-chen"|"Chu Pei-chên"|"Chu Pao San"|"Chu Péi-chên"', "proquest") %>%
  mutate(Year = stringr::str_sub(Date,0,4)) %>% 
  group_by(Year) %>% count(Source) %>%
  ggplot(aes(x=Year, y=n, fill=Source)) + 
  geom_col(alpha = 0.8) +
  theme(legend.position="bottom", 
        legend.text = element_text(size=8)) + 
  labs(title = "Zhu Baosan in the English-language press (1890-1949)", 
       subtitle = "Distribution of mentions by newspaper", 
       caption = "based on data extracted from the ProQuest collection 'Chinese Historical Newspapers'",
       y = "Number of articles", size = 2)



# Examine temporal distribution of articles across publications from curated dataset
zbs_proq_ft_sort2 %>% 
  group_by(Year) %>% count(Source) %>%
  ggplot(aes(x=Year, y=n, fill=Source)) + 
  geom_col(alpha = 0.8) + 
  scale_x_discrete(breaks = c(1895, 1905, 1915, 1925, 1935))  + 
  theme(legend.position="bottom", 
        legend.text = element_text(size=8)) + 
  labs(title = "Zhu Baosan in the English-language press (1895-1935)", 
       subtitle = "Distribution of mentions by newspaper", 
       caption = "based on data extracted from the ProQuest collection 'Chinese Historical Newspapers'",
       y = "Number of articles", size = 2)



### This part of the script has become irrelevant
# Search with concordance  for each name
zbs_prqconc1 <- search_concordance('"Chu Pao-san"', corpus = "proquest", context_size = 1000)
zbs_prqconc2 <- search_concordance('"Chu Pei-chen"', corpus = "proquest", context_size = 1000)
zbs_proqconc3 <- search_concordance('"Chu Pei-chên"', corpus = "proquest", context_size = 1000)
zbs_proqconc4 <- search_concordance('"Chu Pao San"', corpus = "proquest", context_size = 1000)
zbs_proqconc5 <- search_concordance('"Chu P\'ei-chen"', corpus = "proquest", context_size = 1000)

zbs_prqconc <- bind_rows(zbs_prqconc1, zbs_prqconc2, zbs_prqconc3, zbs_prqconc4, zbs_prqconc5)
zbs_prqconc <- unique(zbs_prqconc)

# I reconstitute a Text column from the Before/Match/After columns
zbs_prqconc <- zbs_proqconc %>% mutate(Text = paste(Before, Matched, After))
zbs_prqconc <- zbs_prqconc %>% mutate(Text = paste(Before, Matched, After))
zbs_proqconc <- zbs_proqconc %>% mutate(Text = paste(Before, Matched, After))

zbs_prqconc <- zbs_prqconc %>% relocate(Text, .before = "Source")
zbs_prqconc <- zbs_prqconc %>% relocate(Text, .before = "Source")
zbs_proqconc <- zbs_proqconc %>% relocate(Text, .before = "Source")

# Add Year column
zbs_proqconc <- zbs_proqconc %>% 
  mutate(Date=lubridate::as_date(Date,"%y%m%d")) %>% 
  mutate(Year= year(Date))

write_csv(zbs_prqconc, "zbs_prqconc.csv")
##### END of concordance search



# I implement NER 
zbs_prq_ner <- histtext::ner_on_df(zbs_proq_ft_sort2, text_column = "Text", id_column = "DocId", model = "spacy:en:ner") 

# Rename file
zbs_prq_ner <- zbs_proqconc_ner

# Remove all "the "

zbs_prq_ner <- zbs_prq_ner %>%
  mutate(Text = str_remove(Text, "^(?i)the\\s"))

# Explanation of the regular expression ^(?i)the\s:
  
#  ^ asserts the position at the start of the string.
# (?i) makes the match case-insensitive.
# the matches the characters "the" literally.
# \s matches any whitespace character (like space).

# Correct the variants of Chu Pao-san

# Construct a Regex Pattern: The pattern should be flexible enough to match most of the variations in your list, focusing on the key parts of the name: "Chu", "Pao", and "san". We'll allow for optional characters and spaces between these key parts.
# This pattern accounts for:
#  Optional spaces, hyphens, and apostrophes between "Chu" and "Pao", and "Pao" and "san".
# Variations of "san" including common OCR errors.
# A trailing [\\s'-.a-zA-Z0-9]* to catch any additional unexpected characters or names following the core name.
# In this updated pattern, [\\W_]* at the beginning will match any non-word characters (anything but a-z, A-Z, 0-9, and _) and the underscore character any number of times at the start. This way, it can capture "-Chu Pao-san" along with other variations where "Chu" might be preceded by symbols or spaces.

pattern <- "[\\W_]*Chu[\\s'-]*Pao[\\s'-]*(san|sah|sau|kuei|San)[\\s'-.a-zA-Z0-9]*"
zbs_prq_ner <- zbs_prq_ner %>%
  mutate(Text = str_replace_all(Text, regex(pattern, ignore_case = TRUE), "Chu Pao-san"))

# This time it caught everything

zbs_prq_ner <- unique(zbs_prq_ner)

# Select DocId and year column in zbs_prq_all
zbs_prq_yearID <- zbs_proq_ft %>% select(DocId, Year)
# Add Year to zbs_proqconc_ner
zbs_prq_ner <- left_join(zbs_prq_ner, zbs_prq_yearID)
write_csv(zbs_prq_ner, "zbs_prq_ner.csv")


# Count NEs by Type
zbs_prq_nerCnt <- zbs_prq_ner %>% group_by(Type) %>% count()



# This concludes the first step of querying all three corpora with the name(s) of ZBS 

## NETWORK ANALYSIS

# Build a two-mode network (person-organization) with igraph/tidygraph

# Step 1: Load necessary libraries
library(dplyr)
library(igraph)
library(tidygraph)

# Remove unnecessary columns
zbs_prq_nwk<-zbs_prq_ner %>% select(-Start, -End, - Confidence)

# Step 2: Filter data for 'PERSON' and 'ORG' types
filtered_prq <- zbs_prqconc_nwk %>%
  filter(Type %in% c("PERSON", "ORG"))

# Step 3: Remove one-character texts
clean_prq <- filtered_prq %>%
  filter(nchar(Text) > 2)

clean_prq <- clean_prq %>% mutate(Type = if_else(Text == "Massey" & Type == "ORG", "PERSON", Type))
clean_prq <- clean_prq %>% mutate(Type = if_else(Text == "Kavkaz" & Type == "ORG", "PERSON", Type))
clean_prq <- clean_prq %>% mutate(Type = if_else(Text == "Welch" & Type == "ORG", "PERSON", Type))
clean_prq <- clean_prq %>% mutate(Type = if_else(Text == "Roach" & Type == "ORG", "PERSON", Type))
clean_prq <- clean_prq %>% mutate(Type = if_else(Text == "Chu Pao San" & Type == "ORG", "PERSON", Type))
clean_prq <- clean_prq %>% mutate(Type = if_else(Text == "Chu Pao-san" & Type == "ORG", "PERSON", Type))
clean_prq <- clean_prq %>% mutate(Type = if_else(Text == "Yice" & Type == "ORG", "PERSON", Type))
clean_prq <- clean_prq %>% mutate(Type = if_else(Text == "Chambers" & Type == "ORG", "PERSON", Type))
clean_prq <- clean_prq %>% mutate(Type = if_else(Text == "Wells" & Type == "ORG", "PERSON", Type))
clean_prq <- clean_prq %>% mutate(Type = if_else(Text == "Brooke-Smith" & Type == "ORG", "PERSON", Type))
clean_prq <- clean_prq %>% mutate(Type = if_else(Text == "Sin Wan Pao" & Type == "PERSON", "ORG", Type))
clean_prq <- clean_prq %>% mutate(Type = if_else(Text == "Shing Yue Hong" & Type == "ORG", "PERSON", Type))

clean_prq <- clean_prq %>% filter(!str_detect(Text, "Harbin"))
clean_prq <- clean_prq %>% filter(!str_detect(Text, "Home"))


write_csv(clean_prq, "clean_prq.csv")

# Manual corrections
# New File: clean_prq2

# Step 4: Create edge list
# Split the dataframe by Type
persons_prq <- clean_prq2 %>% filter(Type == "PERSON")
orgs_prq <- clean_prq2 %>% filter(Type == "ORG")

write_csv(persons_prq, "persons_prq.csv")
write_csv(orgs_prq, "orgs_prq.csv")

### DO NOT RUN the code below. It is correct, but I edited the names manually and uploaded an edited file in Files
# Create node list and edge for Cytoscpe
zbs_nodes_Cy <- bind_rows(persons_prq, orgs_prq)
zbs_edges_Cy <- inner_join(persons_prq, orgs_prq, by = c("DocId", "Year"))
zbs_nodes_Cy <- unique(zbs_nodes_Cy)
zbs_edges_Cy <- unique(zbs_edges_Cy)
zbs_edges_Cy <- zbs_edges_Cy %>% select(-Type.x, -Type.y, -DocId)
zbs_edges_Cy <- unique(zbs_edges_Cy)
write_csv(zbs_edges_Cy, "zbs_edges_Cy.csv")
write_csv(zbs_nodes_Cy, "zbs_nodes_Cy.csv")

# Join the two dataframes on DocId to create edges between PERSON and ORG within the same document
edge_prq <- persons_prq %>%
  inner_join(orgs_prq, by = "DocId") %>%
  select(Source = Text.x, Target = Text.y) %>%
  distinct()

# Step 6: Create node list
node_prq <- clean_prq2 %>%
  select(Text, Type) %>%
  distinct() %>%
  rename(id = Text)

write_csv(edge_prq, "edge_prq.csv")
write_csv(node_prq, "node_prq.csv")



# Check names that are present as both PERS and ORG
intersect(edge_prq$Source, edge_prq$Target)

# [1] "Canton Guild"                  "Kelly and Walsh Ltd"           "Kempffer British Cigarette Co" "Moore Co Ltd"                  "Patterson"                    
# [6] "Red Cross Society"             "Shen Tun-ho" 
# [1] "Moore Co Ltd" "Patterson"  

# Since I had a persisting issue with igraph, even if my intersect show no duplicate vertex
# ig <- graph_from_data_frame(d = edge_prq, vertices = node_prq, directed = FALSE)
# Erreur dans graph_from_data_frame(d = edge_prq, vertices = node_prq, directed = FALSE) : 
#  Duplicate vertex names
# De plus : Messages d'avis :
# 1: Dans graph_from_data_frame(d = edge_prq, vertices = node_prq, directed = FALSE) :
#  In `d' `NA' elements were replaced with string "NA"
# 2: Dans graph_from_data_frame(d = edge_prq, vertices = node_prq, directed = FALSE) :
#  In `vertices[,1]' `NA' elements were replaced with string "NA"
# > # Check names that are present as both PERS and ORG
# > intersect(edge_prq$Source, edge_prq$Target)
# character(0)

# The following script serves to checkl the presence of undetected duplicate or NA values

# 1. Check for NA values
# Check for NAs in edge data frame
sum(is.na(edge_prq$Source))
sum(is.na(edge_prq$Target))

# Check for NAs in vertices data frame
sum(is.na(node_prq$id))  # Assuming 'name' is the column for vertex names

# Remove or replace NA values
edge_prq$Source[is.na(edge_prq$Source)] <- "NA_Source"
edge_prq$Target[is.na(edge_prq$Target)] <- "NA_Target"
node_prq$id[is.na(node_prq$id)] <- "NA_Vertex"

# CH Remove the "NA_Target"
edge_prq <- edge_prq %>% filter(!str_detect(Target, "NA_Target"))

# 2. Verify Unique Vertex Names
# Ensure unique vertex names
any(duplicated(node_prq$id))

# CH: Check duplicate node
node_prq_cnt <- node_prq %>% group_by(id) %>% count()
# Remove duplicate nodes
node_prq <- unique(node_prq)

# The operations above showed that there were duplicates and NA values in the original clean_prq2 file
# I removed them manually

write_csv(edge_prq, "edge_prq.csv")
write_csv(node_prq, "node_prq.csv")

# Step 7: Construct network with igraph
ig <- graph_from_data_frame(d = edge_prq, vertices = node_prq, directed = FALSE)


# Add other node attributes from 'zbs_prqconc_nodes'
# For example, if there's a 'type' attribute in your nodes data frame
V(ig)$type <- node_prq$Type[match(V(ig)$name, node_prq$id)]


# index nodes shape/color on nodes type 
V(ig)[node_prq$Type == "PERSON"]$shape <- "circle"
V(ig)[node_prq$Type == "ORG"]$shape <- "square"
V(ig)[node_prq$Type == "PERSON"]$color <- "red"
V(ig)[node_prq$Type == "ORG"]$color <- "light blue"

# plot with igraph 
plot.igraph(ig, vertex.size = 3, 
            vertex.label.color = "black", 
            vertex.label.cex = 0.3, 
            main="ZBS prq Affiliation network")

# convert into a tidy graph object
tg <- tidygraph::as_tbl_graph(ig) %>% 
  activate(nodes) %>% 
  mutate(label=name) 

# project in padagraph
tg %>% histtext::in_padagraph("prqNetwork") 

# get the URL 
tg %>% histtext::get_padagraph_url("prqNetwork")


# Analyze the two-mode network
# Two methods to obtain the basic parameters of the network
# Order = number of nodes (nodes)
# Size = number of edges (links = ties)
# Diameter = the length of the longest geodesic 
# Average Distance = the average length between nodes
# 

ig
summary(ig)
# IIGRAPH 10e9ba0 UN-B 2885 5369 -- 
diameter(ig)
#[1] 10
average.path.length(ig)
# [1] 3.511511


#  Measures of Interconnectedness
## Density: How densely connected are the nodes in these networks (two methods)
graph.density(ig) # [1] 0.001290572
edge_density(ig) # [1] 0.001290572
## Average Degree : How many ties do the nodes in these networks have, on average? 
mean(degree(ig)) # [1] 3.72201

## Number and size of components: to understand the connectivity structure of the graph ig:

# The 'no.clusters' function returns the number of connected components in the graph ig. 
no.clusters(ig)
#[1] 818

# The clusters(ig) function computes the connected components of the graph
# It returns an object that includes various information about these components
clusters(ig)$csize
# [1] 1971    13    6 



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

# Now 'main_component' contains the largest (main) component of your original graph 'ig'

# Assuming 'name' is a vertex attribute in both 'ig' and 'main_component'
# and 'node_sb' contains a matching identifier in a column named 'id' or similar


# index nodes shape/color on nodes type 
V(main_component)[node_prq$Type == "PERSON"]$shape <- "circle"
V(main_component)[node_prq$Type == "ORG"]$shape <- "square"
V(main_component)[node_prq$Type == "PERSON"]$color <- "red"
V(main_component)[node_prq$Type == "ORG"]$color <- "light blue"


# Explanation:
# line V(main_component)[node_list$Type == "PERSON"]$shape <- "circle" sets the shape of all nodes in main_component that represent a "PERSON" 
# (as indicated by the Type column in node_list) to be a circle
# Breakdown of code
# V(main_component): The V() function returns a vertex sequence, which represents all the nodes in the graph gmc.
# [node_list$Type == "PERSON"]: The expression node_list$Type == "PERSON" creates a logical vector that is TRUE for rows where the Type column has the value "PERSON" and FALSE otherwise.
# $shape: This determines the shape attributed to the vertex when it is plotted
# <- "circle": This assigns the value "circle" to the shape attribute of all nodes that were indexed by the logical vector


# plot with igraph 
plot.igraph(main_component, vertex.size = 3, 
            vertex.label.color = "black", 
            vertex.label.cex = 0.3, 
            main="ZBS prq network MC")

# Export for Cytoscape
write_graph(main_component, "zbsprqMC.graphml", format = "graphml")


# Extraction of the next largest component
# Identify connected components
comp_info <- clusters(ig)

# Find the largest component
largest_comp_size <- max(comp_info$csize)
largest_comp_index <- which.max(comp_info$csize)

# Exclude the largest component and find the next largest
comp_sizes_excl_largest <- comp_info$csize[-largest_comp_index]
next_largest_comp_size <- max(comp_sizes_excl_largest)
next_largest_comp_index <- which(comp_info$csize == next_largest_comp_size)

# Extract the vertex ids belonging to the next largest component
next_largest_comp_vertices <- which(comp_info$membership == next_largest_comp_index)

# Create the subgraph for the next largest component
next_largest_component <- induced_subgraph(ig, vids = next_largest_comp_vertices)

# index nodes shape/color on nodes type 
V(next_largest_component)[node_prq$Type == "PERSON"]$shape <- "circle"
V(next_largest_component)[node_prq$Type == "ORG"]$shape <- "square"
V(next_largest_component)[node_prq$Type == "PERSON"]$color <- "red"
V(next_largest_component)[node_prq$Type == "ORG"]$color <- "light blue"

# plot with igraph 
plot.igraph(next_largest_component, vertex.size = 3, 
            vertex.label.color = "black", 
            vertex.label.cex = 0.5, 
            main="ZBS 2nd largest prq Affiliation network")

# Export for Cytoscape
write_graph(next_largest_component, "zbsMCnxt.graphml", format = "graphml")

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


# Explore the visualization possibilities
# list possible shapes in main_componentraph 
names(igraph:::.igraph.shapes)
# You can also use the help command '?igraph::vertex.shape' for the list of supported shapes

# list possible colors in igraph
colors()

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
     main = "Cutpoints in ZBS prq network",
     sub = "Red squares refer to cutpoints")

# remove self loops and multiple edges
gmc <- simplify(main_component)

# Explanation
# The function is_simple(main_component) in R's igraph library checks whether the provided graph main_component is a simple graph
# A simple graph in graph theory has the following properties:
# No loops: There are no edges that connect nodes to themselves.
# No multiple edges: There are no duplicate edges; i.e., there is at most one edge between any pair of nodes.
# The is_simple(main_component) function returns a logical value: TRUE if main_component is a simple graph; FALSE if main_component is not a simple graph

is_simple(main_component)
is_simple(gmc)




V(gmc)$shape = ifelse(V(gmc) %in%
                                   articulation_points(gmc),   
                                 "square", "circle")

# Explanation
# The operation are the same as in the script above, except that it applies to colors

V(gmc)$color= ifelse(V(gmc) %in%
                                  articulation_points(gmc),   
                                "red","orange")


# plot with igraph 
plot.igraph(gmc, vertex.size = 5, 
            vertex.label.color = "black", 
            vertex.label.cex = 0.9, 
            main="Cutpoints in ZBS prq Affiliation network")

# plot with additional visual attributes for large networks
plot(gmc, vertex.size = 6, vertex.color = "tomato", vertex.frame.color = NA, vertex.label = NA, edge.curved = .1, edge.arrow.size = .3, edge.width = .7)


# Export for Cytoscape
write_graph(gmc, "zbscutpoints.graphml", format = "graphml")


##### LOCAL METRICS 

mcDegree <- degree(gmc) # degree centrality (number of edges)
mcDegree_norm <- degree(gmc, normalized = TRUE) # number of edges divided by total number of possible edges
mcEig <- evcent(gmc)$vector # eigenvector
mcBetw <- betweenness(gmc) # betweenness
mcClose <- closeness(gmc)  # closeness

mccentralities <- cbind(mcDegree, mcDegree_norm, mcEig, mcBetw, mcClose) # compile
mccentralities_df <- as.data.frame(mccentralities) # convert into dataframe
mccentralities_df_labels <- tibble::rownames_to_column(mccentralities_df, "id") # transform row names into column 

mccentralities_attributes <- inner_join(node_prq, mccentralities_df_labels)
write_csv(mccentralities_attributes, "zbsmccentralities.csv")


# Visualize centralities

# Make nodes size proportionate to degree centrality

plot(gmc,
     vertex.size = Degree*0.5,
     vertex.label.color = "black", 
     vertex.label.cex = V(g1)$size*0.025, 
     main="ZBS prq Affiliation network",
     sub = "Node size represents degree centrality")


# Make nodes size proportionate to eigenvector centrality

plot(gmc,
     vertex.size = Eig*20,
     vertex.label.color = "black", 
     vertex.label.cex = Eig, 
     main="ZBS prq Affiliation network",
     sub = "Node size represents eigenvector centrality")

# nodes size proportionate to betweenness centrality

plot(gmc,
     vertex.size = Betw*0.002,
     vertex.label.color = "black", 
     vertex.label=NA,
     main="ZBS prq Affiliation network",
     sub = "Node size represents betweenness centrality")



## Change network layout and curve edges

# Breakdown for the first line with "kk"
# the function layout_with_kk() computes the layout for a graph using the Kamada-Kawai algorithm for force-directed placement
# kk: This is the variable in which the result of the layout_with_kk() function will be stored
# layout_with_kk() is the function from the igraph package that implements the Kamada-Kawai layout algorithm

# Explanation: The Kamada-Kawai layout algorithm aims to position nodes in a graph in two-dimensional space 
# so that the distances between the nodes are as close as possible to the graph-theoretical distances (e.g., shortest path distances in the graph)
# The idea is to make the layout reflect the structure of the graph as accurately as possible.


kk <- layout_with_kk(gmc)
nice <- layout_nicely(gmc)
tree <- layout_as_tree(gmc)
random <- layout_randomly(gmc)

# Explanation:
# V(gmc): This retrieves the vertex sequence (all nodes) of the graph gmc
# [node_list$Type == "PERSON"]: selects only those nodes for which the corresponding entry in node_list$Type is equal to "PERSON" 
# $shape: this accesses the shape attribute of the nodes that have been filtered by the previous condition
# <- "circle": This assigns the value "circle" to the shape attribute of all the nodes that meet the condition (i.e., those nodes that represent persons)

V(gmc)[node_prq$Type == "PERSON"]$shape <- "circle"
V(gmc)[node_prq$Type == "ORG"]$shape <- "square"
V(gmc)[node_prq$Type == "PERSON"]$color <- "red"
V(gmc)[node_prq$Type == "ORG"]$color <- "light blue"

plot(gmc, vertex.label.color = "black", 
     vertex.label.cex = 0.3, 
     vertex.color = V(gmc)$color, 
     vertex.shape = V(gmc)$shape, 
     vertex.size =5, edge.curved=0.1, layout=kk, 
     main = "ZBS prq Affiliation network",
     sub = "Red circles= PERS, blue squares= ORG")

plot(gmc, vertex.label.color = "black", 
     vertex.label.cex = 0.3, 
     vertex.color = V(gmc)$color, 
     vertex.shape = V(gmc)$shape, 
     vertex.size =5, edge.curved=0.1, layout=nice, 
     main = "ZBS prq Affiliation network",
     sub = "Red circles= PERS, blue squares= ORG")

plot(gmc, vertex.label.color = "black", 
     vertex.label.cex = 0.3, 
     vertex.color = V(gmc)$color, 
     vertex.shape = V(gmc)$shape, 
     vertex.size =5, edge.curved=0.1, layout=tree, 
     main = "ZBS prq Affiliation network",
     sub = "Red circles= PERS, blue squares= ORG")

plot(gmc, vertex.label.color = "black", 
     vertex.label.cex = 0.3, 
     vertex.color = V(gmc)$color, 
     vertex.shape = V(gmc)$shape, 
     vertex.size =5, edge.curved=0.1, layout=random, 
     main = "ZBS prq Affiliation network",
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
# we focus on gmc because some algorithms (fast_greedy) do not work with multiple edges 

## detect communities
lv <- cluster_louvain(gmc)
fg <- cluster_fast_greedy(gmc) 
eb <- cluster_edge_betweenness(gmc)


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

V(gmc)$group <- lv$membership # create a group for each community
V(gmc)$color <- lv$membership # node color reflects group membership (1 cluster = 1 color)

plot(lv, gmc, vertex.label=V(gmc)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.3, 
     vertex.size=2,
     main="Communities in prq network (Louvain method)",
     sub = "black ties = intra-cluster; red ties = inter-cluster")

V(gmc)$group <- fg$membership # create a group for each community
V(gmc)$color <- fg$membership # node color reflects group membership (1 cluster = 1 color)

plot(fg, gmc, vertex.label=V(gmc)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.3, 
     vertex.size=2,
     main="Communities in prq network (fast greedy method)",
     sub = "black ties = intra-cluster; red ties = inter-cluster")

V(gmc)$group <- eb$membership # create a group for each community
V(gmc)$color <- eb$membership # node color reflects group membership (1 cluster = 1 color)

plot(eb, gmc, vertex.label=V(gmc)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.3, 
     vertex.size=2,
     main="Communities in prq network (Girvan-Newman method)",
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

louvain_attribute <- inner_join(louvain, node_prq, by = "id")
fastgreedy_attribute <- inner_join(fastgreedy, node_prq, by = "id")
girman_attribute <- inner_join(girman, node_prq, by = "id")

# extract communities (focus on Louvain method)
# This will create a subgraph from an existing graph
# lvgmc <- ...: This part of the code is assigning the result of the operation to the variable lvgmc.
# induced_subgraph(gmc, V(gmc)$group==1): This function call is creating an induced subgraph from the graph gmc.
# gmc: This is the original graph from which you want to create a subgraph.
# V(gmc): This returns the node sequence of gmc, i.e., all the nodes in gmc.
# $group: This accesses a node attribute named group. In this context, V(gmc)$group refers to the group attribute for each node in gmc.

lvgmc <- induced_subgraph(gmc, V(gmc)$group==1) 
lvg2mc <- induced_subgraph(gmc, V(gmc)$group==2) 
lvg3mc <- induced_subgraph(gmc, V(gmc)$group==3)


# VISUALIZE COMMUNITIES SEPARATELY

V(gmc)[node_list$Type == "PERSON"]$shape <- "circle"
V(gmc)[node_list$Type == "ORG"]$shape <- "square"
V(gmc)[node_list$Type == "PERSON"]$color <- "red"
V(gmc)[node_list$Type == "ORG"]$color <- "light blue"

plot(lvgmc, vertex.label=V(lvgmc)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.7, 
     vertex.size= 8,
     vertex.color = V(lvgmc)$color, 
     vertex.shape = V(lvgmc)$shape,
     edge.curved=0.8, 
     main="prq Community 1 ")


plot(lvg2, vertex.label=V(lvg2)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.7, 
     vertex.size= 8,
     vertex.color = V(lvg2)$color, 
     vertex.shape = V(lvg2)$shape,
     edge.curved=0.8, 
     main="prq Community 2 ")

plot(lvg3, vertex.label=V(lvg3)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.7, 
     vertex.size= 8,
     vertex.color = V(lvg3)$color, 
     vertex.shape = V(lvg3)$shape,
     edge.curved=0.8, 
     main="prq Community 3")



# customize size of node (by degree centrality)

plot(lvg2, vertex.label=V(lvg2)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.7, 
     vertex.size= degree(lvg2)*2, # node size proportionate to node degree (in cluster)
     vertex.color = V(lvg2)$color, 
     vertex.shape = V(lvg2)$shape,
     edge.curved=0.8, 
     main="prq Community 2 ")

plot(lvg2, vertex.label=V(lvg2)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.7, 
     vertex.size= degree(gmc)*0.5, # node size proportionate to node degree (in the whole network)
     vertex.color = V(lvg2)$color, 
     vertex.shape = V(lvg2)$shape,
     edge.curved=0.8, 
     main="prq Community 2 ")

# Extract and compile global metrics to compare the structures of communities 

order1 <- gorder(lvgmc)
order2 <- gorder(lvg2)
order3 <- gorder(lvg3)


order <- c(order1, order2, order3)
order

size1 <- gsize(lvgmc)
size2 <- gsize(lvg2)
size3 <- gsize(lvg3)


size <- c(size1, size2, size3)
size


diameter1 <- diameter(lvgmc)
diameter2 <- diameter(lvg2)
diameter3 <- diameter(lvg3)

diameter <- c(diameter1, diameter2, diameter3)
diameter


edge_density1 <- edge_density(lvgmc)
edge_density2 <- edge_density(lvg2)
edge_density3 <- edge_density(lvg3)

edge_density <- c(edge_density1, edge_density2, edge_density3)
edge_density


# compile 

louvain_metrics <- cbind(order, size, diameter, edge_density) 

louvain_metrics_df <- as.data.frame(louvain_metrics) # convert into dataframe
louvain_metrics_df_labels <- tibble::rownames_to_column(louvain_metrics_df, "cluster") # transform row names into column 

# save objects in .RData file
save.image('zbs.RData')





### From affiliation network to one-mode network

## Build a one-mode network (person-person through documents) 

# Now we want to project our two mode-network into a one-mode network linking persons to persons through documents. 
# First we create an edge list in the form of a table linking the source person (from) to the target person (to) - which is the standard format for igraph object:

zbs_prqconc_perdata <- persons_prq %>% 
  select(DocId, Text)

edges_prqconc_perdata <- inner_join(zbs_prqconc_perdata, zbs_prqconc_perdata, by = "DocId") %>%
  filter(Text.x < Text.y) %>%
  transmute(from=Text.x, to=Text.y) %>%
  distinct()

edges_prqconc_perdata %>% arrange(from, to)

# The inner_join() function joins the table with itself through DocID. It creates a link for each couple of relation. "Distinct()" is used to eliminate duplicates in documents. 
# Next we create the one-mode network Person-to-person using igraph and tidygraph:

edges__zbsprqconc_perdata_tg <- edges_prqconc_perdata %>% transmute(from=from, to=to) 
ig__zbsprqconc_perdata <- graph_from_data_frame(d=edges__zbsprqconc_perdata_tg, vertices=NULL, directed = FALSE)
tg__zbsprqconc_perdata <- tidygraph::as_tbl_graph(ig__zbsprqconc_perdata)

# plot with igraph 
plot.igraph(ig__zbsprqconc_perdata, vertex.size = 3, 
            vertex.label.color = "black", 
            vertex.label.cex = 0.9, 
            main="ZBS prq  person-DocId network")


## Build a one-mode network that link persons through organizations

# Create a bipartite graph
g_bipartite <- graph_from_data_frame(edge_prq, directed = FALSE)


V(g_bipartite)$type <- ifelse(V(g_bipartite)$name %in% edge_prq$Source, FALSE, TRUE)


# Project onto one mode (persons)
proj <- bipartite_projection(g_bipartite)

g_one_mode_person <- proj[[1]]

class(g_one_mode_person)


# Plot network
plot(g_one_mode_person,
     vertex.size=5,       # Adjust the size of the vertices
     vertex.label.cex=0.7, # Adjust the size of the vertex labels for readability
     main="ZBS Person-to-Person Network in Proquest", # Title of the plot
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
edge_1Mprq <- get.edgelist(g_one_mode_person)

# Convert edge list to a data frame
edge_list_df <- data.frame(source = edge_1Mprq[, 1], target = edge_1Mprq[, 2])

# Write Edge List to CSV
write.csv(edge_list_df, "edges1M_prqzbs.csv", row.names = FALSE)


# Extract Node Names
node_1Mprq <- V(g_one_mode_person)$name

# Convert node names to a data frame
node_1Mprq_df <- data.frame(name = node_1Mprq)

# Write Node Names to CSV
write.csv(node_1Mprq_df, "nodes1M_prqzbs.csv", row.names = FALSE)

