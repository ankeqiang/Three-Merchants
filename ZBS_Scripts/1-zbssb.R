# First we load the necessary packages 

library(histtext)
library(lubridate)
library(ggplot2)
library(tidygraph)
library(igraph)
library(tidyverse)
library(tidytext)


library(httr)
library(jsonlite)



# Search ZBS in the corpus, trying various possible spellings, using the function search_documents() : 
zbs_wiki <- search_documents('"朱葆三"', "wikibio-zh")
zbs_wiki_ft <- get_documents(zbs_wiki, "wikibio-zh")
write_csv(zbs_wiki_ft, "zbs_wiki_ft.csv")

yqq_wiki <- search_documents('"虞洽卿"', "wikibio-zh")
yqq_wiki_ft <- get_documents(yqq_wiki, "wikibio-zh")
write_csv(yqq_wiki_ft, "yqq_wiki_ft.csv")

wxl_wiki <- search_documents('"王曉籟"', "wikibio-zh")
wxl_wiki_ft <- get_documents(wxl_wiki, "wikibio-zh")
write_csv(wxl_wiki_ft, "wxl_wiki_ft.csv")


# list all available corpora 
histtext::list_corpora()

# save objects in .RData file
save.image('zbssb.RData')


# Re-upload saved RData file
load(file = "zbssb.RData")


# Search ZBS in the corpus, trying various possible spellings, using the function search_documents() : 
zbs_sball <- search_documents('"朱葆三"|"朱佩珍"', "shunpao-revised")
zbs_sball <- unique(zbs_sball)

# Search SB by individual name
zbs_sb1 <- search_documents('"朱葆三"', "shunpao-revised")
zbs_sb2 <- search_documents('"朱佩珍"', "shunpao-revised")
zbs_sball2 <- bind_rows(zbs_sb1, zbs_sb2)
zbs_sball2 <- unique(zbs_sball2)

# Retrieve documents in Shenbao
zbs_sb_ft <- get_documents(zbs_sball, "shunpao-revised")
write_csv(zbs_sball, "zbs_sball.csv")
write_csv(zbs_sb_ft, "zbs_sb_ft.csv")

# Remove all mentions of 朱葆三路
zbs_sb_ft <- zbs_sb_ft %>% 
  filter(!str_detect(Text, "朱葆三路")) %>% 
  filter(!str_detect(Text, "朱葆三 路"))
# 5103 rows

# Check article length in each corpus
zbs_sb_ft <- zbs_sb_ft %>% mutate(Length = nchar(Text))

# Visualize distribution of length in dataset
# Arrange the data by Length from shortest to longest
zbs_sb_ft_sort <- zbs_sb_ft %>%
  arrange(Length)
# Plot the data
ggplot(zbs_sb_ft_sort, aes(x = reorder(DocId, Length), y = Length)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "DocId", y = "Length", title = "Article Length from Shortest to Longest in Shenbao") +
  theme_minimal()


# Examine temporal distribution of articles in SB
zbs_sb_temp <- zbs_sb_ft_sort %>%
  mutate(Year = as.integer(str_sub(Date, 1, 4)))
# Add a column 'N' with each value set to 1
zbs_sb_temp <- zbs_sb_temp %>%
  mutate(N = 1) %>%
  group_by(Year) %>%
  summarise(N = sum(N)) 
# Plot the data
ggplot(zbs_sb_temp, aes(x = Year, y = N)) + 
  geom_col(fill = "blue") + 
  labs(title = "朱葆三 in Shenbao",
       subtitle = "Number of mentions",
       x = "Year",
       y = "Number of articles") +
  theme(panel.background = element_rect(fill = "lightgrey")) 

write.csv(zbs_sb_temp, "zbs_sb_temp.csv")

# Compare temporal distribution in Shenbao and English-language press
# Create files
zbs_sb_temp <- zbs_sb_temp %>% mutate(Source = "Shenbao")
zbs_sbprq <- bind_rows(zbs_sb_temp, zbs_proq_temp)

# Create the histogram
ggplot(zbs_sbprq, aes(x = Year, weight = N, fill = Source)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  labs(title = "Temporal Distribution of Zhu Baosan in the Shenbao and English-language Press",
       x = "Year",
       y = "Count",
       fill = "Group") +
  theme_minimal()




# save objects in .RData file
save.image('zbssb.RData')

# The articles present the recurrent issue of extra-long articles
# For NER, I opt for selecting the articles through concordance

# Search with concordance  for each name
zbs_sbconc <- search_concordance('"朱葆三"|"朱佩珍"', corpus = "shunpao-revised", context_size = 400)
# Initially 7350 rows

# Reconstitute a Text column from the Before/Match/After columns
zbs_sbconc <- zbs_sbconc %>% mutate(Text = paste(Before, Matched, After))
zbs_sbconc <- zbs_sbconc %>% relocate(Text, .before = "Source")

# Select all rows that mention 朱葆三路
zbs_lu <- zbs_sbconc %>% filter(str_detect(Text, "朱葆三路"))
zbs_lu2 <- zbs_sbconc %>% filter(str_detect(Text, "朱葆三 路"))
zbs_lu <- bind_rows(zbs_lu, zbs_lu2)

# Remove all rows that mention of 朱葆三路
zbs_sbconc <- zbs_sbconc %>% 
  filter(!str_detect(Text, "朱葆三路")) %>% 
  filter(!str_detect(Text, "朱葆三 路"))
# Initially 6285 rows
write_csv(zbs_sbconc, "zbs_sbconc.csv")

# Join with file with extracted categories of articles
# Done with ChatGPT

# Filtering file with ChatGPT (classification, extraction of relevant text)
zbs_sbconc_ext2 <- zbs_sbconc_ext2 %>% filter(!str_detect(Text, "朱葆三 路"))
zbs_sbconc_ext2 <- zbs_sbconc_ext2 %>% filter(!str_detect(Text, "朱葆三路"))
write_csv(zbs_sbconc_ext2, "zbs_sbconc_ext2.csv")
# After filtering all ads, conc_ext2 file has 4250 articles

# Examine temporal distribution of concordance articles in SB (data frame)
# Group by 'Year' and count the number of articles
yearly_counts <- zbs_sbconc_ext2 %>%
  group_by(Year) %>%
  summarise(N = n())
# Filter years 1888 to 1949
yearly_counts <- yearly_counts %>%
  filter(Year >= 1888 & Year <= 1949)
# Plot the data
ggplot(yearly_counts, aes(x = Year, y = N)) +
  geom_col(fill = "blue") +
  labs(title = "朱葆三 in Shenbao",
       subtitle = "Number of articles",
       x = "Year",
       y = "Number of articles") +
  theme(panel.background = element_rect(fill = "lightgrey")) 


# Extract TM article sample from zbs_sbconc_ext2
zbsTMsample <- left_join(zbs_tm_articles, zbs_sbconc_ext2)
zbsTMsample <- zbsTMsample%>% filter(!is.na(Date))
zbsTMsample <- unique(zbsTMsample)
write_csv(zbsTMsample, "zbsTMsample.csv")


# NER on concordance files 
zbs_sbconc_ner <- histtext::ner_on_corpus(zbs_sbconc_ext2, corpus = "shunpao-revised", only_precomputed = TRUE) 
write_csv(zbs_sbconc_ner, "zbs_sbconc_ner.csv")

# Count NEs by Type
zbs_sbconc_nerCnt <- zbs_sbconc_ner %>% group_by(Type) %>% count()

# Remove unnecessary columns
zbs_sbconc_ner<-zbs_sbconc_ner %>% select(-Start, -End, -Confidence)
# Remove non sinograms (punctuation, symbols)
zbs_sbconc_ner <- zbs_sbconc_ner %>% mutate(Text = str_replace_all(Text, "[^[\\p{Han}]]", " "))
# Remove cells with empty strings (from the previous operation)
zbs_sbconc_ner <- zbs_sbconc_ner %>%
  filter(Text != "") 
# Minor manual curation to remove empty rows and white spaces at beginning of a few rows
# 1,660,076 rows

# save objects in .RData file
save.image('zbssb.RData')

# Create file without cardinals and ordinals
zbs_sbconc_ner2 <- zbs_sbconc_ner %>%
  filter(!str_detect(Type, "CARDINAL|DATE|QUANTITY|MONEY|ORDINAL|TIME"))
# Remove one-character texts
zbs_sbconc_ner2 <- zbs_sbconc_ner2 %>%
  filter(nchar(Text) > 1)
write_csv(zbs_sbconc_ner2, "zbs_sbconc_ner2.csv")
# 880,567 rows

# Create files by type of NE
zbssb_ner_LOC<-zbs_sbconc_ner %>% filter(Type == "LOC")
zbssb_ner_GPE<-zbs_sbconc_ner %>% filter(Type == "GPE")
zbssb_ner_NORP<-zbs_sbconc_ner %>% filter(Type == "NORP")
zbssb_ner_FAC<-zbs_sbconc_ner %>% filter(Type == "FAC")
zbssb_ner_EVE<-zbs_sbconc_ner %>% filter(Type == "EVENT")
# Remove one-character texts
zbssb_ner_LOC <- zbssb_ner_LOC %>%
  filter(nchar(Text) > 1)
zbssb_ner_GPE <- zbssb_ner_GPE %>%
  filter(nchar(Text) > 1)
zbssb_ner_NORP <- zbssb_ner_NORP %>%
  filter(nchar(Text) > 1)
zbssb_ner_FAC <- zbssb_ner_FAC %>%
  filter(nchar(Text) > 1)
zbssb_ner_EVE <- zbssb_ner_EVE %>%
  filter(nchar(Text) > 1)
write_csv(zbssb_ner_LOC, "zbssb_ner_LOC.csv")
write_csv(zbssb_ner_GPE, "zbssb_ner_GPE.csv")
write_csv(zbssb_ner_NORP, "zbssb_ner_NORP.csv")
write_csv(zbssb_ner_FAC, "zbssb_ner_FAC.csv")
write_csv(zbssb_ner_EVE, "zbssb_ner_EVE.csv")

# This concludes the first step of querying all three corpora with the name(s) of ZBS 
# Based on the distribution of articles over time, I define 5 periods in ZBS's life
# 1888-1899
# 1900-1912
# 1913-1918
# 1919-1926
# 1927-1949

## TOPIC MODELING
# Prepare files for topic modeling
# Create period files
zbs_p1 <- zbs_sbconc_ext2 %>% filter(Year <1900)
zbs_p2 <- zbs_sbconc_ext2 %>% filter(Year >1899 & Year <1913)
zbs_p3 <- zbs_sbconc_ext2 %>% filter(Year >1912 & Year <1919)
zbs_p4 <- zbs_sbconc_ext2 %>% filter(Year >1918 & Year <1927)
zbs_p5 <- zbs_sbconc_ext2 %>% filter(Year >1926)

# save objects in .RData file
save.image('zbssb.RData')


### NETWORK ANALYSIS

# Build a two-mode network (person-organization) with igraph/tidygraph

# Step 2: Filter data for 'PERSON' and 'ORG' types
filtered_sb <- zbs_sbconc_ner %>%
  filter(Type %in% c("PERSON", "ORG"))

# Remove non sinograms (punctuation, symbols)
filtered_sb <- filtered_sb %>% mutate(Text = str_replace_all(Text, "[^[\\p{Han}]]", " "))
# Remove cells with empty strings (from the previous operation)
filtered_sb <- filtered_sb %>%
  filter(Text != "") %>%
  filter(!is.na(Text))

# Step 3: Remove one-character texts
clean_sb <- filtered_sb %>% mutate(Length =(nchar(Text)))
clean_sb <- clean_sb %>% filter(Length > 1)
write_csv(clean_sb, "clean_sb.csv")

# Split up rows where name segmentation was not done properly
# Split the 'Text' column into multiple rows
clean_sb <- clean_sb %>%
  separate_rows(Text, sep = "\\s+") %>%
  filter(Text != "")  # Optional: filter out any empty entries
# Remove original Length columns
clean_sb <- clean_sb %>% select(-Length)
# Add new length column
clean_sb <- clean_sb %>% mutate(Length =(nchar(Text)))
clean_sb <- clean_sb %>% filter(Length > 1)
# 509,126 rows
write_csv(clean_sb, "clean_sb.csv")

# save objects in .RData file
save.image('zbssb.RData')

# Manual curation： light cleaning of obvious incomplete, names, wrong names, etc.
# Upload curated file clean_sb2
# 508,183 rows

# Remove NA values
clean_sb2 <- clean_sb2 %>%
  filter(Text != "") %>%
  filter(!is.na(Text))
# 505,417 rows

# Removal of all 君 and 氏 at end of names
clean_sb2 <- clean_sb2 %>%
  mutate(Text = sub("君$", "", Text))  # Removes '君' at the end of strings in the Text column
clean_sb2 <- clean_sb2 %>%
  mutate(Text = sub("氏$", "", Text))
# Removal of all 君 at beginning of names
clean_sb2 <- clean_sb2 %>%
  mutate(Text = sub("^君", "", Text))  # Removes '君' at the beginning of strings in the Text column
clean_sb2 <- clean_sb2 %>%
  mutate(Text = sub("^云", "", Text))

# Some 君 inserts still appear, but I cannot apply a single rules as 君 is sometimes part of the name
# I select all rows where 君 appears in second positions for manual curation
clean_sb_jun <- clean_sb2 %>%
  filter(str_sub(Text, 2, 2) == "君")  # Select rows where the second character is '君'
write_csv(clean_sb_jun, "clean_sb_jun.csv")
# Remove all rows with 君 inserts from clean_sb
clean_sb2 <- clean_sb2 %>%
  filter(!str_sub(Text, 2, 2) == "君")
# Remove original Length columns
clean_sb2 <- clean_sb2 %>% select(-Length, -Date)
# Add new length column
clean_sb2 <- clean_sb2 %>% mutate(Length =(nchar(Text)))
clean_sb2 <- clean_sb2 %>% filter(Length > 1)
# Add curated clean_sb_jun_Ed
clean_sb_jun_Ed <- clean_sb_jun_Ed %>% select(-Date)
clean_sb2 <- bind_rows(clean_sb2, clean_sb_jun_Ed)
clean_sb2 <- clean_sb2 %>% filter(!is.na(Text))
clean_sb2 <- unique(clean_sb2)
write_csv(clean_sb, "clean_sb.csv")
# 325,322 rows

# Select ORG to remove names with less than 2 characters
clean_sb2_Org <- clean_sb2 %>% filter(Type == "ORG")
clean_sb2_Per <- clean_sb2 %>% filter(Type == "PERSON")
clean_sb2_Org <- clean_sb2_Org %>% filter(Length > 2)
clean_sb2_Org <- clean_sb2_Org %>% select(-Date)
clean_sb2_Per <- clean_sb2_Per %>% select(-Date)
# Reconstitute clean_sb2
clean_sb2 <- bind_rows(clean_sb2_Per, clean_sb2_Org)
clean_sb2 <- unique(clean_sb2)
# Select names with more than 15 characters
clean_sb2_15k <- clean_sb2 %>% filter(Length > 15)
write_csv(clean_sb2_15k, "clean_sb2_15k.csv")
# Separation of the long names after a given string of characters
clean_sb2_15kc <- clean_sb2_15k %>%
  mutate(Text = gsub("總會", "\n總會", Text)) %>%
  separate_rows(Text, sep = "\n")
clean_sb2_15kc <- clean_sb2_15kc %>%
  mutate(Text = gsub("(會)(\\S)", "\\1\n\\2", Text)) %>%
  separate_rows(Text, sep = "\n")
clean_sb2_15kc <- bind_rows(clean_sb2_15k, clean_sb2_15kc)
write_csv(clean_sb2_15kc, "clean_sb2_15kc.csv")
# Select rows with less than 16 characters
clean_sb2_temp<- clean_sb2 %>% filter(Length < 16)
# Reconstitute clean_sb2
clean_sb2 <- bind_rows(clean_sb2_temp, clean_sb2_15kc)
clean_sb2 <- unique(clean_sb2)
# Final file with 306,579 rows for ORG and PERSON
write_csv(clean_sb2, "clean_sb2.csv")
# save objects in .RData file
save.image('zbssb.RData')




# I want to add the Start-End columns to clean_sb2
# Select PERSON and ORG in zbs_conc_ner
zbs_sbconc_ner <- unique(zbs_sbconc_ner)
# 954,636 rows
zbs_sbconc_ner_raw <- unique(zbs_sbconc_ner_raw)
# 1,312,799 rows
zbs_sbconc_ner_join <- left_join(zbs_sbconc_ner, zbs_sbconc_ner_raw, by = join_by(DocId, Type, Text))
zbs_sbconc_ner_join <- unique(zbs_sbconc_ner_join)
# # 1,311,316 rows

zbs_PerOrg <- zbs_sbconc_ner_join %>%
  filter(Type %in% c("PERSON", "ORG")) 
zbs_PerOrg <- left_join(clean_sb2, zbs_PerOrg, by = join_by(DocId, Type, Text))
zbs_PerOrg <- unique(zbs_PerOrg)
zbs_PerOrg <- zbs_PerOrg %>% filter(!is.na(Start))
write_csv(zbs_PerOrg, "zbs_PerOrg.csv")
zbs_PerStrt <- zbs_PerOrg %>% filter(Type == "PERSON")
zbs_PerStrt <- unique(zbs_PerStrt)
zbs_OrgStrt <- zbs_PerOrg %>% filter(Type == "ORG")
zbs_OrgStrt <- unique(zbs_OrgStrt)
zbs_zbs <- zbs_PerOrg %>% filter(str_detect(Text, "朱葆三"))
zbs_OrgStrt <- bind_rows(zbs_OrgStrt, zbs_zbs)
write_csv(zbs_OrgStrt, "zbs_OrgStrt.csv")
write_csv(zbs_PerStrt, "zbs_PerStrt.csv")

# Identify organization in close textual proximity to 朱葆三
# This is based on co-occurrence with a max. distance of 10 characters
# Filter for "朱葆三" and organizations
zhubao_data <- zbs_OrgStrt[zbs_OrgStrt$Text == "朱葆三", ]
org_data <- zbs_OrgStrt[zbs_OrgStrt$Type == "ORG", ]

# Initialize a list to store results
results <- list()

# Iterate through each instance of "朱葆三"
for (i in 1:nrow(zhubao_data)) {
  current_entry <- zhubao_data[i, ]
  
  # Filter for organizations in the same document
  same_doc_orgs <- org_data[org_data$DocId == current_entry$DocId, ]
  
  # Determine proximity of organizations to "朱葆三"
  close_orgs <- same_doc_orgs[abs(same_doc_orgs$Start - current_entry$End) <= 15 |
                                abs(same_doc_orgs$End - current_entry$Start) <= 15, ]
  
  # Collect results if any organizations are close
  if (nrow(close_orgs) > 0) {
    results[[length(results) + 1]] <- close_orgs
  }
}

# Combine all results into one dataframe
final_results <- do.call(rbind, results)
final_results <- unique(final_results)
write.csv(final_results, "final_results.csv")


# Select only the organizations that immediately precede 朱葆三 in the text
# Initialize a list to store results
results <- list()

# Iterate through each instance of "朱葆三"
for (i in 1:nrow(zhubao_data)) {
  current_entry <- zhubao_data[i, ]
  
  # Filter for organizations in the same document
  same_doc_orgs <- org_data[org_data$DocId == current_entry$DocId, ]
  
  # Determine organizations that end within 12 characters before the start of "朱葆三"
  close_orgs <- same_doc_orgs[same_doc_orgs$End <= current_entry$Start & 
                                current_entry$Start - same_doc_orgs$End <= 12, ]
  
  # Collect results if any organizations meet the criteria
  if (nrow(close_orgs) > 0) {
    results[[length(results) + 1]] <- close_orgs
  }
}

# Combine all results into one dataframe
final_results2 <- do.call(rbind, results)
final_results2 <- unique(final_results2)
write.csv(final_results2, "final_results2.csv")



# It is at this stage that one needs to verify that there are no "duplicate vertex names" (= Duplicate node names)
# If one waits until implementing igraph, it will be too late 
# The main reason is that correcting the node and edge list means correcting file without DocId

# Identify Text values with more than one Type
clean_sb_issues <- clean_sb2 %>%
  group_by(Text) %>%                 # Group data by Text column
  summarise(Types = unique(Type)) %>% # Summarise to get unique Types for each Text
  filter(length(Types) > 1)           # Filter groups where the number of unique Types is more than 1

# To see the original rows for the inconsistent Text values
clean_sb_inconsistent <- clean_sb2 %>%
  filter(Text %in% clean_sb_issues$Text) # Filter original data for Texts with more than one Type
# Export clean_sb_inconsistent and curate manually
write_csv(clean_sb_inconsistent, "clean_sb_inconsistent.csv")

# Remove the rows with duplicate vertex for future row binding
clean_sb_temp <- anti_join(clean_sb2, clean_sb_inconsistent, by = c("Text", "Type"))

# Upload manually curated file
# Reconstitute clean_sb
clean_sb2 <- bind_rows(clean_sb_temp, clean_sb_incons_Ed)
# Remove Length column
clean_sb2 <- clean_sb2 %>% select(-Length)
clean_sb2 <- clean_sb2 %>% mutate(Length =(nchar(Text)))
clean_sb2 <- unique(clean_sb2)
# 305,398 rows
write_csv(clean_sb2, "clean_sb2.csv")

# save objects in .RData file
save.image('zbssb.RData')







#############
# Alternative script to remove "Duplicate vertex names" 
# If igraph indicates your data has "Duplicate vertex names" (nodes)
# You need to reshape your data and make sure the same vertices (nodes) do not appear in two different "Types"
# If there are few cases, you can process them using the code lines at Step 6 and replace the node_sb with node_sb2
# If there are many cases of duplicate vertices, you need to clean the data in the original "filtered_sb" that contains "ORG" and "PERSON" (in our case, but it coul dbe other categories of NEs)
# Check duplicated names with different types.

# Identify texts that appear in more than one "Type"
text_with_multiple_types <- filtered_sb %>%
  group_by(Text) %>%
  summarize(UniqueTypes = n_distinct(Type)) %>%
  filter(UniqueTypes > 1) %>%
  select(-UniqueTypes) %>%
  ungroup()

# Filter the original data to only include rows with texts that have multiple types
filtered_dupli <- filtered_sb %>%
  semi_join(text_with_multiple_types, by = "Text")
write_csv(filtered_dupli, "filtered_dupli.csv")

# Group by Text and Filter: It groups the dataset by the "Text" column, then filters to keep only the groups where the number of distinct "Type" values is greater than 1. 
# This intermediate dataset (texts_with_multiple_types) contains all the rows (including duplicates and multiple types) for texts identified as having multiple types.
# Semi Join: It then uses semi_join to filter the original dataset, keeping only those rows where "Text" matches one of the texts in texts_with_multiple_types. 
# This way, you maintain the original structure and all relevant columns, including "Type", but limit the dataset to only include the texts of interest.

# Manual curation of the filtered_dupli file
# Removal of all uncertain terms
# Unification of types
# Join filtered_dupli & filtered_dupli_ed by Text
filtered_dupli <- filtered_dupli %>% rename(Text_orig = Text)
filtered_dupli_join <- left_join(filtered_dupli, filtered_dupli_Ed, by = "DocId")
filtered_dupli_join <- filtered_dupli_join %>% filter(!is.na(Text))
filtered_dupli_join <- filtered_dupli_join %>% select(-Text_orig, -Type.y)
filtered_dupli_join <- filtered_dupli_join %>% rename(Type = Type.x)

# Remove the data in "filtered_dupli" from the original "filtered_sb" file
filtered_dupli <- filtered_dupli %>% rename(Text = Text_orig)
filtered_sb2 <- anti_join(filtered_sb, filtered_dupli, by = join_by(DocId, Type, Text))
# Add back the data in the curated "filtered_dupli_Ed" file
clean_sb <- bind_rows(filtered_sb2, filtered_dupli_join)
write_csv(filtered_sb2, "filtered_sb2.csv")
write_csv(clean_sb, "clean_sb.csv")
################



## Build networks
# Build a two-mode network (person-organization) with igraph/tidygraph


# Step 1: Create edge list
# Split the dataframe by Type
persons_sb <- clean_sb2 %>% filter(Type == "PERSON") %>% 
    select(-Length, -Year) %>%
    distinct()
orgs_sb <- clean_sb2 %>% filter(Type == "ORG")  %>% 
  select(-Length) %>%
  distinct()

write_csv(persons_sb, "persons_sb.csv")
write_csv(orgs_sb, "orgs_sb.csv")


### CYTOSCAPE FILE PREPARATION
# Create node list and edge for Cytoscpe
zbs_nodessb_Cy <- bind_rows(persons_sb, orgs_sb)
zbs_nodessb_Cy <- zbs_nodessb_Cy %>% mutate(Year = substr(DocId, 5, 8))
zbs_nodessb_Cy <- zbs_nodessb_Cy %>% select(Type, Text, Year)
zbs_nodessb_Cy <- unique(zbs_nodessb_Cy)
zbs_edgessb_Cy <- inner_join(persons_sb, orgs_sb, by = "DocId")
zbs_edgessb_Cy <- zbs_edgessb_Cy %>% rename(Name = Text.x)
zbs_edgessb_Cy <- zbs_edgessb_Cy %>% rename(Institution = Text.y)
zbs_edgessb_Cy <- zbs_edgessb_Cy %>% mutate(Year = substr(DocId, 5, 8))
zbs_edgessb_Cy <- zbs_edgessb_Cy %>% select(Name, Institution, Year)
zbs_edgessb_Cy <- unique(zbs_edgessb_Cy)
write_csv(zbs_edgessb_Cy, "zbs_edgessb_Cy.csv")
write_csv(zbs_nodessb_Cy, "zbs_nodessb_Cy.csv")
# Both the node and edge list have a year column to be used to filter datasets for each period
# Then the Year column can be remove from the node list
# I will keep the Year column in the edge list to examine the temporal evolution of each network
# Create period files for nodes
zbs_ndsb_Cy1 <- zbs_nodessb_Cy %>% filter(Year <1900) # 11 years
zbs_ndsb_Cy2 <- zbs_nodessb_Cy %>% filter(Year >1899 & Year <1913) # 12 years
zbs_ndsb_Cy3 <- zbs_nodessb_Cy %>% filter(Year >1912 & Year <1919) # 5 years
zbs_ndsb_Cy4 <- zbs_nodessb_Cy %>% filter(Year >1918 & Year <1927) # 7 years
zbs_ndsb_Cy5 <- zbs_nodessb_Cy %>% filter(Year >1926) # 22 years
# Remove the Year column
zbs_ndsb_Cy1 <- zbs_ndsb_Cy1 %>% select(-Year)
zbs_ndsb_Cy2 <- zbs_ndsb_Cy2 %>% select(-Year)
zbs_ndsb_Cy3 <- zbs_ndsb_Cy3 %>% select(-Year)
zbs_ndsb_Cy4 <- zbs_ndsb_Cy4 %>% select(-Year)
zbs_ndsb_Cy5 <- zbs_ndsb_Cy5 %>% select(-Year)

# Save files
write_csv(zbs_ndsb_Cy1, "zbs_ndsb_Cy1.csv")
write_csv(zbs_ndsb_Cy2, "zbs_ndsb_Cy2.csv")
write_csv(zbs_ndsb_Cy3, "zbs_ndsb_Cy3.csv")
write_csv(zbs_ndsb_Cy4, "zbs_ndsb_Cy4.csv")
write_csv(zbs_ndsb_Cy5, "zbs_ndsb_Cy5.csv")


# Create period files for edges
zbs_edg_Cy1 <- zbs_edgessb_Cy %>% filter(Year <1900)
zbs_edg_Cy2 <- zbs_edgessb_Cy %>% filter(Year >1899 & Year <1913)
zbs_edg_Cy3 <- zbs_edgessb_Cy %>% filter(Year >1912 & Year <1919)
zbs_edg_Cy4 <- zbs_edgessb_Cy %>% filter(Year >1918 & Year <1927)
zbs_edg_Cy5 <- zbs_edgessb_Cy %>% filter(Year >1926)
# Save files
write_csv(zbs_edg_Cy1, "zbs_edg_Cy1.csv")
write_csv(zbs_edg_Cy2, "zbs_edg_Cy2.csv")
write_csv(zbs_edg_Cy3, "zbs_edg_Cy3.csv")
write_csv(zbs_edg_Cy4, "zbs_edg_Cy4.csv")
write_csv(zbs_edg_Cy5, "zbs_edg_Cy5.csv")
write_csv(zbs_edg_Cy6, "zbs_edg_Cy6.csv")

# save objects in .RData file
save.image('zbssb.RData')
### END CYTOSCAPE FILE PREPARATION



# Step 1: Join the two dataframes on DocId to create edges between PERSON and ORG within the same document
edge_sb <- persons_sb %>%
  inner_join(orgs_sb, by = "DocId") %>%
  select(Source = Text.x, Target = Text.y) %>%
  distinct()

# Step 2: Create node list
node_sb <- clean_sb2 %>%
  select(Text, Type) %>%
  distinct() %>%
  rename(id = Text)

write_csv(node_sb, "node_sb.csv")
write_csv(edge_sb, "edge_sb.csv")

save.image('zbssb.RData')

# Step 3 below is optional. Use it only if at Step 4 igraph detects "duplicate vertex names"
# Check names that are present as both PERS and ORG
duplicates <- intersect(edge_sb$Source, edge_sb$Target)
duplicates <- as.data.frame(duplicates)
duplicates <- duplicates %>% rename(id = duplicates)
goodnode <- anti_join(node_sb, duplicates)
write.csv(goodnode, "goodnode.csv")
badnode <- inner_join(node_sb, duplicates)
write.csv(badnode, "badnode.csv")

node_sb2 <- bind_rows(goodnode, badnode_Ed)
node_sb2 <- unique(node_sb2)
node_sb2cnt <- node_sb2 %>% group_by(id) %>% count()

write_csv(node_sb2, "node_sb2.csv")

# If the number of "duplicate vertex names" is large, the script above may not suffice
# For a more elaborate script, see the 1-zbs_sb script in the yyq project

# Step 4: Construct network with igraph
ig <- graph_from_data_frame(d = edge_sb, vertices = node_sb, directed = FALSE)

# Check Duplicate vertex names (= Duplicate node names)
intersect(edge_sb$Source, edge_sb$Target)
# If you happen to have a limited number of Duplicate vertex names, save the Intersect data
# Download the node_sb file and correct manually
# Upload the node_sb_Ed file and re-run the script 


# The script that follows unfolds the step to create various types of networks from the whole dataset
# If the dataset is very large, there is probably little interest in pursuing visualization through igraph
# igraph visualization are quite poor visually and can only serve as very preliminary exploration
# The real value of processing the dataset with igraph is to calculate metrics
# It is also possible to export the igraph networks in graphml format for direct use in Cytoscape

# Export of the ig graph for Cytoscape
write_graph(ig, "zbsig.graphml", format = "graphml")

# Add other node attributes from 'zbs_sbconc_nodes'
# For example, if there's a 'type' attribute in your nodes data frame
V(ig)$type <- node_sb$Type[match(V(ig)$name, node_sb$id)]

# index nodes shape/color on nodes type 
V(ig)[node_sb$Type == "PERSON"]$shape <- "circle"
V(ig)[node_sb$Type == "ORG"]$shape <- "square"
V(ig)[node_sb$Type == "PERSON"]$color <- "red"
V(ig)[node_sb$Type == "ORG"]$color <- "light blue"

# plot with igraph 
plot.igraph(ig, vertex.size = 3, 
            vertex.label.color = "black", 
            vertex.label.cex = 0.3, 
            main="ZBS Affiliation network in Shenbao")

# convert into a tidy graph object
tg <- tidygraph::as_tbl_graph(ig) %>% 
  activate(nodes) %>% 
  mutate(label=name) 

# project in padagraph
tg %>% histtext::in_padagraph("SBNetwork") 

# get the URL 
tg %>% histtext::get_padagraph_url("SBNetwork")


# Analyze the two-mode network
# Two methods to obtain the basic parameters of the network
# Order = number of nodes (nodes)
# Size = number of edges (links = ties)
# Diameter = the length of the longest geodesic 
# Average Distance = the average length between nodes

ig
summary(ig)
#  IGRAPH 4c04746 UN-B 34657 284427 -- 

diameter(ig)
# [1] 14
average.path.length(ig)
# [1] 3.639918

#  Measures of Interconnectedness
## Density: How densely connected are the nodes in these networks (two methods)
graph.density(ig) # [1] 0.0004736215
edge_density(ig) # [1] 0.0004736215
## Average Degree : How many ties do the nodes in these networks have, on average? 
mean(degree(ig)) # [1] 16.41383

## Number and size of components: to understand the connectivity structure of the graph ig:

# The 'no.clusters' function returns the number of connected components in the graph ig. 
no.clusters(ig)
# 2444

# The clusters(ig) function computes the connected components of the graph
# It returns an object that includes various information about these components
clusters(ig)$csize
# There is giant component: 31879 and one at 10. All the other are at 1.


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
# If node_sb was created or related to the original graph ig, its indices or identifiers might not directly correspond to those in main_component. 
# You need to ensure that the filtering or matching criteria are valid for the vertices in main_component.
# Assuming 'name' is a vertex attribute in both 'ig' and 'main_component'
# and 'node_sb' contains a matching identifier in a column named 'id' or similar


# Set default shapes and colors for all vertices
V(main_component)$shape <- "circle" # Default shape
V(main_component)$color <- "red"   # Default color

# Specifically update shapes and colors for PERSON and ORG types
# Ensure 'name' is the correct vertex attribute that matches identifiers in 'node_sb$ID'
person_ids <- node_sb$ID[node_sb$Type == "PERSON"]
org_ids <- node_sb$ID[node_sb$Type == "ORG"]

# Update shapes and colors based on matching identifiers
V(main_component)$shape[V(main_component)$name %in% person_ids] <- "circle"
V(main_component)$color[V(main_component)$name %in% person_ids] <- "red"
V(main_component)$shape[V(main_component)$name %in% org_ids] <- "square"
V(main_component)$color[V(main_component)$name %in% org_ids] <- "light blue"

# plot with igraph 
plot.igraph(main_component, vertex.size = 3, 
            vertex.label.color = "black", 
            vertex.label.cex = 0.3, 
            main="ZBS Affiliation Network in Shenbao")


### CUT POINTS on Main Component 
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
# The operation is the same as in the script above, except that it applies to colors

V(main_component)$color= ifelse(V(main_component) %in%
                                  articulation_points(main_component),   
                                "red","orange")

plot(main_component, vertex.label.color = "black", 
     vertex.label.cex = 0.3, 
     vertex.color = V(main_component)$color, 
     vertex.shape = V(main_component)$shape, 
     vertex.size =5, 
     main = "ZBS Affiliation Network Cutpoints in Shenbao",
     sub = "Red squares refer to cutpoints")

# Remove self loops and multiple edges
g1mc <- simplify(main_component)
# remove self loops only
g2mc <- simplify(main_component, remove.loops = TRUE) 
# Explanation
# The function is_simple(main_component) in R's igraph library checks whether the provided graph main_component is a simple graph
# A simple graph in graph theory has the following properties:
# No loops: There are no edges that connect nodes to themselves.
# No multiple edges: There are no duplicate edges; i.e., there is at most one edge between any pair of nodes.
# The is_simple(main_component) function returns a logical value: TRUE if main_component is a simple graph; FALSE if main_component is not a simple graph

is_simple(main_component)
is_simple(g1mc)
is_simple(g2)

V(g1mc)$color= ifelse(V(g1mc) %in%
                                  articulation_points(g1mc),   
                                "red","orange")

plot(g1mc, vertex.label.color = "black", 
     vertex.label.cex = 0.3, 
     vertex.color = V(g1mc)$color, 
     vertex.shape = V(g1mc)$shape, 
     vertex.size =5, 
     main = "ZBS Affiliation Network Cutpoints in Shenbao (neat)",
     sub = "Red squares refer to cutpoints")

# Plot with additional visual attributes for large networks
plot(g1mc, vertex.size = 6, vertex.color = "tomato", vertex.frame.color = NA, vertex.label = NA, edge.curved = .1, edge.arrow.size = .3, edge.width = .7)

save.image('zbssb.RData')

### LOCAL METRICS (main component)

mcDegree <- degree(g1mc) # degree centrality (number of edges)
mcDegree_norm <- degree(g1mc, normalized = TRUE) # number of edges divided by total number of possible edges
mcEig <- evcent(g1mc)$vector # eigenvector
mcBetw <- betweenness(g1mc) # betweenness
mcClose <- closeness(g1mc)  # closeness

mccentralities <- cbind(mcDegree, mcDegree_norm, mcEig, mcBetw, mcClose) # compile
mccentralities_df <- as.data.frame(mccentralities) # convert into dataframe
mccentralities_df_labels <- tibble::rownames_to_column(mccentralities_df, "id") # transform row names into column 
mccentralities_attributes <- inner_join(node_sb, mccentralities_df_labels)

# Visualize centralities

# Make nodes size proportionate to degree centrality
plot(g1mc,
     vertex.size = mcDegree*0.5,
     vertex.label.color = "black", 
     vertex.label.cex = V(g1)$size*0.025, 
     main="ZBS Affiliation Network in Shenbao",
     sub = "Node size represents degree centrality")


# Make nodes size proportionate to eigenvector centrality
plot(g1mc,
     vertex.size = mcEig*20,
     vertex.label.color = "black", 
     vertex.label.cex = Eig, 
     main="ZBS Affiliation Network in Shenbao",
     sub = "Node size represents eigenvector centrality")

# nodes size proportionate to betweenness centrality
plot(g1mc,
     vertex.size = mcBetw*0.002,
     vertex.label.color = "black", 
     vertex.label=NA,
     main="ZBS Affiliation Network in Shenbao",
     sub = "Node size represents betweenness centrality")

## Change network layout and curve edges

# Breakdown explanation for the first line with "kk"
# the function layout_with_kk() computes the layout for a graph using the Kamada-Kawai algorithm for force-directed placement
# kk: This is the variable in which the result of the layout_with_kk() function will be stored
# layout_with_kk() is the function from the igraph package that implements the Kamada-Kawai layout algorithm

# Explanation: The Kamada-Kawai layout algorithm aims to position nodes in a graph in two-dimensional space 
# so that the distances between the nodes are as close as possible to the graph-theoretical distances (e.g., shortest path distances in the graph)
# The idea is to make the layout reflect the structure of the graph as accurately as possible.

kk <- layout_with_kk(g1mc)
nice <- layout_nicely(g1mc)
tree <- layout_as_tree(g1mc)
random <- layout_randomly(g1mc)

# Explanation:
# V(g1mc): This retrieves the vertex sequence (all nodes) of the graph g1mc
# [node_list$Type == "PERSON"]: selects only those nodes for which the corresponding entry in node_list$Type is equal to "PERSON" 
# $shape: this accesses the shape attribute of the nodes that have been filtered by the previous condition
# <- "circle": This assigns the value "circle" to the shape attribute of all the nodes that meet the condition (i.e., those nodes that represent persons)

V(g1mc)[node_sb$Type == "PERSON"]$shape <- "circle"
V(g1mc)[node_sb$Type == "ORG"]$shape <- "square"
V(g1mc)[node_sb$Type == "PERSON"]$color <- "red"
V(g1mc)[node_sb$Type == "ORG"]$color <- "light blue"

# Set default shapes and colors for all vertices
V(g1mc)$shape <- "circle" # Default shape
V(g1mc)$color <- "red"   # Default color

# Specifically update shapes and colors for PERSON and ORG types
# Ensure 'name' is the correct vertex attribute that matches identifiers in 'node_sb$ID'
person_ids <- node_sb$ID[node_sb$Type == "PERSON"]
org_ids <- node_sb$ID[node_sb$Type == "ORG"]

# Update shapes and colors based on matching identifiers
V(g1mc)$shape[V(g1mc)$name %in% person_ids] <- "circle"
V(g1mc)$color[V(g1mc)$name %in% person_ids] <- "red"
V(g1mc)$shape[V(g1mc)$name %in% org_ids] <- "square"
V(g1mc)$color[V(g1mc)$name %in% org_ids] <- "light blue"

plot(g1mc, vertex.label.color = "black", 
     vertex.label.cex = 0.3, 
     vertex.color = V(g1mc)$color, 
     vertex.shape = V(g1mc)$shape, 
     vertex.size =5, edge.curved=0.1, layout=kk, 
     main = "ZBS Affiliation Network in Shenbao (kk)",
     sub = "Red circles= PERS, blue squares= ORG")

plot(g1mc, vertex.label.color = "black", 
     vertex.label.cex = 0.3, 
     vertex.color = V(g1mc)$color, 
     vertex.shape = V(g1mc)$shape, 
     vertex.size =5, edge.curved=0.1, layout=nice, 
     main = "ZBS Affiliation Network in Shenbao (nice)",
     sub = "Red circles= PERS, blue squares= ORG")

plot(g1mc, vertex.label.color = "black", 
     vertex.label.cex = 0.3, 
     vertex.color = V(g1mc)$color, 
     vertex.shape = V(g1mc)$shape, 
     vertex.size =5, edge.curved=0.1, layout=tree, 
     main = "ZBS Affiliation Network in Shenbao (tree)",
     sub = "Red circles= PERS, blue squares= ORG")

plot(g1mc, vertex.label.color = "black", 
     vertex.label.cex = 0.3, 
     vertex.color = V(g1mc)$color, 
     vertex.shape = V(g1mc)$shape, 
     vertex.size =5, edge.curved=0.1, layout=random, 
     main = "ZBS Affiliation Network in Shenbao (random)",
     sub = "Red circles= PERS, blue squares= ORG")

# More layouts here: https://igraph.org/r/doc/layout_.html 

### COMMUNITY DETECTION (main component)

# Subject to relevance, the purpose is to identify subgroups of nodes (persons and organizations) that are more densely connected together. 
# We proceed in four steps:
# We compare various clustering methods and select the most appropriate.
# We analyze the size of communities and their membership
# We extract, visualize and compare the largest communities (their global features)

# list of algorithms available in igraph: https://igraph.org/r/doc/communities.html 

# In this script, we will compare: Louvain, fast greedy, and Girvan-Newman (edge betweeness) (hierarchical clustering)
# We focus on g1mc because some algorithms (fast_greedy) do not work with multiple edges 

# Detect communities
lv <- cluster_louvain(g1mc)
fg <- cluster_fast_greedy(g1mc) 
eb <- cluster_edge_betweenness(g1mc)

ls(lv) # lists all the information packed in the result -> also names()
ls(fg)
ls(eb)
?communities # open the manual page related to communities object

# Inspect results (short summary : number of communities, modularity score, membership)
print(lv)
print(fg) 
print(eb) 
# Communities sizes
sizes(lv) # from 10 to 46
sizes(fg)
sizes(eb)

# Compare sizes of communities in the three clustered networks
hist(sizes(lv))
hist(sizes(fg))
hist(sizes(eb))

# Which nodes are in which group (community)? 
membership(lv)
membership(fg)
membership(eb) 

save.image('zbssb.RData')

# Plot communities 

V(g1mc)$group <- lv$membership # create a group for each community
V(g1mc)$color <- lv$membership # node color reflects group membership (1 cluster = 1 color)

plot(lv, g1mc, vertex.label=V(g1mc)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.3, 
     vertex.size=2,
     main="Communities in ZBS Shenbao network  (Louvain method)",
     sub = "black ties = intra-cluster; red ties = inter-cluster")

V(g1mc)$group <- fg$membership # create a group for each community
V(g1mc)$color <- fg$membership # node color reflects group membership (1 cluster = 1 color)

plot(fg, g1mc, vertex.label=V(g1mc)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.3, 
     vertex.size=2,
     main="Communities in ZBS Shenbao network  (fast greedy method)",
     sub = "black ties = intra-cluster; red ties = inter-cluster")

V(g1mc)$group <- eb$membership # create a group for each community
V(g1mc)$color <- eb$membership # node color reflects group membership (1 cluster = 1 color)

plot(eb, g1mc, vertex.label=V(g1mc)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.3, 
     vertex.size=2,
     main="Communities in ZBS Shenbao network (Girvan-Newman method)",
     sub = "black ties = intra-cluster; red ties = inter-cluster")

#Extract membership data
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

louvain_attribute <- inner_join(louvain, node_sb, by = "id")
fastgreedy_attribute <- inner_join(fastgreedy, node_sb, by = "id")
girman_attribute <- inner_join(girman, node_sb, by = "id")

# Extract communities (focus on Louvain method)
# This will create a subgraph from an existing graph
# lvg1mc <- ...: This part of the code is assigning the result of the operation to the variable lvg1mc.
# induced_subgraph(g1mc, V(g1mc)$group==1): This function call is creating an induced subgraph from the graph g1mc.
# g1mc: This is the original graph from which you want to create a subgraph.
# V(g1mc): This returns the node sequence of g1mc, i.e., all the nodes in g1mc.
# $group: This accesses a node attribute named group. In this context, V(g1mc)$group refers to the group attribute for each node in g1mc.

lvg1mc <- induced_subgraph(g1mc, V(g1mc)$group==1) 
lvg2mc <- induced_subgraph(g1mc, V(g1mc)$group==2) 
lvg3mc <- induced_subgraph(g1mc, V(g1mc)$group==3)

save.image('zbssb.RData')

# Visualize communities separately
V(g1mc)[node_list$Type == "PERSON"]$shape <- "circle"
V(g1mc)[node_list$Type == "ORG"]$shape <- "square"
V(g1mc)[node_list$Type == "PERSON"]$color <- "red"
V(g1mc)[node_list$Type == "ORG"]$color <- "light blue"

plot(lvg1mc, vertex.label=V(lvg1mc)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.7, 
     vertex.size= 8,
     vertex.color = V(lvg1mc)$color, 
     vertex.shape = V(lvg1mc)$shape,
     edge.curved=0.8, 
     main="sb Community 1 ")


plot(lvg2, vertex.label=V(lvg2)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.7, 
     vertex.size= 8,
     vertex.color = V(lvg2)$color, 
     vertex.shape = V(lvg2)$shape,
     edge.curved=0.8, 
     main="sb Community 2 ")

plot(lvg3, vertex.label=V(lvg3)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.7, 
     vertex.size= 8,
     vertex.color = V(lvg3)$color, 
     vertex.shape = V(lvg3)$shape,
     edge.curved=0.8, 
     main="sb Community 3")

# Customize size of node (by degree centrality)
plot(lvg2, vertex.label=V(lvg2)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.7, 
     vertex.size= degree(lvg2)*2, # node size proportionate to node degree (in cluster)
     vertex.color = V(lvg2)$color, 
     vertex.shape = V(lvg2)$shape,
     edge.curved=0.8, 
     main="sb Community 2 ")

plot(lvg2, vertex.label=V(lvg2)$id,
     vertex.label.color = "black", 
     vertex.label.cex = 0.7, 
     vertex.size= degree(g1mc)*0.5, # node size proportionate to node degree (in the whole network)
     vertex.color = V(lvg2)$color, 
     vertex.shape = V(lvg2)$shape,
     edge.curved=0.8, 
     main="sb Community 2 ")

# Extract and compile global metrics to compare the structures of communities 
order1 <- gorder(lvg1mc)
order2 <- gorder(lvg2)
order3 <- gorder(lvg3)

order <- c(order1, order2, order3)
order

size1 <- gsize(lvg1mc)
size2 <- gsize(lvg2)
size3 <- gsize(lvg3)

size <- c(size1, size2, size3)
size

diameter1 <- diameter(lvg1mc)
diameter2 <- diameter(lvg2)
diameter3 <- diameter(lvg3)

diameter <- c(diameter1, diameter2, diameter3)
diameter

edge_density1 <- edge_density(lvg1mc)
edge_density2 <- edge_density(lvg2)
edge_density3 <- edge_density(lvg3)

edge_density <- c(edge_density1, edge_density2, edge_density3)
edge_density

# Compile 
louvain_metrics <- cbind(order, size, diameter, edge_density) 

louvain_metrics_df <- as.data.frame(louvain_metrics) # convert into dataframe
louvain_metrics_df_labels <- tibble::rownames_to_column(louvain_metrics_df, "cluster") # transform row names into column 


### ONE-MODE NETWORK
## From affiliation network to one-mode network

# 1. Build a one-mode network (person-person through documents) 
# Project the two mode-network into a one-mode network linking persons to persons through documents. 
# First we create an edge list in the form of a table linking the source person (from) to the target person (to) - which is the standard format for igraph object
zbs_sbconc_perdata <- persons_sb %>% 
  select(DocId, Text)

edges_sbconc_perdata <- inner_join(zbs_sbconc_perdata, zbs_sbconc_perdata, by = "DocId") %>%
  filter(Text.x < Text.y) %>%
  transmute(from=Text.x, to=Text.y) %>%
  distinct()

edges_sbconc_perdata %>% arrange(from, to)

# The inner_join() function joins the table with itself through DocID. It creates a link for each couple of relation. "Distinct()" is used to eliminate duplicates in documents. 
# Next we create the one-mode network Person-to-person using igraph and tidygraph:

edges__zbssbconc_perdata_tg <- edges_sbconc_perdata %>% transmute(from=from, to=to) 
ig__zbssbconc_perdata <- graph_from_data_frame(d=edges__zbssbconc_perdata_tg, vertices=NULL, directed = FALSE)
tg__zbssbconc_perdata <- tidygraph::as_tbl_graph(ig__zbssbconc_perdata)

# Plot with igraph 
plot.igraph(ig__zbssbconc_perdata, vertex.size = 3, 
            vertex.label.color = "black", 
            vertex.label.cex = 0.9, 
            main="ZBS Person-Document Network in Shenbao")


# 2. Build a one-mode network that link persons through organizations

# Create a bipartite graph
g_bipartite <- graph_from_data_frame(edge_sb, directed = FALSE)

V(g_bipartite)$type <- ifelse(V(g_bipartite)$name %in% edge_sb$Source, FALSE, TRUE)

# Project onto one mode (persons)
proj <- bipartite_projection(g_bipartite)
g_one_mode_person <- proj[[1]]

# Check graph type
class(g_one_mode_person)

# Plot network
plot(g_one_mode_person,
     vertex.size=5,       # Adjust the size of the vertices
     vertex.label.cex=0.7, # Adjust the size of the vertex labels for readability
     main="ZBS Person-to-Person Network in Shenbao", # Title of the plot
     asp=0)               # Keep the aspect ratio at 0 to prevent distortion

# Create a layout
layout <- layout_with_fr(g_one_mode_person) # Fruchterman-Reingold layout

# If you have assigned groups or any attribute to nodes and want to color them based on this
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
save.image('zbssb.RData')

