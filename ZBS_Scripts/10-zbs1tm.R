
# Topic modeling

# Based on the distribution of articles over time, I define 4 periods in ZBS's life
#1 1888-1899
#2 1900-1912
#3 1913-1918
#4 1919-1926
#5 1927-1949

# Load packages
library(histtext)
library(tidyverse)
library(stm)
library(stminsights)

# save objects in .RData file
save.image('zbstm1.RData')

# Re-upload saved RData file
load(file = "zbstm1.RData")

# Tokenize the texts

# first remove non sinograms (punctuation, symbols)
zbs_p1 <- zbs_p1 %>% mutate(text_clean = str_replace_all(Text, "[^[\\p{Han}]]", " "))

# Remove blank spaces that remain
zbs_p1$text_clean <- str_squish(zbs_p1$text_clean) 

# Add year
zbs_p1 <- zbs_p1 %>%
  mutate(Date = ymd(Date)) %>%
  mutate(Year = year(Date))

# next tokenize : 

zbs1_tok <- cws_on_df(zbs_p1, 
                      text_column = "text_clean",
                      id_column = "DocId",
                      model = "trftc_shunpao_23:zh:cws",
                      detailed_output = FALSE,
                      token_separator = " ",
                      verbose = TRUE
)


# Save file
write_csv(zbs1_tok, "zbs1_tok.csv")


# save objects in .RData file
save.image('zbstm1.RData')

# remove noisy outputs 
zbs1_tok_filt <- zbs1_tok %>% filter(stringr::str_detect(zbs1_tok$Text, "[\\p{Han}]"))

# Select year column in zbs_p1
zbs_p1_year <- zbs_p1 %>% select(DocId, Year)

# Add Year to zbs_tok_filt
zbs1_tok_filt <- left_join(zbs1_tok_filt, zbs_p1_year)
zbs1_tok_filt <- unique(zbs1_tok_filt)

# Remove blank spaces that remain
zbs1_tok_filt$Text <- str_squish(zbs1_tok_filt$Text) 

# Save file
write_csv(zbs1_tok, "zbs1_tok.csv")
write_csv(zbs1_tok_filt, "zbs1_tok_filt.csv")


save.image('zbstm1.RData')

# preprocess

# join tokenized text with metatada 

zbs_doc <- zbs1_tok_filt %>% select(DocId, Year) 
zbs1_tok2 <- inner_join(zbs_doc, zbs1_tok_filt)
zbs1_tok2 <- unique(zbs1_tok2)


zbs1_tok3 <- zbs1_tok2 %>%
  mutate(Text = str_remove_all(Text, "朱葆三")) %>%
  mutate(Text = str_remove_all(Text, "朱佩珍")) %>%
  mutate(Text = str_remove_all(Text, "先生")) %>%
  mutate(Text = str_remove_all(Text, "昨日")) %>%
  mutate(Text = str_remove_all(Text, "無名氏")) %>%
  mutate(Text = str_remove_all(Text, "[零一二三四五六七八九十百千]+")) %>%
  mutate(Text = str_replace_all(Text, "本公司", "公司"))  %>%
  mutate(Text = str_replace_all(Text, "本銀行", "銀行"))

# create list of stopwords 
library(stopwords)
# https://stopwords.quanteda.io/
# list all sources 
stopwords::stopwords_getsources()
# list languages for a specific source: 3 sources for Chinese
stopwords::stopwords_getlanguages("marimo") # two Chinese: zh_tw (taiwan) and zh_cn
stopwords::stopwords_getlanguages("stopwords-iso")
stopwords::stopwords_getlanguages("misc")
# extract stop words from different dictionaries 
zh_iso_stopwords <- stopwords(language = "zh", source = "stopwords-iso")
zh_marimo_tw <- stopwords(language = "zh_tw", source = "marimo")
zh_misc <- stopwords(language = "zh", source = "misc")
stop_iso <- as.data.frame(zh_iso_stopwords) 
stop_iso <- stop_iso %>% mutate(word = zh_iso_stopwords)
stop_iso$zh_iso_stopwords <- NULL
stop_marimo <- as.data.frame(zh_marimo_tw) 
stop_marimo <- stop_marimo %>% mutate(word = zh_marimo_tw)
stop_marimo$zh_marimo_tw <- NULL
stop_misc <- as.data.frame(zh_misc) 
stop_misc <- stop_misc %>% mutate(word = zh_misc)
stop_misc$zh_misc <- NULL

# Combine all stop words
stop_all <- bind_rows(stop_iso, stop_marimo, stop_misc) %>% unique()
# Create a regex pattern from the stopwords
stop_pattern <- paste(stop_all$word, collapse = "|")
# Remove stopwords using the combined pattern
zbs1_tok3 <- zbs1_tok3 %>% mutate(Text = str_remove_all(Text, stop_pattern))

# Remove additional stopwords
zbs1_tok3 <- zbs1_tok3 %>% mutate(Text = (str_remove_all(Text, "以及|本會|二馬路|第|一路|個|時間|其中|研究|部分|主要|分析|影响|方面|的|了|在|和|是|為|与|中|日|年|以|等|人|也|上|他們|有|于|之|而|但|由|所|並|到|下|成|及|多|地|者|此|作|被|来|向|就|着|都|具|還|更|起|生|化|其|文|用|大|末|出|海|他|路|名|物|月|王|社|本|时|同|位|期|不|事|工|因|比較|介紹|第|命|性|近代|全面|背景|獨特|原|租|族|两|代|做|先|共|前|北|参|又|字|定|方|著|過|道|部")))


write_csv(zbs1_tok3, "zbs1_tok3.csv")


# save objects in .RData file
save.image('zbstm1.RData')


#### build the topic model

meta <- zbs1_tok3 %>% transmute(DocId, Year)

corpus <- stm::textProcessor(zbs1_tok3$Text,
                             metadata = meta, 
                             stem = FALSE, 
                             wordLengths = c(2, Inf), 
                             verbose = FALSE, 
                             customstopwords = stop_all$word)

words <- as_tibble(corpus$vocab) 

stm::plotRemoved(corpus$documents, lower.thresh = c(0,10, by=5))


out <- stm::prepDocuments(corpus$documents, 
                          corpus$vocab, 
                          corpus$meta, 
                          lower.thresh = 2)

# Removing  2655 of 2933 terms (2948 of 4763 tokens) due to frequency 
# Your corpus now has 125 documents, 278 terms and 1815  tokens


# inspect words and docs removed
wordsremoved <- as_tibble(out$words.removed) 
docremoved <- as_tibble(out$docs.removed) 

set.seed(1111)
K<-seq(5,15, by=5) 
kresult <- searchK(out$documents, out$vocab, K,  data=out$meta, prevalence =~ Year, verbose=FALSE)
plot(kresult)


# save objects in .RData file
save.image('zbstm1.RData')

# Interpretation:

# The Held-Out Likelihood suggests 5 & 10 topics is optimal, as the likelihood is maximized here.
# The Residuals suggest fewer topics might be better, as the residuals are lower for 5 topics.
# Semantic Coherence is clearly the highest at 5 topics, indicating strong within-topic word associations.
# The Lower Bound decreases with more topics, suggesting diminishing returns with more than 6 topics.
# Considering all these diagnostics together, it appears that a topic model with 5 topics might be the best trade-off. It has the highest semantic coherence, which is crucial for interpretability and usefulness of the topics. It also has the lowest residuals, indicating a good fit with the observed data.
# However, choosing the 10-topic model come quite close.


# 5-topic model
mod.5 <- stm::stm(out$documents, 
                  out$vocab, K=5, 
                  data=out$meta, 
                  prevalence =~ Year, 
                  verbose = FALSE)


# 10-topic model
mod.10 <- stm::stm(out$documents, 
                   out$vocab, K=10, 
                   data=out$meta, 
                   prevalence =~ Year, 
                   verbose = FALSE)



# estimate time effect (Year)

Year5 <- stm::estimateEffect(1:5 ~ Year, mod.5, meta=out$meta)
Year10 <- stm::estimateEffect(1:10 ~ Year, mod.10, meta=out$meta)


save.image('zbstm1.RData')


# explore topics 
# use stm 

# extract topic proportions 
topicprop5<-make.dt(mod.5, meta)
topicprop10<-make.dt(mod.10, meta)

write_csv(topicprop5, "zbs1topicprop5.csv")

# plot topic proportions per document

plot.STM(mod.5, "hist")
plot.STM(mod.10, "hist")

# Word per topic (summary)

plot.STM(mod.5,"summary", n=5)
plot.STM(mod.10,"summary", n=10)


# Label topics (word frequencies models for each topic)

labelTopics(mod.5, n=10)
labelTopics(mod.10, n=10)


# word clouds

cloud(mod.5, topic = 1, scale = c(4, 0.4))
cloud(mod.5, topic = 2, scale = c(4, 0.4))
cloud(mod.5, topic = 3, scale = c(4, 0.4))
cloud(mod.5, topic = 4, scale = c(4, 0.4))
cloud(mod.5, topic = 5, scale = c(4, 0.4))
cloud(mod.5, topic = 6, scale = c(5, 0.6))
cloud(mod.10, topic = 1, scale = c(4, 0.4))
cloud(mod.10, topic = 2, scale = c(4, 0.4))
cloud(mod.10, topic = 3, scale = c(4, 0.4))
cloud(mod.10, topic = 4, scale = c(4, 0.4))
cloud(mod.10, topic = 5, scale = c(4, 0.4))
cloud(mod.10, topic = 6, scale = c(4, 0.4))


### TOPIC PROPORTIONS OVER TIME 

# select topic proportions
topic5prop <- topicprop5 %>% select(c(2:6))
topic10prop <- topicprop10 %>% select(c(2:11))


# Compute topic proportions per Year
topic_proportion_per_year5 <- aggregate(topic5prop, by = list(Year = meta$Year), mean)
topic_proportion_per_year10 <- aggregate(topic10prop, by = list(Year = meta$Year), mean)


# Reshape data frame
library(reshape)
vizDataFrame5y <- melt(topic_proportion_per_year5, id.vars = "Year")
vizDataFrame10y <- melt(topic_proportion_per_year10, id.vars = "Year")

save.image('zbstm1.RData')

# Plot topic proportions per year as bar plots:

library(pals)
# 5-topic model: 
ggplot(vizDataFrame5y, aes(x=Year, y=value, fill=variable)) + 
  geom_bar(stat = "identity") + ylab("proportion") + 
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name = "Topic") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="ZBS in the Shenbao (1888-1899)", 
       subtitle = "Topic proportion over time (5-topic model)")

# 10-topic model:
ggplot(vizDataFrame10y, aes(x=Year, y=value, fill=variable)) + 
  geom_bar(stat = "identity") + ylab("proportion") + 
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name = "Topic") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="ZBS in the Shenbao (1888-1899)", 
       subtitle = "Topic proportion over time (10-topic)")


save.image('zbstm1.RData')


# interactive visualization (LDA Viz)

set.seed(1111)
stm::toLDAvis(mod.5, doc=out$documents)
stm::toLDAvis(mod.10, doc=out$documents)

## interactive visualization (stm insights)

library(stminsights)
run_stminsights()
