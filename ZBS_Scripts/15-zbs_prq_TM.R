# First we load the necessary packages 

# Load packages
library(histtext)
library(tidyverse)
library(stm)
library(stminsights)

# we start from the corpus we previously built

# save objects in .RData file
save.image('zbsprstm.RData')
# export file

# Re-upload saved RData file
load(file = "zbsprstm.RData")


# The workflow follows steps 
# 1 pre-process the text data
# 2 build the model(s)
# 3 explore topics
# 4 co-variate influence (time, publisher)

# preprocess

meta <- zbs_proq_ft_sort2 %>% transmute(DocId, Year, Source, Date) 

corpus <- stm::textProcessor(zbs_proq_ft_sort2$Text,
                             metadata = meta, 
                             stem = FALSE, 
                             wordLengths = c(4, Inf), 
                             verbose = FALSE, 
                             customstopwords = c("miss", "messrs", "will", "said", "chinese"))

words <- as_tibble(corpus$vocab) 

stm::plotRemoved(corpus$documents, lower.thresh = c(0,10, by=5))


out <- stm::prepDocuments(corpus$documents, 
                          corpus$vocab, 
                          corpus$meta, 
                          lower.thresh = 5)

wordsremoved <- as_tibble(out$words.removed) 

# Removing 28495 of 34443 terms (43630 of 197596 tokens) due to frequency 
# Your corpus now has 547 documents, 5948 terms and 153966 tokens.

set.seed(1111)
K<-seq(5,15, by=5) 
kresult <- searchK(out$documents, out$vocab, K,  data=out$meta, prevalence =~ Year + Source + Date, verbose=FALSE)
plot(kresult)


# 5-topic model
mod.5 <- stm::stm(out$documents, 
                  out$vocab, K=5, 
                  data=out$meta, 
                  prevalence =~ Year + Source + Date, 
                  verbose = FALSE)

# 10-topic model
mod.10 <- stm::stm(out$documents, 
                   out$vocab, K=10, 
                   data=out$meta, 
                   prevalence =~ Year + Source + Date, 
                   verbose = FALSE)

# 15-topic model
mod.15 <- stm::stm(out$documents, 
                   out$vocab, K=15, 
                   data=out$meta, 
                   prevalence =~ Year + Source + Date,  
                   verbose = FALSE)

# estimate time effect (year)

year5 <- stm::estimateEffect(1:5 ~ Year, mod.5, meta=out$meta)
year10 <- stm::estimateEffect(1:10 ~ Year, mod.10, meta=out$meta)
year15 <- stm::estimateEffect(1:15 ~ Year, mod.15, meta=out$meta)

# estimate source effect (publisher)

source5 <- stm::estimateEffect(1:5 ~ Source, mod.5, meta=out$meta)
source10 <- stm::estimateEffect(1:10 ~ Source, mod.10, meta=out$meta)
source15 <- stm::estimateEffect(1:15 ~ Source, mod.15, meta=out$meta)


# save models 
save.image('zbsprstm.RData')


# explore topics 

# use stm insights 

library(stminsights)
run_stminsights()

# use stm 

# extract topic proportions 
topicprop5<-make.dt(mod.5, meta)
topicprop10<-make.dt(mod.10, meta)
topicprop15<-make.dt(mod.15, meta)

# plot topic proportions per document

plot.STM(mod.5, "hist")
plot.STM(mod.10, "hist")
plot.STM(mod.15, "hist")

# Word per topic (summary)

plot.STM(mod.5,"summary", n=5)
plot.STM(mod.10,"summary", n=5)
plot.STM(mod.15,"summary", n=5)

# We could remove "will" and "said"

# label topics (word frequencies for each topic)

labelTopics(mod.5, n=10)
labelTopics(mod.10, n=10)
labelTopics(mod.15, n=10)

# word clouds

cloud(mod.5, topic = 4, scale = c(4, 0.4))
cloud(mod.5, topic = 5, scale = c(3, 0.3))
cloud(mod.5, topic = 1, scale = c(4, 0.4))
cloud(mod.5, topic = 2, scale = c(4, 0.4))
cloud(mod.5, topic = 3, scale = c(4, 0.4))


cloud(mod.10, topic = 6, scale = c(4, 0.4))
cloud(mod.10, topic = 8, scale = c(4, 0.4))
cloud(mod.10, topic = 9, scale = c(4, 0.4))
cloud(mod.10, topic = 7, scale = c(4, 0.4))
cloud(mod.10, topic = 10, scale = c(4, 0.4))

# quotations

par(mfrow=c(1,1))

T10_thoughts4 <- findThoughts(mod.10,texts=nrc_clean$fulltext, topics=4, n=5)$docs[[1]]
T10_thoughts8 <- findThoughts(mod.10,texts=nrc_clean$fulltext, topics=8, n=5)$docs[[1]]
T10_thoughts9 <- findThoughts(mod.10,texts=nrc_clean$fulltext, topics=9, n=5)$docs[[1]]
T10_thoughts7 <- findThoughts(mod.10,texts=nrc_clean$fulltext, topics=7, n=5)$docs[[1]]
T10_thoughts5 <- findThoughts(mod.10,texts=nrc_clean$fulltext, topics=5, n=5)$docs[[1]]
T5_thoughts1 <- findThoughts(mod.5,texts=zbs_proq_ft_sort2$Text, topics=1, n=5)$docs[[1]]
T5_thoughts2 <- findThoughts(mod.5,texts=zbs_proq_ft_sort2$Text, topics=2, n=5)$docs[[1]]
T10_thoughts6 <- findThoughts(mod.10,texts=nrc_clean$fulltext, topics=6, n=5)$docs[[1]]
T5_thoughts3 <- findThoughts(mod.5,texts=zbs_proq_ft_sort2$Text, topics=3, n=5)$docs[[1]]
T10_thoughts10 <- findThoughts(mod.10,texts=nrc_clean$fulltext, topics=10, n=5)$docs[[1]]

par(mfrow=c(1,3), mar=c(1,1,2,2))
plotQuote(T5_thoughts1, width=50, maxwidth=400, text.cex=0.8, main="Topic 4")
plotQuote(T5_thoughts2, width=50, maxwidth=400, text.cex=0.8, main="Topic 5")
plotQuote(T5_thoughts3, width=50, maxwidth=400, text.cex=0.8, main="Topic 9")

# interactive visualization (LDA Viz)

stm::toLDAvis(mod.5, doc=out$documents)
stm::toLDAvis(mod.10, doc=out$documents)
stm::toLDAvis(mod.15, doc=out$documents)

### TOPIC PROPORTIONS OVER TIME 

# select topic proportions
topic5prop <- topicprop5 %>% select(c(2:6))
topic10prop <- topicprop10 %>% select(c(2:11))
topic15prop <- topicprop15 %>% select(c(2:16))

# Compute topic proportions per year

topic_proportion_per_year5 <- aggregate(topic5prop, by = list(Year = meta$Year), mean)
topic_proportion_per_year10 <- aggregate(topic10prop, by = list(Year = meta$Year), mean)
topic_proportion_per_year15 <- aggregate(topic15prop, by = list(Year = meta$Year), mean)

# Reshape data frame

library(reshape)
vizDataFrame5y <- melt(topic_proportion_per_year5, id.vars = "Year")
vizDataFrame10y <- melt(topic_proportion_per_year10, id.vars = "Year")
vizDataFrame15y <- melt(topic_proportion_per_year15, id.vars = "Year")

# Plot topic proportions per year as bar plots:

library(pals)
# 5-topic model: 
ggplot(vizDataFrame5y, aes(x=Year, y=value, fill=variable)) + 
  geom_bar(stat = "identity") + ylab("proportion") + 
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name = "Topic") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="The NRC in the English-language press", 
       subtitle = "Topic proportion over time (5-topic model)")

# 10-topic model:
ggplot(vizDataFrame10y, aes(x=Year, y=value, fill=variable)) + 
  geom_bar(stat = "identity") + ylab("proportion") + 
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name = "Topic") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="The NRC in the English-language press", 
       subtitle = "Topic proportion over time (10-topic)")

# 15-topic model:
ggplot(vizDataFrame15y, aes(x=Year, y=value, fill=variable)) + 
  geom_bar(stat = "identity") + ylab("proportion") + 
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name = "Topic") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title="The NRC in the English-language press", 
       subtitle = "Topic proportion over time (15-topic)")


# save models 
save.image('zbsprstm.RData')
