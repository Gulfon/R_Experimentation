#Chainsaw Stuff ----
install.packages(c('geometry', 'Rtsne', 'rsvd'))
install.packages(c('tidyverse', 'rio', 'quanteda', 'rtweet', 'topicmodels', 'stm'))
install.packages("parsedate")
library(parsedate)
install.packages(c('spacyr', 'igraph'))
install.packages("psych")
library("psych")
install.packages("desc")
library(dplyr)
library(descr)
require(spacyr)
library(quanteda)
spacy_install()
devtools::install_github("quanteda/quanteda.corpora")
install.packages("newsmap")
require(quanteda)
require(readtext)
require(quanteda.corpora)
require(newsmap)

# Data Preparation ----

nyt <- read.csv("NYTHK.csv", header = FALSE, encoding = "UTF-8")

guardian_data <- subset(guardian_data, select = - c(V1,V3))

nyt_data3 <- subset(nyt_data3, select = - c(V1,V3))
nyt_data3 <- nyt_data2[grepl(c("World","U.S.","Opinion","Business Day","Briefing"), nyt_data2$V3),]


guardian <- as_tibble(guardian_data) 
guardian %>% dplyr::rename(
  Title = V2,
  Link = V4,
  Date = V5,
  Text = V6
)

nyt <- as_tibble(nyt_data3)
nyt <- nyt %>% rename(
  Title = V2,
  Link = V4,
  Date = V5,
  Text = V6)

hdaily <- as_tibble(headline_data)
hdaily <- hdaily %>% rename(
  Title = V1,
  Link = V2,
  Date = V3,
  Text = Articles)

write.csv(guardian, "guardian.csv")
write.csv(nyt, "nyt.csv")
write.csv(hdaily,"hdaily.csv")
# Corpus ----

nyt2 <- nyt 
nyt2$Date <- as.Date(nyt2$Date)
nyt2$Text <- as.character(nyt2$Text)

ncorpus <- corpus(nyt2,text_field = "Text")


head(docvars(ncorpus))


ncorp_docs <- corpus_reshape(ncorpus, to = "sentences")
ncorp_docs <- corpus_reshape(ncorp_docs, to = "documents")

# Tokens ----
ntok <- tokens(ncorp_docs, remove_punct = TRUE)
ntok <- tokens_remove(ntok, pattern = stopwords('en'))
ntok <- tokens_remove(ntok, pattern = c("Hong","Kong","KONG","HONG","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))



context_keyword <- kwic(ntok, pattern = "elect*")
context_keywords <- kwic(ntok, pattern = c("elect*","victor*"), window = 11)
context_pattern <- kwic(ntok, pattern = phrase('Junius Ho*'), window = 7)


#Removal of tokens changes the lengths of documents, but they remain the same if you set padding = TRUE. This option is useful especially when you perform positional analysis.

multiword <- kwic(ntok_nostop, pattern = phrase(c("Junius Ho","Joshua Wong")))
head(multiword)

#To preserve these expressions in bag-of-word analysis, you have to compound them using tokens_compound().

toks_comp <- tokens_compound(ntok_nostop, pattern = phrase(c("Junius Ho","Joshua Wong")))
kw_comp <- kwic(toks_comp, pattern = c("Junius_Ho","Joshua_Wong"))
head(kw_comp, 10)

#Can also generate ngrams
toks_ngram <- tokens_ngrams(ntok_clean, n = 2)
head(toks_ngram)



# DFM ----

nyt_dfm <- dfm(ntok)

head(featnames(nyt_dfm), 20)

topfeatures(nyt_dfm)


#You can also select features based on the length of features.
nyt_dfm_long <- dfm_select(nyt_dfm, min_nchar = 5)
nfeat(nyt_dfm_long)


#While dfm_select() selects features based on patterns, dfm_trim() does this based on feature frequencies. If min_termfreq = 10, features that occur less than 10 times in the corpus are removed.
nyt_dfm_freq <- dfm_trim(nyt_dfm, min_termfreq = 20)
nfeat(nyt_dfm_freq)
head(nyt_dfm_freq)

#If max_docfreq = 0.1, features that occur in more than 10% of the documents are removed.
nyt_dfm_docfreq <- dfm_trim(nyt_dfm, max_docfreq = 0.01, docfreq_type = "prop")
nfeat(nyt_dfm_docfreq)

# Feature Co-Occurence Matrix ---- 

feat <- fcm(nyt_dfm_freq)
feats <- names(topfeatures(feat, 300))

feat_select <- fcm_select(feat, pattern = feats)

dim(feat_select)


size <- log(colSums(dfm_select(feat, feats)))
set.seed(144)
textplot_network(feat_select, vertex_size = size/max(size)*3)



# Statistical Analysis ---- 

nyt_dfm %>% 
  textstat_frequency(n = 15) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()

set.seed(132)

textplot_wordcloud(nyt_dfm, max_words = 100)

# Lexical Diversity ----
summary(ncorp_docs,5)

tstat_lexdiv <- textstat_lexdiv(nyt_dfm)
tail(tstat_lexdiv, 5)

plot(tstat_lexdiv$TTR, type = 'l', xaxt = 'n', xlab = NULL, ylab = "TTR")
grid()
axis(1, at = seq_len(nrow(tstat_lexdiv)), labels = docvars(nyt_dfm, 'Date'))


# Document/Feature Similarity ----

tstat_dist <- as.dist(textstat_dist(nyt_dfm))
clust <- hclust(tstat_dist)
plot(clust, xlab = "Distance", ylab = NULL)


# Relative Frequency Analysis ----

tstat_key <- textstat_keyness(nyt_dfm, 
                              target = docvars(nyt_dfm, 'Date')) >= "2019-09-01"
attr(tstat_key, 'documents') <- c('2019.08', '2019.07','2019.06')

textplot_keyness(tstat_key)