#Chainsaw Stuff ----
install.packages('installr')
library(installr)
install.Rtools()

install.packages(c('geometry', 'Rtsne', 'rsvd','spacyr',
                   'igraph','tidyverse', 'rio', 'quanteda',
                   'rtweet', 'topicmodels', 'stm','parsedate',
                   'psych','desc','zoo','lubridate','newsmap','dplyr','descr','readtext','ggplot2'
  ))

devtools::install_github("quanteda/quanteda.corpora")

library(dplyr)
library(desc)
library(quanteda)
library(geometry)
library(readtext)
library(quanteda.corpora)
library(rsvd)
library(spacyr)
library(igraph)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(newsmap)
library(descr)
library(psych)
library(parsedate)
library(stm)
library(Rtsne)
library(rio)
library(rtweet)
library(topicmodels)
library(zoo)




# Data Preparation ----

nyt <- read.csv("nyt.csv", encoding = "UTF-8")
guardian <- read.csv("guardian.csv", encoding = "UTF-8")
hkfp <- read.csv("hkfp.csv", encoding = "UTF-8")

nyt_data <- nyt[grepl(c("World","U.S.","Opinion","Business Day","Briefing"), nyt$Section),] %>% 
  subset(select = - c(Section))
nyt_data <- as_tibble(nyt_data)


guardian_data <- subset(guardian, select = - c(Section))
guardian_data <- as_tibble(guardian_data)

hkfp_data <- as_tibble(hkfp)




#hdaily <- as_tibble(headline_data)
#hdaily <- hdaily %>% rename(
 # Title = V1,
  #Link = V2,
  #Date = V3,
  #Text = Articles)

#write.csv(guardian, "guardian.csv")
#write.csv(nyt, "nyt.csv")
#write.csv(hdaily,"hdaily.csv")





# Rewriting Dates ---- 







nyt_data$MonthYear <- as.yearmon(nyt_data$Date)
guardian_data$MonthYear <- as.yearmon(guardian_data$Date)
hkfp_data$MonthYear <- as.yearmon(hkfp_data$Date)

nyt_data$Date <- as.Date(nyt_data$Date)
guardian_data$Date <- as.Date(guardian_data$Date)
hkfp_data$Date <- as.Date(hkfp_data$Date)

nyt_data$Titles <- as.character(nyt_data$Titles)
guardian_data$Titles <- as.character(guardian_data$Titles)
hkfp_data$Titles <- as.character(hkfp_data$Titles)

nyt_data$Article <- as.character(nyt_data$Article)
guardian_data$Article <- as.character(guardian_data$Article)
hkfp_data$Article <- as.character(hkfp_data$Article)


head(nyt_data)
head(guardian_data)
head(hkfp_data)

hkfp_data <- hkfp_data %>% rename(
  Newspaper = Newspaper,
  Titles = Titles,
  Link = Links,
  Date = Dates,
  Article = Articles)

head(news_data)

# Basic Frequency Analysis ----

freq(news_data$Newspaper)
freq(news_data$MonthYear)
freq(news_data$Date)



# Corpus ----
news_data$MonthYear <- as.Date(news_data$MonthYear)

ncorpus <- corpus(news_data,text_field = "Article")
head(docvars(ncorpus))

ncorp_docs <- corpus_reshape(ncorpus, to = "documents")

# Tokens ----
ntok <- tokens(ncorpus, remove_punct = TRUE)
ntok <- tokens_remove(ntok, pattern = stopwords('en'))
ntok <- tokens_remove(ntok, pattern = c("Hong","Kong","KONG","HONG","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday","Free","Press","hong","kong's"))
context_keyword <- kwic(ntok, pattern = "elect*", window = 3)
context_keywords <- kwic(ntok, pattern = c("elect*","victor*"), window = 3)
context_pattern <- kwic(ntok, pattern = phrase('Carrie Lam*'), window = 7)
#Removal of tokens changes the lengths of documents, but they remain the same if you set padding = TRUE. This option is useful especially when you perform positional analysis.
multiword <- kwic(ntok, pattern = phrase(c("Junius Ho","Joshua Wong","Carrie Lam","Xi Jinping")))
head(multiword)
#To preserve these expressions in bag-of-word analysis, you have to compound them using tokens_compound().
toks_comp <- tokens_compound(ntok, pattern = phrase(c("Junius Ho","Joshua Wong","Carrie Lam","Xi Jinping")))
kw_comp <- kwic(toks_comp, pattern = c("Junius_Ho","Joshua_Wong","Carrie_Lam","Xi_Jinping"))
head(kw_comp, 50)
#Can also generate ngrams
toks_ngram <- tokens_ngrams(ntok, n = 2)
head(toks_ngram)
# DFM ----

ndfm <- dfm(ntok)

head(featnames(ndfm))

topfeatures(ndfm)


#You can also select features based on the length of features.
ndfm_long <- dfm_select(ndfm, min_nchar = 5)
nfeat(ndfm_long)
topfeatures(ndfm_long)

#While dfm_select() selects features based on patterns, dfm_trim() does this based on feature frequencies. If min_termfreq = 10, features that occur less than 10 times in the corpus are removed.
ndfm_freq <- dfm_trim(ndfm, min_termfreq = 50)
nfeat(ndfm_freq)
topfeatures(ndfm_freq)

dfm_select(ndfm_freq, min_nchar = 5)

#If max_docfreq = 0.1, features that occur in more than 10% of the documents are removed.
ndfm_docfreq <- dfm_trim(ndfm, max_docfreq = 0.5, docfreq_type = "prop")
nfeat(ndfm_docfreq)
topfeatures(dfm_select(ndfm_docfreq, min_nchar = 5))


# Feature Co-Occurence Matrix ---- 



feat <- fcm(dfm_select(ndfm_freq, min_nchar = 5))
feats <- names(topfeatures(feat, 300))

feat_select <- fcm_select(feat, pattern = feats)

dim(feat_select)


size <- log(colSums(dfm_select(feat, feats)))
set.seed(144)
textplot_network(feat_select, vertex_size = size/max(size)*3)



# Statistical Analysis ---- 

stdfm <- dfm(ntok, remove = stopwords("english"),remove_punct = TRUE) %>% dfm_select(min_nchar = 5)

stdfm %>% 
  textstat_frequency(n = 15) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()

set.seed(132)

textplot_wordcloud(stdfm, max_words = 300, min_count = 20)

# Lexical Diversity ----
summary(ncorpus,5)

tstat_lexdiv <- textstat_lexdiv(ndfm)
tail(tstat_lexdiv, 5)

plot(tstat_lexdiv$TTR, type = 'l', xaxt = 'n', xlab = NULL, ylab = "TTR")
grid()
axis(1, at = seq_len(nrow(tstat_lexdiv)), labels = docvars(ndfm, ('MonthYear')))


# Document/Feature Similarity ----

tstat_dist <- as.dist(textstat_dist(ndfm))
clust <- hclust(tstat_dist)
plot(clust, xlab = "Distance", ylab = NULL)


# Relative Frequency Analysis ----



tstat_key <- textstat_keyness(stdfm, 
                              target = month(docvars(stdfm, 'MonthYear')) <= "2019-12-01")
attr(tstat_key, 'documents') <- c("2020-01-01")

textplot_keyness(tstat_key)

# Chinese Data ----

hdaily2 <- hdaily[order(hdaily$Date),] 
hdaily2$MonthYear <- as.Date(hdaily2$MonthYear)
hdaily2$Date <- as.Date(hdaily2$Date)
hdaily2$Text <- as.character(hdaily2$Text)

CNcorpus <- corpus(hdaily2,text_field = "Text")

#Stopwords
CN_stop <- stopwords("zh", source = "misc")

#Tokenize 
CNtok <- CNcorpus %>% 
  tokens(remove_punct = TRUE) %>%
  tokens_remove(pattern = CN_stop) %>%
  tokens_remove(pattern = c("指","香港","條例","會","時","逃犯","示威者","修訂",
                            "香","港"
  ))
#DFM

CN_dfm <- dfm(CNtok)
topfeatures(CN_dfm)

#Word Cloud

# plot a word cloud
set.seed(100)

# to set the font correctly for macOS
textplot_wordcloud(CN_dfm, min_count = 25, random_order = FALSE,
                   rotation = .25, max_words = 200,
                   min_size = 0.5, max_size = 2.8,
                   font = if (Sys.info()['sysname'] == "Darwin") "SimHei" else NULL,
                   color = RColorBrewer::brewer.pal(8, "Dark2"))



# fcm within the window size of 5
ch10_corp <- corpus_subset(CNcorpus, MonthYear >= "2017-11-01")
ch10_toks <- 
  tokens(ch10_corp, remove_punct = TRUE) %>% 
  tokens_remove(CN_stop)
ch_fcm <- fcm(ch10_toks, context = "window")  
topfeatures(ch_fcm["暴力", ])

# collocations

cn_col <- textstat_collocations(CNtok, size = 2:4, min_count = 15)
knitr::kable(head(cn_col, 10))

# bigrams since october
ch10_col <- textstat_collocations(ch10_toks, size = 2)
knitr::kable(head(ch10_col, 10))

#Document Scaling?

wf <- textmodel_wordfish(CN_dfm)
y <- as.Date("2019-09-01"):as.Date("2019-12-01")
y <- y[y <= "2019-10-01" | y >= "2019-11-01"]
y <- y[!y %in% c("2019-10-01", "2019-10-15", "2019-10-31", "2019-11-01", "2019-11-15","2019-11-30")]
plot(y, wf$theta, xlab = "Month", ylab = "Position")







