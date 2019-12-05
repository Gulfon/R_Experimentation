#Chainsaw Stuff ----
install.packages(c('geometry', 'Rtsne', 'rsvd'))
install.packages(c('tidyverse', 'rio', 'quanteda', 'rtweet', 'topicmodels', 'stm'))

install.packages(c('spacyr', 'igraph'))
install.packages("psych")
library("psych")
install.packages("desc")
library(dplyr)
library(descr)
require(spacyr)
spacy_install()

# Data Preparation ----
devtools::install_github("quanteda/quanteda.corpora")
install.packages("newsmap")
require(quanteda)
require(readtext)
require(quanteda.corpora)
require(newsmap)
nyt <- read.csv("NYTHK.csv", header = FALSE, encoding = "UTF-8")
names(guardian_data)
guardian_data$V6[1]
headline_data$V4[1]
names(guardian_data)
guardian_data <- subset(guardian_data, select = - c(V1,V3))
nyt_data3 <- subset(nyt_data3, select = - c(V1,V3))
nyt_data2 <- nyt_data
nyt_data3 <- nyt_data2[grepl(c("World","U.S.","Opinion","Business Day","Briefing"), nyt_data2$V3),]
freq(sort(nyt_data$V3))

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

hdaily <- hdaily %>% rename(
  Title = V1,
  Link = V2,
  Date = V3,
  Text = Articles)

write.csv(guardian, "guardian.csv")
write.csv(nyt, "nyt.csv")
write.csv(hdaily,"hdaily.csv")
# Basic Functions ----

