require(quanteda)

options(stringsAsFactors = F)

source("utils.R")

d <- read.csv("data/same-side-classification/within-topic/test.csv")

t1 <- unlist(d[grepl("abortion", d$topic), c("argument1", "argument2")])
t2 <- unlist(d[grepl("marriage", d$topic), c("argument1", "argument2")])

toks <- corpus(t1) %>% 
  tokens() %>%
  tokens_remove(pattern = "NA")
dtm <- dfm(toks) %>% 
  dfm_trim(min_termfreq = 3)
dim(dtm)


dfm2 <- corpus(t2) %>% 
  tokens() %>%
  tokens_remove(pattern = "NA") %>%
  dfm() %>% 
  dfm_trim(min_termfreq = 3)
dim(dfm2)



kt <- calculateLogLikelihood(colSums(dtm), colSums(dfm2))
sort(kt, decreasing = T)[1:100]


kt2 <- calculateLogLikelihood(colSums(dfm2), colSums(dtm))
sort(kt2, decreasing = T)[1:100]


wikis <- readLines("eng_news_2016_1M-sentences.txt")
dtmw <- corpus(wikis) %>% 
  tokens() %>%
  tokens_remove(pattern = "NA") %>%
  dfm() %>% 
  dfm_trim(min_termfreq = 3)
dim(dtmw)

kt3 <- calculateLogLikelihood(colSums(dfm2), colSums(dtmw))
sort(kt3, decreasing = T)[1:100]

kt4 <- calculateLogLikelihood(colSums(dtm), colSums(dtmw))
sort(kt4, decreasing = T)[1:100]

dtmt1t1 <- corpus(c(t1,t2)) %>% 
  tokens() %>%
  tokens_remove(pattern = "NA") %>%
  dfm() %>% 
  dfm_trim(min_termfreq = 3)
dim(dtmt1t1)

kt5 <- calculateLogLikelihood(colSums(dtmt1t1), colSums(dtmw))
sort(kt5, decreasing = T)[1:100]


kw_ab <- sort(calculateLogLikelihood(colSums(dtm), colSums(dfm2)), decreasing = T)
kw_ab <- kw_ab[kw_ab > 0]

kw_gm <- sort(calculateLogLikelihood(colSums(dfm2), colSums(dtm)), decreasing = T)
kw_gm <- kw_gm[kw_gm > 0]

kw_both <- sort(calculateLogLikelihood(colSums(dtmt1t1), colSums(dtmw)), decreasing = T)
kw_both <- kw_both[kw_both > 0]

kw_arg <- setdiff(names(kw_both), union(names(kw_ab), names(kw_gm)))
