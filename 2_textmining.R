# ===== Text mining cluster formation =====
# Adding identifier to the dataset
M$doc_id <-  paste("ipdm", 1:(nrow(M)), sep = "_")
# Composing corpus
# corp_bm <- corpus(M$AB_RAW, docnames = M$doc_id)
corpus_raw <- corpus(M$TI_TM, docnames = M$doc_id)
docvars(corpus_raw, "author") <- M$AU
docvars(corpus_raw, "paper_numeric") <- seq_len(ndoc(corpus_raw))

# # Testing the composition
# summary(corpus_raw)
# texts(corpus_raw)[10] %>% 
#   stringi::stri_sub(1, 240) %>% 
#   cat()

dfm_prep <- dfm(corpus_raw, remove_numbers = TRUE, tolower = TRUE,
                remove_punct = TRUE, verbose = TRUE)
# dfm_papers <- dfm(dfm_prep, stem = TRUE, remove = stopwords("english"))
dfm_papers <-  dfm(x = dfm_prep)
dfm_papers <- dfm_papers[, order(featnames(dfm_papers))]
#head(dfm_papers, nf = 10)

## Removing unwanted terms from the dataframe.
dict <- dictionary(list(blackholes = c("industri", "model", "busi")))

dfm_papers <- dfm_remove(x = dfm_papers, pattern = dict)
#head(dfm_papers, nf = 10)

# textplot_wordcloud()  plot word clouds of the most frequent words in Papers 12 and 24
#set.seed(15)
#textplot_wordcloud(dfm_papers[c("ipdm_12", "ipdm_24"), ], 
#                   max_words = 50, comparison = TRUE)

# identify clusters of similar essay based on term frequency-inverse document frequency 
dfm_papers_tfidf <- dfm_tfidf(dfm_papers, base = 10)
# these are the 10 most important words from a specific article
#topfeatures(dfm_papers_tfidf[50, ], n = 10)

k <- 4

km_out <- stats::kmeans(dfm_papers_tfidf, centers = k, algorithm = )
km_out$iter

colnames(km_out$centers) <- featnames(dfm_papers_tfidf)

for (i in 1:k) { # loop for each cluster
  cat("CLUSTER", i, "\n")
  cat("Top 10 words:\n") # 10 most important terms at the centroid
  print(head(sort(km_out$centers[i, ], decreasing = TRUE), n = 10))
  cat("\n")
  cat("Publications classified: \n") # extract essays classified
  print(docnames(dfm_papers_tfidf)[km_out$cluster == i])
  cat("\n")
}

