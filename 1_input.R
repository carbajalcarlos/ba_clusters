# ----- Initialisation -----
# Loading required libraries
require(bibliometrix)

# Loading input data
if (!is.element(el = "directory", set = objects())) {
   directory <- choose.dir(default = getwd(), caption = "Select folder")
}
files <- list.files(path = directory, pattern = "*.bib", full.names = TRUE)
raw = list()
for (i in 1:length(files)) {
  raw[[i]] = suppressWarnings(readLines(files[[i]], encoding = "UTF-8"))
}
raw <- unlist(raw)
# converting input into to DF
bm <- convert2df(file = raw, dbsource = "isi", format = "bibtex")

# ----- Blibliometric analysis -----
bm_anl <- biblioAnalysis(bm, sep = ";")

# Printing bibliometric summary
bm_smm <- summary(object = bm_anl, k = 12, pause = FALSE)

# Printing production graphs
plot(x = bm_anl, k = 12, pause = TRUE)

# Obtaining most cited references by article and author 
#bm$CR[5000] # Used to define the appropriated separatord fiels
bm_cit_art <- citations(M = bm, field = "article", sep = ".   ")
head(bm_cit_art$Cited, n = 12)
bm_cit_aut <- citations(M = bm, field = "author", sep = ".   ")
head(bm_cit_aut$Cited, n = 12)

# Obtaining the most cited local references
bm_cit_loc <- localCitations(M = bm,  sep = "; ")
## By Article
head(bm_cit_loc$Papers, n = 12)
## By Author
head(bm_cit_loc$Authors, n = 12)

# Calculating authors dominance factor
bm_dom <- dominance(results = bm_anl, k = 12)
bm_dom

# Calculating the authors h-index
## One author: 
bm_hin_yoo <- Hindex(M = bm, authors = "BULLON PEREZ J", sep = ";", years = 10)
bm_hin_yoo$H
bm_hin_yoo$CitationList
## One author:  
bm_hin_nam <- Hindex(M = bm, authors = "ZOTT C", sep = ";", years = 10)
bm_hin_nam$H
bm_hin_nam$CitationList
## The 12 most productive authors
authors <- gsub(pattern = ",", replacement = " ", x = names(bm_anl$Authors)[1:12]) # Extracting the 12 most productive authors
bm_hin <- Hindex(M = bm, authors = authors, sep = ";", years = 10)
bm_hin$H
bm_hin$CitationList

# Estimation of the Lotka's Law coefficient
bm_ltk <- lotka(bm_anl)
bm_ltk$AuthorProd
## Estimation coefficients
bm_ltk$Beta # beta
bm_ltk$C # constant
bm_ltk$R2 # Goodness of fit
bm_ltk$p.value # P-value of K-S two sample test
## Distribution comparison plot
bm_ltk_obs <- bm_ltk$AuthorProd[,3] # Observed distribution
bm_ltk_the <- 10^(log10(bm_ltk$C)-2*log10(bm_ltk$AuthorProd[,1])) # Theoretical distribution
plot(x = bm_ltk$AuthorProd[,1], y = bm_ltk_the, col = "red",
     type = "l", ylim = c(0,1), 
     main = "Scientific Productivity",
     xlab = "Articles", ylab = "Frequency of authors") 
lines(x = bm_ltk$AuthorProd[,1], y = bm_ltk_obs, col = "blue")
legend(x = "topright",  lty = c(1,1,1), cex = 1, bty = "n",
       c("Theoretical (B=2)", "Observed"), col = c("red", "blue"))

# Bibliometric matrices
#names(bm)
#bm$SO[50] # Used to define the appropriated separatord fiels
## Printing the 12 most productive journals
bm_cma_jou <- cocMatrix(M = bm, Field = "SO", sep = ";")
sort(Matrix::colSums(bm_cma_jou), decreasing = TRUE)[1:12]
## Printing the 12 most productive authors
bm_cma_aut <- cocMatrix(M = bm, Field = "AU", sep = ";")
sort(Matrix::colSums(bm_cma_aut), decreasing = TRUE)[1:12] # Printing the 12 most productive articles
## Printing the 12 most productive universities
bm_cma_uni <- cocMatrix(M = bm, Field = "AU_UN", sep = ";")
sort(Matrix::colSums(bm_cma_uni), decreasing = TRUE)[1:12] # Printing the 12 most productive articles
## Printing the 12 most productive first author country
bm <- metaTagExtraction(M = bm, Field = "AU_CO", sep = ";")
bm_cma_cou <- cocMatrix(M = bm, Field = "AU_CO", sep = ";")
sort(Matrix::colSums(bm_cma_cou), decreasing = TRUE)[1:12] # Printing the 12 most productive articles
## Printing the 12 most cited references
bm_cma_ctr <- cocMatrix(M = bm, Field = "CR", sep = ".  ")
sort(Matrix::colSums(bm_cma_ctr), decreasing = TRUE)[1:12] # Printing the 12 most productive articles
## Printing the 12 most cited first author
bm <- metaTagExtraction(M = bm, Field = "CR_AU", sep = ";")
bm_cma_cau <- cocMatrix(M = bm, Field = "CR_AU", sep = ";")
sort(Matrix::colSums(bm_cma_cau), decreasing = TRUE)[1:12] # Printing the 12 most productive articles
## Printing the 12 most cited publication
bm <- metaTagExtraction(M = bm, Field = "CR_SO", sep = ";")
bm_cma_cjo <- cocMatrix(M = bm, Field = "CR_SO", sep = ";")
sort(Matrix::colSums(bm_cma_cjo), decreasing = TRUE)[1:12] # Printing the 12 most productive articles
## Printing the 12 most used author's keyword
bm_cma_akw <- cocMatrix(M = bm, Field = "DE", sep = ";")
sort(Matrix::colSums(bm_cma_akw), decreasing = TRUE)[1:12] # Printing the 12 most productive articles
## Printing the 12 most used database aggregator's keyword
bm_cma_dkw <- cocMatrix(M = bm, Field = "ID", sep = ";")
sort(Matrix::colSums(bm_cma_dkw), decreasing = TRUE)[1:12] # Printing the 12 most productive articles
## Printing the 12 most used database aggregator's subject category
bm_cma_dsc <- cocMatrix(M = bm, Field = "SC", sep = ";")
sort(Matrix::colSums(bm_cma_dsc), decreasing = TRUE)[1:12] # Printing the 12 most productive articles

# Bibliographic coupling
## Coupuling articles
bm_bcp_art <- biblioNetwork(M = bm, analysis = "coupling", network = "references", ";")
set.seed(69)
networkPlot(NetMatrix = bm_bcp_art,
            weighted = TRUE, n = 16, Title = "Article's coupuling",
            type = "auto", size = TRUE, remove.multiple = TRUE, edgesize = 5, halo = TRUE,noloops = FALSE, remove.isolates = TRUE)
## Coupuling authors
bm_bcp_aut <- biblioNetwork(M = bm, analysis = "coupling", network = "authors", ";")
set.seed(69)
networkPlot(NetMatrix = bm_bcp_aut, normalize = "salton",
            weighted = TRUE, n = 40, Title = "Author's coupuling",
            type = "fruchterman", size = TRUE, remove.multiple = TRUE, edgesize = 5)
## Coupuling journals
bm_bcp_jou <- biblioNetwork(M = bm, analysis = "coupling", network = "sources", ";")
set.seed(69)
networkPlot(NetMatrix = bm_bcp_jou,
            weighted = TRUE, n = 10, Title = "Journal's coupuling",
            type = "auto", size = TRUE, edgesize = 5)
## Coupuling countries
bm_bcp_cou <- biblioNetwork(M = bm, analysis = "coupling", network = "countries", ";")
set.seed(69)
networkPlot(NetMatrix = bm_bcp_cou, normalize = "salton",
            weighted = TRUE, n = 20, Title = "Country's coupuling",
            type = "fruchterman", size = TRUE, remove.multiple = TRUE)

# Bibliographic co-citation
## Co-citation articles
bm_bcc_art <- biblioNetwork(M = bm, analysis = "co-citation", network = "references", ";")
set.seed(69)
networkPlot(NetMatrix = bm_bcc_art, n = 10, Title = "Article's co-citation network",
            type = "kamada", size = TRUE, remove.multiple = TRUE, 
            labelsize = 0.8, edgesize = 5)
## Co-citation authors
bm_bcc_aut <- biblioNetwork(M = bm, analysis = "co-citation", network = "authors", ";")
set.seed(69)
networkPlot(NetMatrix = bm_bcc_aut, n = 10, Title = "Author's co-citation network",
            type = "kamada", size = TRUE, remove.multiple = TRUE, 
            labelsize = 0.8, edgesize = 2, remove.isolates = TRUE)
## Co-citation journals
bm_bcc_jou <- biblioNetwork(M = bm, analysis = "co-citation", network = "sources", ";")
set.seed(69)
networkPlot(NetMatrix = bm_bcc_jou, n = 20, Title = "Author's co-citation network",
            type = "fruchterman", size = TRUE, remove.multiple = TRUE, 
            labelsize = 0.8, edgesize = 2, remove.isolates = TRUE)

# Bibliographic collaboration
# Author's collaboration
bm_bcl_aut <- biblioNetwork(M = bm, analysis = "collaboration", network = "authors", ";")
set.seed(69)
networkPlot(NetMatrix = bm_bcl_aut, n = dim(bm_bcl_aut)[1], Title = "Author's collaboration",
            type = "kamada", size = TRUE, labelsize = 0.8, edgesize = 2,
            remove.multiple = TRUE, remove.isolates = TRUE, degree = 5)
# University's collaboration
bm_bcl_uni <- biblioNetwork(M = bm, analysis = "collaboration", network = "universities", ";")
set.seed(69)
networkPlot(NetMatrix = bm_bcl_uni, n = dim(bm_bcl_uni)[1], Title = "University's collaboration",
            type = "kamada", size = TRUE, labelsize = 0.8, edgesize = 2,
            remove.multiple = TRUE, remove.isolates = TRUE, degree = 3)
# Country's collaboration
bm_bcl_cou <- biblioNetwork(M = bm, analysis = "collaboration", network = "countries", ";")
set.seed(69)
networkPlot(NetMatrix = bm_bcl_cou, n = dim(bm_bcl_cou)[1], Title = "University's collaboration",
            type = "kamada", size = TRUE, labelsize = 0.8, edgesize = 2,
            remove.multiple = TRUE, remove.isolates = TRUE)

# Co-occurrences analysis
## Aggregator's keywords
bm_coc_dkw <- biblioNetwork(M = bm, analysis = "co-occurrences", network = "keywords", ";")
set.seed(69)
networkPlot(NetMatrix = bm_coc_dkw, normalize = "association", weighted = TRUE, n = 12,
            Title = "Keywords co-ocurrences",
            remove.multiple = TRUE, halo = TRUE,
            type = "kamada", size = TRUE, labelsize = 1, edgesize = 2.5, remove.isolates = TRUE)
## Authors's keywords
bm_coc_akw <- biblioNetwork(M = bm, analysis = "co-occurrences", network = "author_keywords", ";")
set.seed(69)
networkPlot(NetMatrix = bm_coc_akw, normalize = "association", weighted = TRUE, n = 12,
            Title = "Author's keywords co-ocurrences",
            remove.multiple = TRUE, remove.isolates = TRUE, halo = TRUE,
            curved = FALSE, cluster = "walktrap",
            type = "sphere", size = TRUE, labelsize = 1, edgesize = 3)

## Subject category
### masking the subject category under  author's keywords
bm2 <- bm
index.akw <- which(names(bm2) == "DE")
index.sc <- which(names(bm2) == "SC")
names(bm2)[index.akw] <- "DE2"
names(bm2)[index.sc] <- "DE"
### masked extraction
bm_coc_sc <- biblioNetwork(M = bm2, analysis = "co-occurrences", network = "author_keywords", ";")
set.seed(69)
networkPlot(NetMatrix = bm_coc_sc, normalize = "association", weighted = TRUE, n = 8,
            Title = "Subject categories's co-ocurrences",
            remove.multiple = TRUE, remove.isolates = TRUE, halo = TRUE,
            curved = FALSE, cluster = "walktrap",
            type = "auto", size = TRUE, labelsize = .85, edgesize = 2.5)

# Co-word analisis
 bm_cst <- conceptualStructure(M = bm, field = "TI",
                               method = "MCA", stemming = TRUE,
                               labelsize = 10, documents = 15)

#Historical co-citation network
bm_hcc <- histNetwork(M = bm, sep = ";")
set.seed(69)
bm_hcc_plot <- histPlot(histResults = bm_hcc, color = TRUE, size = TRUE,
                        labelsize = 2, size.cex = .5, n = 10)
set.seed(69)
bm_hcc_plot <- histPlot(histResults = bm_hcc, color = TRUE, size = TRUE,
                        arrowsize = .1, labelsize = 2, size.cex = TRUE, n = 20, edgesize = .5)
set.seed(69)
bm_hcc_plot <- histPlot(histResults = bm_hcc, color = TRUE, size = TRUE,
                        arrowsize = .1, labelsize = 4, size.cex = TRUE, n = 22, edgesize = 1)

# ----- Cleaning -----
# Removing unwanted publications
lista <- c("ARTICLE", "ARTICLE; BOOK CHAPTER", "ARTICLE; PROCEEDINGS PAPER", "BOOK", 
           "EDITORIAL MATERIAL", "PROCEEDINGS PAPER")
rare <- rare[which(is.element(el = rare$DT2, set = lista)), ]

# ----- Closing project -----
# Removing temporary objects
rm(directory)
rm(files)
rm(i)
rm(lista)
rm(raw)
# Storing data files
save.image(file = "1_process/image.RData")
save(list = "rare", file = "1_process/rare.RData")
