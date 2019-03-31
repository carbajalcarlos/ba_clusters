# ----- Initialisation -----
# Loading required libraries
require(bibliometrix)

# Manual reading of the codes
## Selection of data folder
if (!is.element(el = "directory", set = objects())) {
  directory <- choose.dir(default = getwd(), caption = "Select folder")
}
## Reading Scopus *.bib files
files <- list.files(path = directory, pattern = "*sco.bib", full.names = TRUE)
raw = list()
for (i in 1:length(files)) {
  raw[[i]] = suppressWarnings(readLines(files[[i]], encoding = "UTF-8"))
}
raw <- unlist(raw)
bm.sco <- convert2df(file = raw,dbsource = "scopus", format = "bibtex")
## Reading WoS *.bib files
files <- list.files(path = directory, pattern = "*wos.bib", full.names = TRUE)
raw = list()
for (i in 1:length(files)) {
  raw[[i]] = suppressWarnings(readLines(files[[i]], encoding = "UTF-8"))
}
raw <- unlist(raw)
bm.wos <- convert2df(file = raw,dbsource = "isi", format = "bibtex")
# Merging dataframes
## Combining dataframes
temp <- subset(x = bm.wos, select = colnames(bm.sco))
bm <- rbind(bm.sco, temp)
## Cleaning merged dataframe
### Cuting date 2018]
bm <- subset(x = bm, subset = PY < 2019)
### Removing items with missing data
index <- is.na(bm$AU) # Missing authors
bm <- bm[!index, ]
index <- is.na(bm$AB) # Missing abstracts
bm <- bm[!index, ]
### Anonymous authors
index <- grep(pattern = "ANONYMOUS", x = bm$AU, ignore.case = TRUE)
if (length(index) > 0) {
  bm <- bm[-index,]
}
index <- grep(pattern = "NA NA", x = bm$AU, ignore.case = TRUE)
if (length(index) > 0) {
  bm <- bm[-index,]
}

### Removing duplicates
index <- duplicated(x = bm$TI)
bm <- bm[!index, ]

temp <- duplicatedMatching(M = bm, Field = "TI", tol = 0.20)
