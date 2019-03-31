# ----- Initialisation -----
# Loading required libraries
require(bibliometrix)
require(stringdist)

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
bm <- duplicatedMatching(M = bm, Field = "TI", tol = 0.95)
bm <- bm[order(bm$TI),]
comparison <- stringdistmatrix(a = bm$TI, b = bm$TI, method = "jw")
comparison <- data.frame(which(comparison < 0.20, arr.ind = TRUE))
comparison <- subset(x = comparison, subset = !(row == col))
comparison <- comparison[order(comparison$row), ]
comparison$group <- 0
comparison$name.a <- bm$AU[comparison$row]
comparison$name.b <- bm$AU[comparison$col]
comparison$std <- stringdist(a = comparison$name.a, b = comparison$name.b, method = "jw")
comparison <- subset(x = comparison, subset = std < 0.2)
for (i in 2:nrow(comparison)) {
  comparison$group[i] <- comparison$row[i] - comparison$row[i-1]
}
comparison <- subset(x = comparison, subset = group != 1)

comparison$abl.a <- nchar(bm$AB[comparison$row])
comparison$abl.b <- nchar(bm$AB[comparison$col])
comparison$size <- comparison$abl.a - comparison$abl.b
comparison$del <- comparison$row
index <- comparison$size > 1
comparison$del[index] <- comparison$col[index]
index <- comparison$del
# Removing duplicates identified
temp <- bm[-index, ]
# Reduction to only one value without multiple parameter selection

