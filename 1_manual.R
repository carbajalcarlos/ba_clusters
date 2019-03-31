# ===== Loading required libraries =====
require(bibliometrix)
require(stringdist)

# ===== Manual reading of the codes =====
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

# ===== Merging dataframes =====
## Combining dataframes
bm <- mergeDbSources(bm.sco, bm.wos, remove.duplicated = TRUE)
## Cleaning merged dataframe
bm <- subset(x = bm, subset = PY < 2019)
## Removing items with missing data
index <- is.na(bm$AU) # Missing authors
bm <- bm[!index, ]
index <- is.na(bm$AB) # Missing abstracts
bm <- bm[!index, ]
### Anonymous authors
index <- grep(pattern = "ANONYMOUS", x = bm$AU, ignore.case = TRUE)
if (length(index) > 0) {
  bm <- bm[-index,]
}
index <- grep(pattern = "^NA N", x = bm$AU, ignore.case = TRUE)
if (length(index) > 0) {
  bm <- bm[-index,]
}

# ===== Removing duplicates =====
# Standard bibliometrix de-duplication
bm <- duplicatedMatching(M = bm, Field = "TI", tol = 0.95)
bm <- bm[order(bm$TI),]

# Identifying duplicates by Title string distance
comparison <- stringdistmatrix(a = bm$TI, b = bm$TI, method = "jw")
comparison <- data.frame(which(comparison < 0.20, arr.ind = TRUE)) # threshold = 0.20
# Removing self-comparisons
comparison <- subset(x = comparison, subset = !(row == col))
comparison <- comparison[order(comparison$row), ]
comparison$group <- comparison$row - comparison$col
comparison <- subset(x = comparison, subset = group < 0)

# Identifying duplicates by Authors string distance
comparison$name.a <- bm$AU[comparison$row]
comparison$name.b <- bm$AU[comparison$col]
comparison$std <- stringdist(a = comparison$name.a, b = comparison$name.b, method = "jw")
comparison <- subset(x = comparison, subset = std < 0.20) # threshold = 0.20

# Identifying the entry to keep according to abstract size
comparison$abl.a <- nchar(bm$AB[comparison$row])
comparison$abl.b <- nchar(bm$AB[comparison$col])
comparison$size <- comparison$abl.a - comparison$abl.b
comparison$del <- comparison$row
index <- comparison$size > 1
comparison$del[index] <- comparison$col[index]
index <- comparison$del

# Removing duplicates identified from the main dataset
bm <- bm[-index, ]

# ===== PRE-Analysis =====
# Terms extraction
bm.pre <- termExtraction(M = bm, Field = "TI", stemming = TRUE)
bm.pre <- termExtraction(M = bm.pre, Field = "AB", stemming = TRUE)
bm.pre <- termExtraction(M = bm.pre, Field = "DE", stemming = TRUE)
bm.pre <- termExtraction(M = bm.pre, Field = "ID", stemming = TRUE)
# Removing empty data in keywords
index <- grep(pattern = "^NA$", x = bm.pre$DE_TM, ignore.case = TRUE)
bm.pre$DE_TM[index] <- NA
index <- grep(pattern = "^NA$", x = bm.pre$ID_TM, ignore.case = TRUE)
bm.pre$DE_TM[index] <- NA

## Visualisation of the terms extracted
# lista <- trimws(unlist(strsplit(na.omit(bm.pre$AB_TM), split = ";")), which = "both")
# lista <- data.frame(table(lista))
