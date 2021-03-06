# ===== Loading required libraries =====
require(bibliometrix)
require(quanteda)
require(stringdist)
require(tm)

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
#bm <- mergeDbSources(bm.sco, bm.wos, remove.duplicated = TRUE)
bm <- bm.wos
## Cleaning merged dataframe
bm <- subset(x = bm, subset = PY < 2019)
bm <- subset(x = bm, subset = DT == "ARTICLE" | DT == "CONFERENCE PAPER" | DT == "PROCEEDINGS PAPER")
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
bm <- termExtraction(M = bm, Field = "TI", stemming = TRUE)
bm <- termExtraction(M = bm, Field = "AB", stemming = TRUE)
bm <- termExtraction(M = bm, Field = "DE", stemming = TRUE)
bm <- termExtraction(M = bm, Field = "ID", stemming = TRUE)
# Removing empty data in keywords
index <- grep(pattern = "^NA$", x = bm$DE_TM, ignore.case = TRUE)
bm$DE_TM[index] <- NA
index <- grep(pattern = "^NA$", x = bm$ID_TM, ignore.case = TRUE)
bm$DE_TM[index] <- NA

# ===== Closing project =====
# Removing temporary objects
rm(files)
rm(raw)
rm(i)
rm(index)
rm(comparison)
rm(bm.sco)
rm(bm.wos)

# Storing data files
output <- gsub(pattern = ".*\\\\([[:alnum:]]+)$", replacement = "\\1", x = directory)
save.image(file = paste(c("1_process/", output, "_image.RData"), collapse = ""))
save(list = "bm", file = paste(c("1_process/", output, "_bm.RData"), collapse = ""))
# write.xlsx(x = bm, file = paste(c("1_process/", directory, "_bm.xlsx"), collapse = ""),
#            sheetName="Sheet1", col.names=TRUE, row.names=TRUE, append=FALSE, showNA=FALSE)

# ===== Adopting strategies =====
M <- bm
# Stemming abstracts
M$AB_RAW <- M$AB
M$AB <- gsub(pattern = ";", replacement = " ", x = M$AB_TM)
# Removing keywords plus
M$ID_RAW <- M$ID
# Internet of things
# index <- grep(pattern = "^INTERNET OF THING", x = M$ID, ignore.case = TRUE)
# M$ID[index] <- trimws(gsub(pattern = "^INTERNET OF THING[^;]*;",
#                            replacement = "", x = M$ID[index]), which = "both")
# index <- grep(pattern = ";.*INTERNET OF THING[^;]", x = M$ID, ignore.case = TRUE)
# M$ID[index] <- trimws(gsub(pattern = ";[^;]*INTERNET OF THING[^;]*",
#             replacement = "", x = M$ID[index]), which = "both")
# Industry 4
index <- grep(pattern = "^INDUSTRY 4.0", x = M$ID, ignore.case = TRUE)
M$ID[index] <- trimws(gsub(pattern = "^INDUSTRY 4.0[^;]*;",
                           replacement = "", x = M$ID[index]), which = "both")

index <- grep(pattern = ";.*INDUSTRY 4[^;]", x = M$ID, ignore.case = TRUE)
M$ID[index] <- trimws(gsub(pattern = ";[^;]*INDUSTRY 4[^;]*",
                           replacement = "", x = M$ID[index]), which = "both")
index <- grep(pattern = ";.*INDUSTRIE 4[^;]", x = M$ID, ignore.case = TRUE)
M$ID[index] <- trimws(gsub(pattern = ";[^;]*INDUSTRIE 4[^;]*",
                           replacement = "", x = M$ID[index]), which = "both")
# INDUSTRIAL INTERNET OF THINGS 
index <- grep(pattern = "INDUSTRIAL INTERNET OF THINGS", x = M$ID, ignore.case = TRUE)
M$ID[index] <- trimws(gsub(pattern = ";[^;]*INDUSTRIAL INTERNET OF THINGS[^;]*", ignore.case = TRUE,
                           replacement = "", x = M$ID[index]), which = "both")
index <- grep(pattern = "INDUSTRIAL IOT", x = M$ID, ignore.case = TRUE)
M$ID[index] <- trimws(gsub(pattern = ";[^;]INDUSTRIAL IOT[^;]*", ignore.case = TRUE,
                           replacement = "", x = M$ID[index]), which = "both")
# business model
index <- grep(pattern = "^[^;]*business model", x = M$ID, ignore.case = TRUE)
M$ID[index] <- trimws(gsub(pattern = "^[^;]*business model[^;]*;", ignore.case = TRUE,
                           replacement = "", x = M$ID[index]), which = "both")
index <- grep(pattern = "business model", x = M$ID, ignore.case = TRUE)
M$ID[index] <- trimws(gsub(pattern = ";[^;]*business model[^;]*", ignore.case = TRUE,
                           replacement = "", x = M$ID[index]), which = "both")

M$ID[index] <- trimws(gsub(pattern = ";[^;]*industrial revolution[^;]*", ignore.case = TRUE,
                           replacement = "", x = M$ID[index]), which = "both")
# INDUSTRIAL REVOLUTION
index <- grep(pattern = "INDUSTRIAL REVOLUTION", x = M$ID, ignore.case = TRUE)
M$ID[index] <- trimws(gsub(pattern = ";[^;]*INDUSTRIAL REVOLUTION[^;]*", ignore.case = TRUE,
                           replacement = "", x = M$ID[index]), which = "both")

# Unification of terms
index <- grep(pattern = "INTERNET OF THING", x = M$ID, ignore.case = TRUE)
M$ID[index] <- trimws(gsub(pattern = "[^;]*INTERNET OF THING[^;]*", ignore.case = TRUE,
                           replacement = " INTERNET OF THINGS", x = M$ID[index[30]]), which = "both")

# # Searching tool
# index <- grep(pattern = "IOT", x = M$ID, ignore.case = TRUE)
# M$ID[index]
# # Visualisation of the terms extracted
# lista <- trimws(unlist(strsplit(na.omit(M$ID), split = ";")), which = "both")
# lista <- data.frame(table(lista))

# ===== DO TO M ====
index <- grep(pattern = "business|management", x = M$SC, ignore.case = TRUE)
M <- M[index, ]


# Storing M into RData file 
save(list = "M", file = paste(c("1_process/", output, "_M2.RData"), collapse = ""))

#Working 
if (FALSE) {
  biblioshiny()
}


bm_cst <- conceptualStructure(M = bm, field = "AB", method = "CA", stemming = TRUE,
                              minDegree=3, k.max = 5)
