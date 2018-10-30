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
rare <- convert2df(file = raw, dbsource = "isi", format = "bibtex")

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
