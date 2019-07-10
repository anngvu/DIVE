library(data.table)

# Adding histo info
histo <- fread("Histopathology.tsv")
setnames(histo, c("nPOD", "DonorType", "Date", "Notes"))
histo <- histo[match(dt$nPOD, histo$nPOD)]


txt2Features <- function(x) {
  if(is.na(x)) return(list(Ins = NA, amyloid = NA, dysplasia = NA, metaplasia = NA, pancreatitis = NA, 
                           atrophy = NA, fibrosis = NA, CD3 = NA, Ki67 = NA, islet.freq = NA, islet.size = NA))
  features <- lapply(c("Ins+", "amyloid", "dysplasia", "metaplasia", "pancreatitis", "atroph", "fibro"), 
                     function(term) ifelse(length(grep(term, x, fixed = T)), 1, 0))
  ls <- strsplit(x, split = "[.]|;")[[1]]
  
  # 0 = no infiltrate/CD3 not mentioned, 1 = mild/low, 2 = moderate/high CD3+ observed
  CD3 <- grep("CD3", ls, value = T)[1] # Finds sentence that mentions CD3
  if(length(CD3)) {
    CD3 <- which(sapply(c("mild|low", "moderate|high"), function(l) grep(l, CD3, ignore.case = T)) == 1) 
    CD3 <- ifelse(length(CD3), CD3, 0)
  } else {
    CD3 <- 0
  }
  # 1 = low Ki67, 2 = increased/high Ki67, or NA
  Ki67 <- grep("Ki67", ls, value = T)[1]
  if(length(Ki67)) {
    Ki67 <- which(sapply(c("low", "increased|high"), function(l) grep(l, Ki67, ignore.case = T)) == 1)
    Ki67 <- ifelse(length(Ki67), Ki67, NA)
  } else {
    Ki67 <- NA
  }
  # 1 = reduced, 2 = normal/plenty, or NA
  islet.freq <- which(sapply(c("[^not] reduc", "normal|plent|numerous"), function(l) grep(l, ls[1], ignore.case = T)) == 1)
  if(length(islet.freq)) {
    islet.freq <- ifelse(length(islet.freq), islet.freq, NA)
  } else {
    islet.freq <- NA
  }
  # 1 = small, 2 = normal, 3 = large, 
  islet.size <- which(sapply(c("small", "normal", "large"), function(l) grep(l, ls[1], ignore.case = T)) == 1) 
  if(length(islet.size)) {
    if(length(islet.size == 2)) {
      islet.size <- 2
    } else {
      islet.size <- ifelse(length(islet.size), islet.size, NA)
    }
  } else {
    islet.size <- NA
  }
  
  features <- c(features, CD3, Ki67, islet.freq, islet.size)
  return(features)
}

features <- rbindlist(lapply(histo$Notes, txt2Features))

histo[, paste0("H.", names(features)) := features]

# Export for manual review
write.table(histo, "histo_subset2.txt", sep = "\t", row.names = F)