library(data.table)

# -- DATA ------------------------------------------------------------------------------------------------------#

H.freq <- fread("A_C_B_DRB1_DQB1.tsv") # Reference data (https://frequency.nmdp.org/jsp/haploTypeFrequencies.jsp)
HLA <- fread("HiResHLAclean_ref.tsv") # nPOD Core HLA data

# -- ETC. ----------------------------------------------------------------------------------------------------- #

loci <- c("A", "B", "C", "DRB1", "DQB1")
coloci <- setNames(unlist(lapply(loci, paste0, c(".1", ".2"))), rep(loci, each = 2))
race.freq <- list("Caucasian" = "CAU_freq", "African Am" = "AFA_freq", "Hispanic/Latino" = "HIS_freq",
                  "American Indian/Alaska Native" = "NAM_freq", "Asian" = "API_freq",
                  "Multiracial" = c("AFA_freq", "API_freq", "CAU_freq", "HIS_freq", "NAM_freq"))

# Minor cleaning and keying of reference table
H.freq[, (loci) := lapply(.SD, function(x) gsub("[^0-9:]", "", sapply(strsplit(x, "*", fixed = T), `[[`, 2))), .SDcols = loci]
setkeyv(H.freq, loci)

# -- FUN ------------------------------------------------------------------------------------------------------ #

# Phasing with complete hi-res, non-ambiguous data using H.freq as reference
phase <- function(x, loci) {
  x <- as.list(x)
  race.code <- x$ethnic.grp[1]
  loci.ls <- lapply(x[loci], unique) # prevent redundant combinations for homozygous loci
  H <- do.call(expand.grid, list(loci.ls, stringsAsFactors = F))
  n <- nrow(H)
  n1 <- 1:(n/2)
  n2 <- n:(n/2+1)
  h.freq <- sapply(split(H, 1:n), function(hkey) H.freq[hkey, get(race.freq[[race.code]])])
  if(race.code == "Multiracial") h.freq <- rowMeans(h.freq)
  h.freq[is.na(h.freq)] <- 0 # If haplotype not observed, then essentially 0
  pair.freq <- h.freq[n1] * h.freq[n2]
  max.pair.freq <- max(pair.freq)
  if(max.pair.freq > 0) {
    mostLikely <- which(pair.freq == max(pair.freq))[1]
  } else {
    mostLikely <- which(h.freq == max(h.freq))[1]
    if(mostLikely %in% n2) mostLikely <- which(n2 == mostLikely)
  }
  H$h.freq <- h.freq
  phased <- list(ID = x$ID[1], ethnic.grp = race.code, `1` = H[n1, loci][mostLikely, ], `2` = H[n2, loci][mostLikely, ],
                 max.pair.freq = max.pair.freq, H = H)
  return(phased)
}

# Modified function to deal with low-res entries
phaseA <- function(x, loci) {
  x <- as.list(x)
  race.code <- x$ethnic.grp[1]
  loci.ls <- x[loci]
  # If low-res allele, get set of possible hi-res, e.g. 02 -> {02:01, 02:02, 02:03, ... }
  for(l in names(loci.ls)) loci.ls[[l]] <- sapply(loci.ls[[l]], simplify = F, # TO DO: re-write to avoid for-loop
                                                  function(a) if(nchar(a) == 2) grep(paste0(a, ":"), unique(H.freq[[l]]), val = T) else a)
  H <- unique(expand.grid(lapply(loci.ls, unlist, use.names = F), stringsAsFactors = F)) # a very large number of combns
  h.freq <- H.freq[H, get(race.freq[[race.code]])]
  if(race.code == "Multiracial") h.freq <- rowMeans(h.freq)
  h.freq[is.na(h.freq)] <- 0
  H$h.freq <- h.freq
  H <- H[which(H$h.freq > 0), ]
  H <- H[order(H$h.freq, decreasing = T), ]
  result <- comPair(H, loci.ls) # check for complementary pairs
  return(c(ID = x$ID[1], ethnic.grp = race.code, result))
}

# At each locus of H1, find allele for H2 using list in loci.ls
getH2 <- function(H1, loci.ls) {
  H2 <- Map(otherAllele, H1, loci.ls) # H1 can map to multiple H2 when data is low-res
  H2 <- unique(expand.grid(H2, stringsAsFactors = F))
  return(H2)
}

otherAllele <- function(H1.allele, locus.i) {
  alleles <- names(locus.i)
  H2.allele <- which(is.na(pmatch(alleles, H1.allele))) # H2 allele is one NOT matched; works for hetero- or homozygous locus
  H2.allele <- locus.i[[H2.allele]] # can contain one or multiple
  return(H2.allele)
}

getFreq <- function(h, H) {
  where <- Reduce(`&`, Map(`%in%`, H[, names(H) != "h.freq"], h))
  if(any(where)) {
    H2 <- H[which(where), ]
    H2 <- H2[order(H2$h.freq), ][1, ]
  } else {
    H2 <- h[1, ]
    H2$h.freq <- 0
  }
  return(H2)
}

# Given H, already ordered by h.freq, compute frequencies for all valid pairs and return most likely pair
comPair <- function(H, loci.ls) {
  H1.ls <- lapply(split(H[, names(loci.ls)], 1:nrow(H)), as.list)
  H2 <- lapply(H1.ls, getH2, loci.ls = loci.ls)
  H2 <- lapply(H2, getFreq, H = H)
  H2 <- do.call(rbind, H2)
  pair.freq <- H$h.freq * H2$h.freq
  max.pair.freq <- max(pair.freq)
  mostLikely <- which(pair.freq == max.pair.freq)[1]
  return(list(`1` = H[mostLikely, names(loci.ls)], `2` = H2[mostLikely, names(loci.ls)], max.pair.freq = max.pair.freq, H = H))
}

# Main phasing function for DR/DQ using WH as reference
phase2 <- function(x, loci, ignoreRace = T, hasAmbiguous = F) {
  x <- as.list(x)
  race.code <- x$ethnic.grp[1]
  loci.ls <- if(hasAmbiguous) xpandL(x, loci) else x[loci]
  H <- do.call(expand.grid, args = list(loci.ls, stringsAsFactors = F))
  n <- nrow(H)
  n1 <- 1:(n/2)
  n2 <- n:(n/2+1)
  if(ignoreRace | race.code == "Multiracial") {
    h.count <- sapply(split(H, 1:n), function(h) WH[h, sum(Count)])
  } else {
    h.count <- sapply(split(H, 1:n), function(h) WH[h][Pop2 == race.code, sum(Count)])
  }
  h.count[is.na(h.count)] <- 0 # If haplotype not observed, then essentially 0
  pair.freq <- h.count[n1] + h.count[n2]
  mostLikely <- which(pair.freq == max(pair.freq))[1]
  H$h.count <- h.count
  phased <- list(ID = x$ID[1], ethnic.grp = race.code, `1` = H[n1, loci][mostLikely, ], `2` = H[n2, loci][mostLikely, ], H = H)
  return(phased)
}

# Re-formatting ambiguous entries
reformat <- function(l) {
  s <- strsplit(l, "/")[[1]]
  if(length(s) == 2) {
    s[2] <- paste0(substr(s[1], 1, 4), s[2])
  }
  return(s)
}

# Just a wrapper that calls reformatting for all loci necessary
xpandL <- function(x, loci) {
  x <- lapply(x[loci], function(l) unlist(lapply(l, reformat)))
  return(x)
}

# -- PHASING 1 ------------------------------------------------------------------------------------------------ #

# Phasing with 5 loci ("A, "B", "C", "DRB1", "DQB1") -- get complete rows
A5 <- HLA[HLA[, Reduce(`&`, lapply(.SD, `!=`, "")), .SDcols = coloci], c("ID","ethnic.grp", coloci), with = F ]

# A few entries containing lower-resolution typing (though none with ambiguous alleles), excluded for now
# Proceed with the 5-loci-complete hi-res data
issue <- A5[, Reduce(`|`, lapply(.SD, function(x) nchar(x) != 5)), .SDcols = coloci]
# These will be processed differently, so exclude these entries for now
A5.2 <- A5[issue ]
A5 <- A5[!issue ]
A5 <- melt(A5, measure.vars = patterns(paste0("^", loci)), value.name = loci) # long format

A5.results <- lapply(split(A5, by = "ID"), phase, loci = loci)
A5.phased <- rbindlist(lapply(A5.results, function(x) as.data.table(x[1:5])))
setnames(A5.phased, c("ID", "ethnic.grp", paste0(rep(loci, 2), rep(c(".1", ".2"), each = 5)), "max.pair.freq"))

# -- PHASING 1.2 ---------------------------------------------------------------------------------- #

# Now deal with issues
A5.2 <- melt(A5.2, measure.vars = patterns(paste0("^", loci)), value.name = loci)

A5.2.results <- lapply(split(A5.2, by = "ID"), phaseA, loci = loci)
A5.2.phased <- rbindlist(lapply(A5.2.results, function(x) as.data.table(x[1:5])))
setnames(A5.2.phased, c("ID", "ethnic.grp", paste0(rep(loci, 2), rep(c(".1", ".2"), each = 5)), "max.pair.freq"))

# Append A5.2 results to main results
A5.phased <- rbind(A5.phased, A5.2.phased)

# -- PHASING 2 ------------------------------------------------------------------------------------------------ #

WH <- fread("world_haplotypes_cleaned.tsv")
setkeyv(WH, c("DRB1", "DQA1", "DQB1"))

drdq <- c("DRB1.1", "DRB1.2", "DQA1.1", "DQA1.2", "DQB1.1", "DQB1.2")
# Use cases with complete data, ignoring cases with ambiguous/missing/low-res alleles
DRDQ <- HLA[ HLA[, Reduce(`&`, lapply(.SD, function(x) nchar(x) == 5)), .SDcols = drdq], c("ID","ethnic.grp", drdq), with = F ]
DRDQ <- melt(DRDQ, measure.vars = patterns(c("DRB1", "DQA1", "DQB1")), value.name = c("DRB1", "DQA1", "DQB1"))

D3.results.T <- lapply(split(DRDQ, by = "ID"), phase2, loci = c("DRB1", "DQA1", "DQB1"))
D3.results.F <- lapply(split(DRDQ, by = "ID"), phase2, loci = c("DRB1", "DQA1", "DQB1"), ignoreRace = F)

D3.T.phased <- rbindlist(lapply(D3.results.T, function(x) as.data.table(x[c(1,3:4)])))
setnames(D3.T.phased, c("ID", unlist(lapply(c(".1", ".2"), function(x) paste0(c("DRB1", "DQA1", "DQB1"), x)))))

D3.F.phased <- rbindlist(lapply(D3.results.F, function(x) as.data.table(x[1:2])))
setnames(D3.F.phased, unlist(lapply(c(".1", ".2"), function(x) paste0(c("DRB1", "DQA1", "DQB1"), x))))

# See how many homozygous:
D3.T.phased[DRB1.1 == DRB1.2 & DQB1.1 == DQB1.2]

# View discrepant results for phasing -- only 2 cases are actually different
diff <- fsetdiff(A5.phased[, c("ID", drdq[c(1:2, 5:6)]), with = F], D3.T.phased[, c("ID", drdq[c(1:2, 5:6)]), with = F])
inverted <- diff$ID[which(Reduce(`&`, Map(`==`, A5.phased[match(diff$ID, ID), .(DRB1.1, DRB1.2, DQB1.1, DQB1.2)],
                                        D3.T.phased[match(diff$ID, ID), .(DRB1.2, DRB1.1, DQB1.2, DQB1.1)]))) ]
# Manually reassign cases
# 6007 differs between A5.phased and D3.T.phased but results with "ignoreRace = F" is consistent with A5.phased
D3.T.phased[ID == 6007, c("DRB1.1", "DRB1.2", "DQB1.1", "DQB1.2") := A5.phased[ID == 6007, .(DRB1.1, DRB1.2, DQB1.1, DQB1.2)]]
for(i in inverted) D3.T.phased[ID == i, c("DRB1.1", "DRB1.2", "DQB1.1", "DQB1.2") := A5.phased[ID == i, .(DRB1.1, DRB1.2, DQB1.1, DQB1.2)]]

phased <- merge(D3.T.phased, A5.phased, by = c("ID", "DRB1.1", "DQB1.1", "DRB1.2", "DQB1.2"), all = T)

# One row with missing data / phased[ID == 6058]
x6058 <- HLA[ID == 6058] # impute
x6058 <- melt(x6058, measure.vars = patterns(c("DRB1", "DQA1", "DQB1")), value.name = c("DRB1", "DQA1", "DQB1"))
x6058 <- phase2(x6058, loci = c("DRB1", "DQA1", "DQB1"))[1:3]

phased[ID == 6058, c("DQA1.1", "DQA1.2") := .("05:01", "04:01")]

# -- PHASING 2.2 ------------------------------------------------------------------------------------------------ #

# Review the cases still not phased
D3.ambig <- HLA[!ID %in% phased$ID]
D3.ambig <-D3.ambig[ D3.ambig[, Reduce(`&`, lapply(.SD, function(x) nchar(x) > 4)), .SDcols = drdq] ]
D3.ambig <- melt(D3.ambig, measure.vars = patterns(c("DRB1", "DQA1", "DQB1")), value.name = c("DRB1", "DQA1", "DQB1"))

D3.ambig.results <- lapply(split(D3.ambig, by = "ID"), phase2, loci = c("DRB1", "DQA1", "DQB1"), ignoreRace = F, hasAmbiguous = T)
D3.ambig.phased <- rbindlist(lapply(D3.ambig.results, function(x) as.data.table(x[1:4])))
setnames(D3.ambig.phased, c("ID", "ethnic.grp", unlist(lapply(c(".1", ".2"), function(x) paste0(c("DRB1", "DQA1", "DQB1"), x)))))

# -- FINAL ------------------------------------------------------------------------------------------------------ #

phased <- rbind(phased, D3.ambig.phased, use.names = T, fill = T)
phasedL <- melt(phased, measure.vars = patterns(paste0("^", c(LETTERS[1:3], c("DRB1", "DQA1", "DQB1")))),
                value.name = c(LETTERS[1:3], c("DRB1", "DQA1", "DQB1")), variable.name = "H")
write.table(phased, file = "phased.tsv", sep = "\t", row.names = F, quote = F)
write.table(phasedL, file = "phasedL.tsv", sep = "\t", row.names = F, quote = F)
