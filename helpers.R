library(AnnotationDbi)
library(org.Hs.eg.db)
library(GO.db)
library(reactome.db)
library(reshape2)

## API Keys
## In the future, would want to store api keys as an environment variable
## But right now this allows testing the app on any machine without having to get/set a key

## BioPortal key
bpkey <- "4e941848-9247-4b38-a68a-fc6f235cb4e6"

load("Ontology/db.Rdata")

########## GO lookup  ######################################################################### 

go2genes <- function(GOID) {
  entrez <- NULL
  go2 <- tryCatch(AnnotationDbi::select(org.Hs.eg.db, GOID, c("SYMBOL", "ENTREZID"), "GO"), error = function(e) return())
  if(!is.null(go2)) {
    # accEvidence <- c("EXP", "IDA", "IPI", "IMP", "IGI", "IEP", "IBA", "IBD", "RCA", "IC")
    # go2genes <- go2genes[go2genes$EVIDENCEALL %in% accEvidence, ]
    entrez <- setNames(unique(go2$ENTREZID), unique(go2$SYMBOL))
  } 
  return(entrez)
}

########## Reactome lookup  ######################################################################### 

path2genes <- function(PATHID) {
  entrez <- NULL
  path2 <- tryCatch(AnnotationDbi::select(reactome.db, PATHID, "ENTREZID", "PATHID"), error = function(e) return())
  if(!is.null(path2)) {
    entrez <- select(org.Hs.eg.db, path2$ENTREZID, "SYMBOL", "ENTREZID")
    # Pathways can include non-human genes that won't be found in org.Hs.eg.db; 
    # if this is ever needed, use Entrez ids as labels
    # na <- is.na(entrez$SYMBOL)
    # entrez$SYMBOL[na] <- paste("(Non-human gene)", entrez$ENTREZID[na])
    entrez <- setNames(entrez$ENTREZID, entrez$SYMBOL)
  } 
  return(entrez)
}

########## Expression Matrix subsetting #########################################################################

# From expression matrix, returns a subsetted data.frame in long format given a NAMED vector of gene entrez ids (names = symbols), e.g.:
# Gene   ID   Expression DonorType
# INS 6102 -2.218647500        HC
# PEX1 6102 -0.315356730        HC
# A1BG 6102 -0.768768900        HC
# NAT1 6102  0.913388970        HC
# NAT2 6102 -1.002683200        HC
genes2xm <- function(entrez, dataformat = "long") {
  group <- unlist(mapply(rep, c("No diabetes", "Autoab Pos", "T1D"), c(7,6,10)))
  
  if(dataformat != "long") { # assume "wide"
    if (length(ri) > 1) xm2 <- t(xm2) # vector becomes a one-column data.frame when coerced; t() not needed
    xm2 <- as.data.frame(xm2)
    colnames(xm2) <- make.names(names(ri))
    xm2$DonorType <- as.numeric(factor(group, levels = c("No diabetes", "Autoab Pos", "T1D")))
  } else {
    xm2$Gene <- names(ri)
    xm2 <- reshape2::melt(xm2, id.vars = "Gene", variable.name = "ID", value.name = "Expression")
    xm2$DonorType <- factor(rep(group, each = length(ri))) 
  }
  return(xm2)
}

# Return only genes that show significant differences in expression; number of genes significantly different


