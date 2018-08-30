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

#-- Correlations  ----------------------------------------------------------------------------------------#
# Remove data with 0 variance
rem0Var <- function(x) sd(na.omit(x)) != 0 

# Return data suitable for running correlations
data2cor <- function(cdata) {
  cor.data <- Filter(is.numeric, cdata) # remove nominal category variables
  cor.data <- cor.data[, !grepl("^ID|_SE$|_SEM$|_SD$", names(cor.data )), with = F]
  cor.data <- Filter(rem0Var, cor.data)
  corM <- cor(cor.data, use = "pairwise.complete.obs", method = "spearman")
  corN <- crossprod(as.matrix(cor.data[, lapply(.SD, function(x) as.integer(!is.na(x)))]))
  return(list(corM = corM, corN = corN))
}

#-- GO lookup  ----------------------------------------------------------------------------------------#

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

#-- Reactome lookup ------------------------------------------------------------------------------------#

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

#-- xdata ----------------------------------------------------------------------------------------#

gxGet <- function(genes) {
  xdata <- gx[Entrez %in% genes, c(1:23, 24)] # cols 1-23 = donor values, 24 = Gene name
  xdata <- xdata[!duplicated(Gene)] 
  genes <- xdata$Gene
  if(!length(genes)) return(NULL)
  xdata <- xdata[, data.table(t(.SD), keep.rownames = T), .SDcols = 1:23]
  setnames(xdata, c("ID", make.names(genes)))
  xdata[, ID := as.numeric(ID)]
  xdata[, ID2 := c(paste0("HC", 1:7), paste0("AAB", 1:6), paste0("T1D", 1:10))]
  return(xdata)
}

px1Get <- function(genes) {
  xdata <- px1[Entrez %in% genes, c("6029", "6057", "6096", "6172", "6174", "6195", "6196", "6051", "6211", "6212", "Gene")]
  xdata <- xdata[!duplicated(Gene)] 
  genes <- xdata$Gene
  if(!length(genes)) return(NULL)
  xdata <- xdata[, data.table(t(.SD), keep.rownames = T), .SDcols = 1:10]
  setnames(xdata, c("ID", make.names(genes)))
  xdata[, ID := as.numeric(ID)]
  xdata[, ID2 := c(paste0("HC", 1:5), paste0("T1D", 1:5))]
  return(xdata)
}

px2Get <- function(genes) {
  xdata <- px1[Entrez %in% genes, c("6029", "6057", "6096", "6172", "6174", "6195", "6196", "6051", "6211", "6212", "Gene")]
  xdata <- xdata[!duplicated(Gene)] # TO DO: for other datasets?
  genes <- xdata$Gene
  if(!length(genes)) return(NULL)
  xdata <- xdata[, data.table(t(.SD), keep.rownames = T), .SDcols = 1:10]
  setnames(xdata, c("ID", make.names(genes)))
  xdata[, ID := as.numeric(ID)]
  xdata[, ID2 := c(paste0("HC", 1:5), paste0("T1D", 1:5))]
  return(xdata)
}

# Merge with cdata
xdataMerge <- function(xdata, cvars) {
  if(!"donor.type" %in% cvars) cvars <- c(cvars, "donor.type")
  xdata <- merge(xdata, cdata[, c("ID", cvars), with = F], by = "ID", all.x = T, all.y = F)
  xdata[, donor.type := factor(donor.type, levels = c("No diabetes", "Autoab Pos", "T1D"))]
  xdata <- xdata[order(donor.type)]
  xdata[, ID := factor(ID, levels = ID)]
  xdata[, ID2 := factor(ID2, levels = ID2)]
  return(xdata)
}

# Return only genes that show significant differences in expression; number of genes significantly different


