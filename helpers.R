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

# ----

newOrderInput <- function(inputId, label, items,
                       as_source = FALSE, connect = NULL,
                       item_class = NULL,
                       placeholder = NULL,
                       width = "500px", ...) {
  if (is.null(connect)) {
    connect <- "false"
  } else {
    connect <- paste0("#", connect, collapse = ", ")
  }
  # item_class <- sprintf("btn btn-%s", match.arg(item_class))
  
  if (length(items) == 0 || (!is.vector(items) && !is.factor(items))) {
    item_tags <- list()
  } else {
    if (is.vector(items)) {
      item_values <- unlist(items, recursive = FALSE, use.names = TRUE)
      nms <- names(item_values)
      item_html <- `if`(
        is.null(nms) || any(nms == "") || any(is.na(nms)),
        item_values, nms
      )
    } else if (is.factor(items)) {
      item_values <- as.numeric(items)
      item_html <- as.character(items)
    }
    item_tags <- lapply(1:length(item_values), function(i) {
      tag <- shiny::tags$div(
        item_html[i],
        `data-value` = item_values[i],
        class = item_class, style = "margin: 1px"
      )
      if (as_source) {
        options <- list(connectToSortable = connect, helper = "clone", cancel = "")
        tag <- jqui_draggable(tag, options = options)
      }
      return(tag)
    })
  }
  
  style <- sprintf(
    "width: %s; font-size: 0px; min-height: 25px;",
    shiny::validateCssUnit(width)
  )
  container <- shiny::tagSetChildren(
    shiny::tags$div(id = inputId, style = style, ...),
    list = item_tags
  )
  if (!as_source) {
    cb <- "function(e, ui){if(!$(e.target).children().length)$(e.target).empty();}"
    func <- 'function(event, ui){
    return $(event.target).children().map(function(i, e){
    return $(e).attr("data-value");
    }).get();
  }'
    options <- list(
      connectWith = connect,
      remove = htmlwidgets::JS(cb),
      shiny = list(
        order = list(
          sortcreate = htmlwidgets::JS(func),
          sortupdate = htmlwidgets::JS(func)
        )
      )
      )
    container <- jqui_sortable(container, options = options)
    }
  
  if (!is.null(placeholder)) {
    css <- '#%s:empty:before{content: "%s"; font-size: 14px; opacity: 0.5;}'
    placeholder <- shiny::singleton(
      shiny::tags$head(
        shiny::tags$style(
          shiny::HTML(
            sprintf(css, inputId, placeholder)
          )
        )
      )
    )
  }
  
  shiny::tagList(
    placeholder,
    shiny::tags$label(label, `for` = inputId, `class` = "orderInput"),
    container
  )
}

matchUI <- function(cv, match.opts) {
  ix <- match(cv, names(match.opts))
  UIlist <- tagList()
  for(i in seq_along(cv)) {
    UIlist[[i]] <- list(newOrderInput(paste0("cv", ix[i]), HTML(paste(cv[i], "&rarr;")), items = match.opts[[ix[i]]], 
                                      connect = c(paste0("cv", seq_along(match.opts)), "cvbank"), 
                                      item_class = "btn btn-sm used covariate", width = 100, placeholder = "n/a"), br())
  }
  return(UIlist)
}

#-- Cohort data fusion -----------------------------------------------------------------------------------#

guessMatch <- function(v) {
  fs <- list(GADA.pos = "gad", IA2A.pos = "ia2", mIAA.pos = "mIA", ZnT8A.pos = "znt8", 
             AutoAb.count = "abcount", age = "age", BMI = "BMI", db.duration = "duration",
             age.onset = "onset", Cpeptide = "cpeptide", HbA1c = "hba1c", peak.gluc = "gluc",
             race_AfricanAmerican = "afr", race_AmericanIndian = "ind", race_Asian = "asian",
             race_Caucasian = "cau", race_Hispanic.Latino = "hisp", race_Multiracial = "multiracial",
             sex_Female = "female", sex_Male = "[^fe]male")
  res <- lapply(fs, function(f) sort(grep(f, v, val = T, ignore.case = T))[1])
  res[sapply(res, is.na)] <- list(NULL)
  return(res)
}

cohortFusion <- function(coh1, coh2, matchOn, cohnames = c("Cohort1", "Cohort2")) {
  coh1[, donor.type := cohnames[1]]
  coh2[, donor.type := cohnames[2]]
  setnames(coh2, old = matchOn, new = names(matchOn))
  fused <- rbind(coh1, coh2, use.names = T, fill = T)
  fused <- fused[ fused[, !Reduce(`|`, lapply(.SD, function(x) is.na(x))), .SDcols = names(matchOn)] ] # remove NAs
  fused <- fused[, c("ID", "donor.type", names(matchOn)), with = F]
  fused[, donor.type := factor(donor.type, levels = c(coh1$donor.type[1], coh2$donor.type[1]))]
  fused
}

Match2 <- function(ds, matchOn) {
  dataset <- copy(ds)
  dataset[, donor.type := as.integer(donor.type) - 1]
  matchformula <- as.formula(paste("donor.type", "~", paste(names(matchOn), collapse = " + ")))
  result <- matchit(matchformula, method = "nearest", replace = F, data = dataset, caliper = 0.2, ratio = 1)
  i1 <- as.numeric(result$match.matrix[, 1])
  i2 <- as.numeric(row.names(result$match.matrix))
  matched <- ds[c(i1, i2)]
  matched[, Match := c(ds[c(i2, i1), ID])]
  return(list(result = result, matched = matched))
}

#-- Correlations  ----------------------------------------------------------------------------------------#
# Remove data with 0 variance
rem0Var <- function(x) sd(na.omit(x)) != 0 

# Return data suitable for running correlations
data2cor <- function(cdata) {
  cor.data <- Filter(is.numeric, cdata) # remove nominal category variables
  cor.data <- cor.data[, !grepl("^ID|_SE$|_SEM$|_SD$", names(cor.data )), with = F] # _SE cols for error bars
  cor.data <- Filter(rem0Var, cor.data)
  corM <- cor(cor.data, use = "pairwise.complete.obs", method = "spearman")
  corN <- crossprod(as.matrix(cor.data[, lapply(.SD, function(x) as.integer(!is.na(x)))])) # n sample size
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
  xdata <- px2[Entrez %in% genes, c("6234", "6238", "6289", "6318", "6339", "6158", "6167", "6184", "6267", "6175", "6245", "6247", "6325", "Gene")]
  xdata <- xdata[!duplicated(Gene)] # TO DO: for other datasets?
  genes <- xdata$Gene
  if(!length(genes)) return(NULL)
  xdata <- xdata[, data.table(t(.SD), keep.rownames = T), .SDcols = 1:10]
  setnames(xdata, c("ID", make.names(genes)))
  xdata[, ID := as.numeric(ID)]
  xdata[, ID2 := c(paste0("HC", 1:5), paste0("AAB", 1:4), paste0("T1D", 1:4))]
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



