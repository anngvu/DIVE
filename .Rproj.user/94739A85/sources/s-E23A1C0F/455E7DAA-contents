library(data.table)

# Assign a group (factor)
makeGroupLabels <- function(x) {
  modified <- fread(x, colClasses = c("RefDye" = "character", "Comment" = "character", "Group" = "character", "Batch" = "character", 
                                      "Node" = "character", "NodeFunction" = "character"))
  modified[Ignore == 1, Group := ""]
  modified[Ignore == 0 & isCTRL == 0, Group := LETTERS[factor(paste(BSMDCD, BSM, Node, NodeFunction))], by = .(GSE, BioSampName, XP)]
  modified[Ignore == 0 & isCTRL == 1, Group := letters[factor(paste(BSMDCD, BSM, Node, NodeFunction))], by = .(GSE, BioSampName, XP)]
  modified
}

makeGroupKey <- function(dt) {
  dt <- dt[Ignore != 1]
  key <- unique(dt[, .(GSE, Group, xpType, XP, Node, NodeFunction, BSM, BSMDCD, BSMDCD2, BioSampName)])
  key
}

# Formatted pasting of contrast names
fpaste <- function(A, a) {
  A <- A[names(A) != "BSMDCD2"] # ignore concentration info
  a <- a[names(a) != "BSMDCD2"]
  constant <- which(A == a)
  var <- which(A != a)
  # Possible to do: parse BSM/Node to better note constants
  s <- paste(paste(A[var], collapse = " "), "vs", paste(a[var], collapse = " "), "|", paste(A[constant], collapse = " "))  
  s <- gsub("\\s+", " ", trimws(s))
}

inferContrasts <- function(dt) {
  if(nrow(dt) == 1) {
    contrast <- c(Contrast = "", Formula = "", dt[, .(Node, NodeFunction, BSM, BSMDCD, BSMDCD2)])
  } else if(nrow(dt) == 2) {
    group <- sort(dt$Group, decreasing = T)
    formula <- paste(group, collapse = "-")
    A <- dt[Group == group[1], .(Node, NodeFunction, BSM, BSMDCD, BSMDCD2)]
    a <- dt[Group == group[2], .(Node, NodeFunction, BSM, BSMDCD, BSMDCD2)]
    contrast <- fpaste(unlist(A), unlist(a))
    contrast <- c(Contrast = contrast, Formula = formula, as.list(A))
  } else {
    Node <- unique(unlist(strsplit(dt$Node, ";")))
    Node <- Node[Node != "(WT)"]
    BSM <- unique(unlist(strsplit(dt$BSM, ";")))
    BSM <- BSM[BSM != "(Veh)"]
    DCD <- unique(unlist(strsplit(dt$BSMDCD, ";")))
    allF <- c(Node, BSM, DCD)
    cX <- lapply(c(Node, BSM), function(x) allF == x) # there will always be either a Node or BSM, so not necessary to check cX 
    cT <- lapply(DCD, function(x) allF == x) # sometimes there isn't cT -> list()
    x0 <- x1 <- x2 <- list()
    if(length(Node)) x0 <- setNames(lapply(Node, function(f) grepl(f, dt$Node)), Node)
    if(length(BSM)) x1 <- setNames(lapply(BSM, function(f) grepl(f, dt$BSM)), BSM)
    if(length(DCD)) {
      x2 <- setNames(lapply(DCD, function(f) grepl(f, dt$BSMDCD)), DCD)
      cT <- suppressWarnings(mapply(function(v1, v2) setNames(v1 | v2, allF[v1]), cX, cT, SIMPLIFY = F)) # Expect arguments to be recycled
    }
    d <- as.data.frame(c(x0, x1, x2), optional = T)
    # Calculate the results of different contrast combinations
    m <- as.matrix(d)
    pairs <- combn(nrow(m), 2)
    pairs <- cbind(pairs, pairs[2:1, ])
    res <- Map(function(i, j) m[i, ] - m[j, ], pairs[1, ], pairs[2, ])
    # Find which contrast combinations isolate the desired variable 
    valid <- lapply(c(cX, cT), function(x) as.matrix(pairs[, sapply(res, function(y) all(y == x))]))
    npairs <- sapply(valid, ncol)
    effectVar <- unlist(mapply(rep, c(Node, BSM, names(cT)), npairs))
    effectNode <- effectBSM <- effectVar
    effectNode[!effectVar %in% Node] <- "" 
    effectBSM[!effectVar %in% BSM] <- ""
    valid <- do.call(cbind, valid)
    A <- dt[valid[1, ], .(Node, NodeFunction, BSM, BSMDCD, BSMDCD2)]
    a <- dt[valid[2, ], .(Node, NodeFunction, BSM, BSMDCD, BSMDCD2)]
    formula <- mapply(function(g1, g2) paste(dt[g1, "Group"], dt[g2, "Group"], sep = "-"), valid[1, ], valid[2, ])
    contrast <- mapply(function(g1, g2) fpaste(unlist(g1), unlist(g2)), split(A, 1:nrow(A)), split(a, 1:nrow(a)))
    contrast <- list(Contrast = contrast, Formula = formula, Node = effectNode, 
                NodeFunction = A$NodeFunction, BSM = effectBSM, BSMDCD = A$BSMDCD, BSMDCD2 = A$BSMDCD2)
  }
    len <- length(contrast$Contrast)
    metadata <- lapply(c(dt$GSE[1], dt$BioSampName[1], dt$xpType[1], dt$XP[1]), function(x) rep(x, each = len))
    names(metadata) <- c("GSE", "BioSampName", "xpType", "XP")
    contrast <- c(contrast, metadata)
    return(contrast)
}

# Wrapper
makeContrasts <- function(dt) {
  key <- makeGroupKey(dt)
  dtlist <- split(key, by = c("GSE", "BioSampName", "xpType", "XP"))
  results <- lapply(dtlist, function(x) try(inferContrasts(x)))
  ok <- allOK(results)
  results <- results[ok]
  results <- rbindlist(results)
  results
}
