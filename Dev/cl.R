library(data.table)
library(ontologyIndex)
library(ggsci)

# FUN ------------------------------------------

nest <- function(li, leaf) {
  if(length(li) == 1) return(setNames(list(leaf[[li]]), li)) 
  parent <- li[1]
  child <- li[-1]
  setNames(list(nest(child, leaf)), parent)
}

mergeLines <- function(l1, l2) modifyList(l1, l2, keep.null = T)

nestD3 <- function(parent, child) {
  if(inherits(child, "data.frame")) return(list(name = parent, color = context.colors[parent], children = child))
  list(name = parent, color = ifelse(length(child) > 1, context.colors[parent], "transparent"),
       children = mapply(nestD3, names(child), child, USE.NAMES = F, SIMPLIFY = F))
}

# FUN ------------------------------------------

# get_relation_names("cl.obo")
cl <- get_ontology("cl.obo", propagate_relationships=c("part_of"))
cl2 <- get_ontology("cl.obo", propagate_relationships=c("is_a"))

db <- fread("../DIVE/Data/Columns.txt")
context.size <- db[, .N, by = TissueContext]
cells <- cl$name[cl$name %in% unique(db$CellTissue)]

ancestors <- lapply(names(cells), function(x) cl2$name[cl2$ancestors[[x]] ])
ancestors <- sapply(ancestors, function(x) 
                    if(any(grepl("panc", x))) { "pancreas" } 
                    else if ("leukocyte" %in% x) { "leukocyte" }
                    else if ("cell" %in% x) { "other cell" }
                    else if (any(grepl("tissue|organ|structure", x))) { "tissue/organ" }
                    else { "other" })
table(ancestors)

colors <- rep("#BDBDBDFF", length(ancestors))
# expect warning since 14 colors -> 13 items for pancreas
colors[ancestors == "pancreas"] <- c(unlist(lapply(c("pink", "red"), function(p) pal_material(p)(10)[8:2])))
colors[ancestors == "leukocyte"] <- c(unlist(lapply(c("indigo", "blue"), function(p) pal_material(p)(10)[1:10 %% 2 == 0])), 
                                      pal_material("teal")(10)[1:10 %% 3 == 0])
colors[ancestors == "other cell"] <- pal_material("purple")(2)
colors[ancestors == "tissue/organ"] <- "transparent"


cell.context.size <- db[, .(size = .N), by = .(CellTissue, TissueContext)]
cell.context.size[, color := colors[match(CellTissue, cells)]]
setnames(cell.context.size, old = c("CellTissue", "TissueContext"), c("name", "context"))
cell.context.size <- split(cell.context.size, by = "context")

lines <- cl$id[cl$name %in% unique(db$TissueContext)]
lines <- lapply(lines, function(x) cl$name[cl$ancestors[[x]]])
# group lines into those with intersecting ancestors
pancreas <- lines[sapply(lines, function(x) ("pancreas" %in% x) & length(x) > 1)]
pancreas <- lapply(pancreas, function(x) x[x != "endocrine system"]) # start from pancreas
names(pancreas) <- sapply(pancreas, last)

hemolymphoid <- lines[sapply(lines, function(x) any(grepl("hemolymphoid", x)))]
names(hemolymphoid) <- sapply(hemolymphoid, last)
other <- lines[sapply(lines, function(x) !any(grepl("pancreas|hemolymphoid", x)))]
names(other) <- sapply(other, last)


hierarchy <- c(Reduce(mergeLines, lapply(pancreas, function(x) nest(x, leaf = cell.context.size))), 
               Reduce(mergeLines, lapply(hemolymphoid, function(x) nest(x, leaf = cell.context.size))),
               Reduce(mergeLines, lapply(other, function(x) nest(x, leaf = cell.context.size)))) 

context.colors <- list(unique(unlist(pancreas, use.names = F)),
                       unique(unlist(hemolymphoid, use.names =F)),
                       unique(unlist(other, use.names = F)))
context.colors <- mapply(function(x, p) setNames(pal_material(p)(length(x)), x), context.colors, list("yellow", "red", "brown"))
context.colors <- unlist(context.colors)

test <- jsonlite::toJSON(nestD3("root", hierarchy), auto_unbox = T)
r2d3::r2d3("cellpack.js", data = test, d3_version = 4)


