#' Create a custom cell pack widget based on d3 circlepack.js
#'
#' A wrapper to produce an html widget representing cells and tissues as circles placed in hierarchy.
#'
#' @param tab Table with data for widget.
#' @param entity Name of column containing relevant entity.
#' @param context Name of column containing context.
#' @param nested Deliminater for nested rows, defaulting to "|".
#' @export
cellPackR <- function(tab, entity = "CellTissue", context = "CellTissueContext", nested = "|",
                      cl = system.file("ontology/cl_2018-11.obo", package = "DIVE"), rel_type = "is_a",
                      ancestor_classes, class_colors, corrections = NULL, contextcolors) {
  dt <- myd3data(tab, entity, context, nested)
  e_ancestry <- getAncestry(unique(dt$name), cl, rel_type)
  classes <- reduceClass(e_ancestry, ancestor_classes, corrections = corrections)
  e_summary <- colorByClass(dt, e_ancestry, classes, class_colors)
  context <- fillContext(unique(dt$context), cl)
  dtree <- inContext(context, e_summary)
  d3json <- data2d3(dtree)
}

# view ancestor relations
# onto_plot(cl_part, terms = get_ancestors(cl_part, e), fontsize = 120)
# onto_plot(cl_, terms = remove_links(cl_, get_ancestors(cl_, e)), fontsize = 120)
myd3data <- function(tab, entity, context, nested, cl, rel_type) {
  dt <- data.table::fread(tab)
  dt <- dt[, c(entity, context), with = F]
  data.table::setnames(dt, c(entity, context), c("name", "context"))
  dt <- na.omit(dt)
  dt <- dt[, name := strsplit(name, nested, fixed = T)]
  dt <- dt[, .(name = unlist(name)), by = .(context)] # un-nest table
  return(dt)
}

getAncestry <- function(e, cl, rel_type) {
  cl_ <- ontologyIndex::get_ontology(cl, propagate_relationships = rel_type)
  in_e <- cl_$name %in% e
  missing <- length(e) - table(in_e)["TRUE"]
  if(missing) cat(paste(missing, "terms in tab did not exist in the ontology"))
  e <- cl_$id[in_e] # retrieve ontology ids from name
  e_ancestry <- lapply(e, function(x) cl_$name[cl_$ancestors[[x]]]) # get ancestor type for e
  return(e_ancestry)
}


#' Reduce Class
#'
#' An entity is reduced to a specific (ancestor) class for coloring,
#' usually based on the topmost ancestor of either the "part_of" or "is_a" relationship.
#'
#' For instance, an ancestor A is assigned color "red", entities descended from A will be assigned some color of red hue.
#' Which ancestors and the hue color associated with each must all be manually specified.
#' The order of values (ancestors) in classes matters; when an entity inherits from two or more classes
#' but only one label can be used, the ancestor class that takes precedence should be listed first.
#' When values in e_classes may be inaccurate due to errors in the ontology, or for special cases,
#' one can specifically assign classes using the corrections parameter.
#'
#' @param e_classes List of ancestor class by entity.
#' @param classes A vector containing ancestor classes to look for and to for li.
#' @param other Catch-all label to give to anything outside of what's given in index_classes.
#' @param corrections A named vector of
reduceClass <- function(e_classes, index_classes, other = "other", corrections = NULL) {

  reduced <- Reduce(function(e_classes, classx) lapply(e_classes, function(ancestors) if(classx %in% ancestors) classx else ancestors),
         index_classes, init = e_classes)
  if(length(corrections)) for(id in names(corrections)) reduced[[id]] <- corrections[id]
  reduced <- lapply(reduced, function(x) if(length(x) > 1) other else x) # apply catch-all lable to non-reduced entities
  return(reduced)
}

#' Color By Class
#'
#' @param dt A data.table with entities for which a color column will be added.
#' @param e_ancestry A list of ancestor classes by entity.
#' @param indexed A list of the same length as e_ancestry, treated as factors and used for splitting e_ancestry into defined classes.
#' @param colors A vector of the same length as unique values in indexed, mapping classes to specific color hues.
#' If not specified, random colors are assigned; note that because there are only eight different hues available
#' c("red", "orange", "yellow", "green", "blue", "purple", "pink", "monochrome"),
#' when there are more than eight classes, some classes will necessarily share the same hue.
colorByClass <- function(dt, e_ancestry, indexed, colors = NULL) {
  e_by_class <- split(sapply(e_ancestry, last), f = unlist(indexed))
  if(!length(colors)) colors <- rep_len(c("red", "green", "blue",  "orange", "purple", "pink", "yellow", "monochrome"), length(e_by_class))
  colors <- Map(function(li, hue) randomcoloR::randomColor(length(li), hue), e_by_class, colors)
  colors <- setNames(unlist(colors), unlist(e_by_class))
  dt$color <- colors[dt$name]
  dt <- dt[, .(size = .N), by = .(name, context, color)]
  return(dt)
}



# Because the index doesn't necessarily give complete line of ancestors for all entities,
# either because of missing inferred axioms in the exported ontology or quirks in ontologyIndex,
# we have to manually check and assemble complete hierarchy for context terms,
# first by making sure that no "top" level appears as a lower level in another's lineage,
# which means that it is not actually the top level globally.
fillContext <- function(e, cl, rel = "part_of") {
  cl_ <- ontologyIndex::get_ontology(cl, propagate_relationships = rel)
  lineages <- cl_$id[cl_$name %in% e] # extract ontology entity ids
  lineages <- lapply(lineages, function(x) cl_$name[cl_$ancestors[[x]]]) # look up ancestors with ids
  names(lineages) <- sapply(lineages, last) # name lineage list by entity
  families <- setNames(lineages, sapply(lineages, first)) # name lineage list by ancestor
  extension <- lapply(unique(names(families)), function(l) lapply(lineages, function(x) if(x[1] == l) {
    character(0)
  } else {
    ind <- match(l, x, nomatch = 0L)
    if(ind) x[1:(ind-1)] else character(0)
  }
  ))
  names(extension) <- unique(names(families))
  lineages_full <- Map(extendLine, split(families, names(families))[names(extension)], extension)
  lineages_full <- unlist(lineages_full, recursive = F, use.names = F)
  return(lineages_full)
}

# Once the context have a harmonized hierarchy, add the data to the leaf levels and restructure
inContext <- function(context, data) {
  data <- split(data, by = "context")
  dtree <- lapply(context, function(l) nest(l, data))
  dtree <- Reduce(function(l1, l2) modifyNestedContext(l1, l2), dtree)
  return(dtree)
}

# A modified version of base::modifyList to avoid separating data.frames and keep.null with TRUE as default
modifyNestedContext <- function(x, val) {
  stopifnot(is.list(x), is.list(val))
  xnames <- names(x)
  vnames <- names(val)
  vnames <- vnames[nzchar(vnames)]
  if(is.data.frame(val)) {
    x[["."]] <- val
  } else {
    for (v in vnames) {
      x[v] <- if (v %in% xnames && is.list(x[[v]]) && is.list(val[[v]]))
        list(modifyNestedContext(x[[v]], val[[v]]))
      else val[v]
    }
  }
  x
}


#' Extends the lineage "history" to match global lineage determination.
#'
#' @param family List containing values within a conceptual group.
#' @param extension A list of possible extensions to prepend to items in family. The longest extension will be used.
extendLine <- function(family, extension) {
  longest <- which.max(lengths(extension))
  ext <- extension[[longest]]
  if(!length(ext)) return(family)
  family <- lapply(family, function(i) append(ext, i))
  return(family)
}

#' Given a vector values in some hierarchical order (e.g. describing classes or lineages)
#' and data that goes under certain of these values,
#' this function recursively re-structures the hierarchical values into lists
#' and puts data into the last matching level
#'
#' @param v A vector of values to be nested, with the order corresponding to nesting levels.
#' @param leaf A named list containing data to be placed at the leaf level, with names matching the values in v.
#' @return A nested list.
nest <- function(v, data) {
  if(length(v) == 1) {
    return(setNames(list(data[[v]]), v))
  }
  parent <- v[1]
  child <- v[-1]
  setNames(list(nest(child, data)), parent)
}

#' Modify nested list to make a version better corresponding to desired d3 json.
#'
#' @param context Name of the context.
#' @param child A list containing nested lists or data in a data.frame/data.table. See details.
#' @param contextcolors Optional, a named character vector mapping levels of the hierarchy to specific colors.
#' If not given, the context background will transparent.
#' @return A list that can be converted to correctly formatted json for use with cellpack.js
nestD3 <- function(context, child, contextcolors = NULL) {
  if(inherits(child, "data.frame")) {
    return(list(name = context,
                color = if(is.null(contextcolors[context]) || is.na(contextcolors[context])) "transparent" else contextcolors[context],
                children = child))
  } else {
    list(name = context,
         color = if(is.null(contextcolors[context]) || is.na(contextcolors[context])) "transparent" else contextcolors[context],
         children = mapply(nestD3, names(child), child, MoreArgs = list(contextcolors = contextcolors), SIMPLIFY = F, USE.NAMES = F)
         )
  }
}

data2d3 <- function(dtree, visualize = F, contextcolors = NULL) {
  d3data <- nestD3("root", dtree, contextcolors)
  d3data <- jsonlite::toJSON(d3data, auto_unbox = T, null = "null")
  if(visualize) r2d3::r2d3("cellpack.js", data = d3data, d3_version = 4) else d3data
}
