#' Create a custom cell pack widget based on d3 circlepack.js
#'
#' This html widget represents cells and tissues as circles placed in hierarchy.
#'
#' @param tab Table with data for widget.
#' @param entity Name of column containing relevant entity.
#' @param context Name of column containing context.
#' @export 
cellPackR <- function(tab, entity = "CellTissue", context = "CellTissueContext") {
    context_size <- tab[, .N, by = context]
    cl_part <- ontologyIndex::get_ontology("cl-release-2019-04-05.obo", propagate_relationships=c("part_of"))
    cl_is <- ontologyIndex::get_ontology("cl-release-2019-04-05.obo", propagate_relationships=c("is_a"))
    e <- cl_part$id[cl_part$name %in% unique(tab[[entity]])] # retrieve ontology ids from name
    ancestors <- lapply(e, function(x) cl_is$name[cl_is$ancestors[[x]]]) # get ancestor class of e
  }
}