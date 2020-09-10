# Exclude values in dataset2 that are present in dataset1
excludePresent <- function(x = "ID", data1, data2) data2[[x]][!which(data2[[x]] %in% data1[[x]]), ]