#-- SET-UP:libraries ------------------------------------------------------------------------------#

library(shiny)
library(visNetwork)
library(data.table)
library(DT)
library(ggplot2)
library(plotly)
# library(ggsci)

#-- SET-UP:data -----------------------------------------------------------------------------------#

load("Data/cdata.Rdata")
load("Data/correlations.Rdata")
load("Data/network.Rdata")
load("Data/gx.Rdata")
load("Data/px1.Rdata")
load("Data/px2.Rdata")
load("Ontology/GObranches.Rdata")
Columns <- fread("Data/Columns.txt")

#-- SET-UP:source ---------------------------------------------------------------------------------#

source("helpers.R")

#-- SET-UP:variables ------------------------------------------------------------------------------#

plotdata <- reactiveValues(corr = cor.data, cdata = cdata, newdata = NULL, corr.last.state = cor.data, 
                           drilldown = NULL, # pertains to "2D" view
                           genes = NULL, xdata = NULL) # pertains to "HD" view
ppColors <- c("Autoab Pos" = "orange", "Cystic fibrosis" = "aquamarine4", "Gastric Bypass" = "bisque4", 
              "Gestational diabetes" = "deeppink2", "Monogenic Diabetes" = "red4", "No diabetes" = "royalblue2", 
              "Other-Diabetes" = "indianred4", "Other-No Diabetes" = "steelblue2", "T1D" = "red",  
              "T1D Medalist" = "maroon", "T2D" = "mediumvioletred")
pal <- c("#A69EB0FF", "#00FF83FF", "yellow2", "darkorchid1", "royalblue1", "violetred1")


#--------------------------------------------------------------------------------------------------#