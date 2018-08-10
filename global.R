#-- SET-UP:libraries ------------------------------------------------------------------------------#

library(shiny)
library(visNetwork)
library(data.table)
library(DT)
library(ggplot2)
library(plotly)

#-- SET-UP:data -----------------------------------------------------------------------------------#

load("Data/cdata.Rdata")
load("Data/dtM.Rdata")
load("Data/accM.Rdata")
load("Data/network.Rdata")
load("Data/gx.Rdata")
load("Data/px1.Rdata")
load("Data/px2.Rdata")
load("Ontology/GOBP.Rdata")
corM <- fread("Data/corM.txt")
Columns <- fread("Data/Columns.txt")

#-- SET-UP:source ---------------------------------------------------------------------------------#

source("helpers.R")

#-- SET-UP:variables ------------------------------------------------------------------------------#

plotdata <- reactiveValues(corr = corM, acc = cdata, newdata = NULL, corr.last.state = corM, 
                           drilldown = NULL, # pertains to "2D tab"
                           xgenes = NULL) # pertains to "3D+" tab
fvars <- c("CR.gender", "CR.ethnic", "CR.COD", "CR.ABO") ## TO DO: Should already have data factored in table
cdata.vars <- names(cdata)[!names(cdata) %in% c("ID", "donorType", fvars)]

ppColors <- c("Autoab Pos" = "orange", "Cystic fibrosis" = "aquamarine4", "Gastric Bypass" = "bisque4", 
              "Gestational diabetes" = "deeppink2", "Monogenic Diabetes" = "red4", "No diabetes" = "royalblue2", 
              "Other-Diabetes" = "indianred4", "Other-No Diabetes" = "steelblue2", "T1D" = "red",  
              "T1D Medalist" = "maroon", "T2D" = "mediumvioletred")
names(ppColors) <- levels(cdata$donorType)
pal <- c("#A69EB0FF", "#00FF83FF", "yellow2", "darkorchid1", "royalblue1", "violetred1")


#--------------------------------------------------------------------------------------------------#