# DIVE: Data Integration & Visual Exploration

DIVE is an R/Shiny application that provides a proof-of-concept framework for curating, integrating and visualizing heterogenous biological data that has been generated from the same collection of samples, such as a specialized biorepository that distributes cells or tissues to many independent investigators. Though this framework is meant to be re-purposeable as much as possible for different collections and use cases, we demonstrate the idea on data that has been painstakingly curated from the Network for Pancreatic Organ Donors (nPOD). Our motivating data comprises of research results from nPOD samples published over the last decade. This data provides a different value proposition to researchers who are not interested in the "methods" part of the package (i.e. re-using the framework); for them, we hope the package can be viewed as a way to conveniently explore and re-use data from nPOD. 

This is still a work in progress and more details will be available as the application is finishing up on development and testing. Investigators who'd like to contribute data or help us with testing can submit a note at the issues page (https://github.com/avucoh/DIVE/issues) or email <avu@coh.org>.

The current version of can be installed via `devtools::install_github("avucoh/DIVE")`

## Modules

DIVE is composed of five stand-alone modules, each with different purposes and methods for interfacing with the data, described below. The first three modules are more "major" modules, while the last two modules are more "minor" modules. Though the modules can be run independently, when put together they can serve as a well-rounded web data portal. 

### Cohort matching (Match App)

- Intended users: Both nPOD and non-nPOD investigators
- Description: Many experimental studies draw samples from multiple sources, and matching along certain features might be desired for more valid statistical results. The nPOD cohort of organ donors includes mostly T1D, T2D, non-diabetes, and a minority of other donor types. This module allows matching and comparison of an external cohort (or internally with an nPOD subset) to nPOD based on clinical data like disease, age, auto-antibody positivity, and more. Investigators can contact nPOD to request tissue samples of matches.  
- To start just this module at the console: `matchAppRun()`

### Exploration of data from small-scale experiments (interactiveMatrix App)

- Intended users: Primarily for nPOD investigators 
- Description: All data from small-scale experiment that we have cleaned and curated is placed into The Matrix, which supports simple interactive data comparisons through correlation analysis, drilldowns, and uploading data. It is meant to be the easiest and fastest way for an nPOD investigator to get started at looking at how their data might relate to others and generate new hypotheses.
- To start just this module at the console: `interactiveMatrixAppRun()`

### Exploration and learning from high-throughput phenotype data (multiVUI App)

- Intended users: Both nPOD and non-nPOD investigators
- Description: Going beyond the limitations of the interactiveMatrix module, this module extends data interaction to high-throughput data and better combines data of different sources/dimensions through an alternate layout and additional capabilities.
- To start just this module at the console: `multiVUIAppRun()`

### Browse and download (Browser App)

- Intended users: Both nPOD and non-nPOD investigators 
- Description: This module is for exploring and understanding the data through the metadata, giving interested researcher a better idea "what is out there" in terms of the data we have curated. Aside from the code that powers the application, much work has been put in to convert data into readable formats and annotate it with metadata to allow better searching and discovery. From this portal, researchers can download all or a select set of data.
- To start just this module at the console: `browseR()`

### Curate (Curater App)

- Intended users: Data curators and other data submitters (investigators)
- Description: So far this will simply be an interface for inputting metadata. Adding capabilities for tracking and generating statistics is being considered.
- To start just this module at the console: `curateR()`
