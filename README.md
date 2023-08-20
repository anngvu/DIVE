# DIVE: Data Integration & Visual Exploration

DIVE is an R/Shiny application that provides a proof-of-concept framework for integrating and visualizing data that have been generated on the same collection of individuals or sites. In the context of biomedical research, one can think of independent studies that use biosamples from individuals within the same cohort. In the context of ecological research, one can think of heterogeneous measurements that were gathered at various times and/or by different groups on the same geographical sites. 

Though the components are designed to be modular and generic enough to be re-purposeable for different data collections, DIVE is optimized for the first context and demonstrate the concept with data from the [Network for Pancreatic Organ Donors (nPOD)](https://www.jdrfnpod.org/) cohort. The motivating data comprise of painstakingly curated published research on the nPOD donor samples published over the last decade. **DIVE can be configured with different instance data; when used with the nPOD data it is branded _nPOD DataView_**, which requires DIVE to be installed, and then simply calls DIVE with custom configurations and data objects. nPOD DataView is intended for researchers (biologists) who want a "friendly" data portal. **Thus, to see DIVE in action with the real data, go to [DataView](https://gitlab.com/npod/dataview).** 

Others in the scientific community (developers and bioinformaticians) may be more interested in reusing or extending the code of DIVE for their own application.

This is still a work in progress and more details will be available as the application is finishing up on development and testing. To request a feature or file bugs: [submit an issue](https://github.com/anngvu/DIVE/issues).

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

[![R build status](https://github.com/anngvu/DIVE/workflows/R-CMD-check/badge.svg)](https://github.com/anngvu/DIVE/actions)
<!-- badges: end -->

## Installation 

The current version can be installed via `devtools::install_github("anngvu/DIVE")`


## Modules

DIVE is composed of four stand-alone modules, each with different purposes and methods for interfacing with data, described below. The modules can be run independently or put together as part of the same web application. 

### Match Module (CohortXchange)

- Intended users: nPOD investigators and non-nPOD diabetes researchers
- Description: Experimental studies can draw samples from multiple cohorts, and matching along certain features might be desired for more valid statistical results. The nPOD cohort of organ donors includes mostly T1D, T2D, non-diabetes, and a minority of other donor types. This module allows matching and comparison of nPOD donors to an external cohort (or internally for a subset of nPOD donors) using clinical and demographic variables like disease status, age, sex. Investigators can then contact nPOD to check for availability of matching tissue samples.  
- To start just this module at the console: `matchAppRun()`

### Matrix Module

- Intended users: More interesting for nPOD investigators who have already generated data or are planning to generate nPOD data 
- Description: All data from small-scale experiment that we have curated and standardized is placed into The Matrix, which supports interactive data comparisons through correlation analysis, drilldowns, and custom data uploads. This is the easiest and fastest way for an nPOD investigator to get started at looking at how data might relate across different studies and to generate new hypotheses.
- To start just this module at the console: `interactiveMatrixAppRun()`

### MultiVUI Module

- Intended users: nPOD investigators and non-nPOD diabetes researchers
- Description: Going beyond the limitations of the Matrix Module, this module extends data interaction to high-throughput data and better combines data of different sources/dimensions through an alternate layout and additional capabilities.
- To start just this module at the console: `multiVAppRun()`

### Data Helper Module

- Intended users: Both nPOD and non-nPOD investigators 
- Description: This module is for exploring and understanding the data through the metadata, giving interested researcher a better idea of "what is out there" with a traditional summary view, metadata dictionary, and basic filter options.
- To start just this module at the console: `browseR()`

