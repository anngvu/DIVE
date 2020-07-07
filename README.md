# DIVE: Data Integration & Visual Exploration

DIVE is an R/Shiny application that provides a proof-of-concept framework for integrating and visualizing data that have been generated on the same collection of individuals or sites. In the context of biomedical research, one can think of independent studies that use biosamples from individuals within the same cohort. In the context of ecological research, one can think of heterogeneous measurements that were gathered at various times and/or by different groups on the same geographical sites. Though the components of this software are modular and generic enough to be re-purposeable for different collections and use cases, we optimize for the first context and demonstrate the idea with data from the Network for Pancreatic Organ Donors (nPOD) cohort. Our motivating data comprise of painstakingly curated published research on the nPOD donor samples published over the last decade. The instance of DIVE configured with nPOD data is branded nPOD DataView, accessible here (nPOD DataView requires DIVE to be installed, and then simply calls DIVE with the custom configurations and data objects). nPOD DataView is intended for diabetes researchers who want a "friendly" data portal. Other researchers/developers may be more interested in reusing or modifying the software components of DIVE to share their own data; feel free to contact me <avu@coh.org> if this is you and you want to ask about adapting DIVE.

This is still a work in progress and more details will be available as the application is finishing up on development and testing. Investigators who'd like to contribute data or help us with testing can submit a note at the issues page (https://github.com/avucoh/DIVE/issues).

The current version can be installed via `devtools::install_github("avucoh/DIVE")`

## Recent Updates



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

### Browser Module

- Intended users: Both nPOD and non-nPOD investigators 
- Description: This module is for exploring and understanding the data through the metadata, giving interested researcher a better idea of "what is out there" with a traditional summary view, metadata dictionary, and basic filter options.
- To start just this module at the console: `browseR()`

