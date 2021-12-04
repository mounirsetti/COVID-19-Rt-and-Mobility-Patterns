# COVID-19-Rt-and-Mobility-Patterns
## In-Depth Correlation Analysis of SARS-CoV-2 Effective Reproduction Number with Mobility Patterns Identifies Three Groups of Countries
### Mounir Ould Setti & Sylvain Tollis

This repository is meant to host the programming script used for the analysis of the study with the abovementioned title published by the Journal of Preventive Medicine and Public Health.

We analyzed how mobile phone mobility data gathered and aggregated by Google align with daily changes of the effective replication number in 125 countries and 50 regions.
Three groups of countries were identified based on their patterns of correlation between the effective reproduction number of SARS-CoV-2 and the mobility indicators. We found that, in some countries, mobility restrictions could worsen disease spread and that different levels of mobility restrictions might have varying effects depending on regional specificities.

The script is meant to work on R programming language version 4.04 (https://www.R-project.org).
The script has lines where it calls for data from open access repositories with files of sizes > 1 GB, which might take time, depending on your internet speed.

Please note that the script was initially meant for internal use only. The decision to make it open access was based on peer-review and editorial suggestion. Therefore, we have not adequately adapted our script for public use. For instance, some of the libraries called at the beginning of the script might not be really needed.

**Important**
- You would need to create empty subfolders for the script to export its outputs, namely, "/Export", "/Export/ResidMobil5a", "/Export/ResidMobil5b", "/Export/ScatterPlots5a", etc.
- A colleague has done some small operations, outside of R environment, on the regional data such as removing very small US States. Please also find in this repository the files "usa2_infinitesimalStatesRemoved.csv" and "usa2_PuertoRicoOnly.csv", which you may need to run the R code.
- Please cite our paper mentioned above if you use our script or a part of it.



