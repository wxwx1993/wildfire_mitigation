# How long does managed fire protect against catastrophic wildfires?

<img src="https://img.shields.io/badge/Study%20Status-Results%20Available-yellow.svg" alt="Study Status: Results Available"> 

This is the data repository for publicly available code and data to conduct analyses in the paper titled "The salience impact of fire trajectory on future catastrophic wildfires using satellite-based counterfactual analyses."

Analyzing the causal impacts of wildfire mitigation strategies in California.

<b>Code: </b><br>

1. [`data_processing`](https://github.com/wxwx1993/wildfire_mitigation/tree/main/data_processing) process downloaded spatial data from different sources into a tabular.

2. [`balancing`](https://github.com/wxwx1993/wildfire_mitigation/tree/main/balancing) apply covariate balancing synthetic control appraoche to obtain control weights to create "synthetic controls".

3. [`analysis`](https://github.com/wxwx1993/wildfire_mitigation/tree/main/analysis) conduct outcome analysis on covariate balanced data to generate results and graphs.

<b> Data Source: </b><br>

| Data    |  Sources      |  Spatial resolution  | Time resolution | Time periods
| ----------  | -------------------- |-----------------|-----------------|-----------------|
| Active Fires   | [`MODIS FIRMS`](https://firms.modaps.eosdis.nasa.gov/download/) |  1 km2        | daily | 11/01/2000 - |
| Meteorological | [`Daymet`](https://daymet.ornl.gov/) |  1 km2        | daily | 01/01/2000 - |
| Land Cover   | [`MCD12Q1 v006`](https://lpdaac.usgs.gov/products/mcd12q1v006) |  500 m2        | yearly | 2001 - |


<b>Data: </b><br>
All data needed to evaluate the conclusions in the paper are present in the paper and/or the Supplementary Materials and Online Repository. Those interested in the original data can contact the corresponding author.

All the analyses are run on Yen Servers with R programming at the Stanford Graduate School of Business. Computational Support was provided by the Data, Analytics, and Research Computing (DARC) group at the Stanford Graduate School of Business (RRID:SCR_022938).

R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"  
Copyright (C) 2022 The R Foundation for Statistical Computing  
Platform: x86_64-pc-linux-gnu (64-bit)  

<b>Terms of Use:</b><br>
Authors/funders retain copyright (where applicable) of code on this Github repo and the article. Anyone who wishes to share, reuse, remix, or adapt this material must obtain permission from the corresponding author.

<b>Contact Us: </b><br>
* Email: xw2892@cumc.columbia.edu, erikcs@stanford.edu, and swager@stanford.edu

<b>Acknowledgments</b><br>

We appreciate the work of Henry Zhu and Sitong Pan for preparing and processing the data set used for anaylsis. 

