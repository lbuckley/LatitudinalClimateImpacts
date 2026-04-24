# Latitudinal Climate Impacts
Code for an AmNat perspective on latitudinal gradients in climate change impacts 

# GENERAL INFORMATION

This README.md file was updated on April 24, 2026 by Lauren Buckley

## A. Paper associated with this archive 
Citation: Buckley LB. Climate variability and extremes flatten latitudinal clines in climate change impacts. American Naturalist

Introduction: Biological impacts of climate change were initially anticipated to be concentrated at higher latitudes where environmental warming is strongest. Subsequent studies emphasized the need to consider organismal sensitivity, not exclusively environmental exposure. Many tropical species are thermal specialists due to evolving in aseasonal climates and might experience more thermal stress despite mild tropical warming. Documentation of tropical impacts is sparse but accumulating. Initial characterizations of organismal sensitivity relied on constant temperature experiments, but organisms are highly sensitive to climate extremes in their variable natural environments. New environmental data and experiments incorporating realistic environmental variability are challenging current understanding of organismal sensitivity across latitudes. Greater temperature variability and more frequent extremes (and their greater increases with warming) can accentuate temperate-zone impacts, even given lesser sensitivity to warming. Here we revisit latitudinal clines in environmental exposure and organismal sensitivity with a focus on temporal environmental variability. 

## B. Originators
Lauren B. Buckley, Department of Biology, University of Washington, Seattle, WA 98195-1800, USA

## C. Contact information
Lauren Buckley
Department of Biology, University of Washington, Seattle, WA 98195-1800, USA
lbuckley@uw.edu

## D. Dates of data collection
No new data are collected, but see script for download of environmental data. 

## E. Geographic Location(s) of data collection
No new data are collected.

## F. Funding Sources 
This research was supported by the US National Science Foundation (IOS- 2222089 to LBB).

# ACCESS INFORMATION

## 1. Licenses/restrictions placed on the data or code
CC0 1.0 Universal (CC0 1.0)
Public Domain Dedication

## 2. Data derived from other sources
Code downloads the followign environmental data: US National Centers for Environmental Information (NCEI) Global Surface Summary of the Day (GSOD)

## 3. Recommended citation for this data/code archive
Buckley LB. Climate variability and extremes flatten latitudinal clines in climate change impacts. https://github.com/lbuckley/LatitudinalClimateImpacts/. 

Data and code will be archived in Zenodo upon acceptance.

# DATA & CODE FILE OVERVIEW

This data repository consist of 1 code script, and this README document.

## Data files and variables
NA

## Code scripts and workflow
1. Figure1.R: code for producing figure 1.

# SOFTWARE VERSIONS
R version 4.3.1 (2023-06-16)

Packages: #utils::sessionInfo()

library(GSODR) #GSODR_4.1.4

library(dplyr) #dplyr_1.1.2

library(tidyr) #tidyr_1.3.0 

library(ggplot2) #ggplot2_3.5.2

library(TrenchR) #TrenchR_1.1.1

library(patchwork) #patchwork_1.2.0.9000

library(mgcv) #mgcv_1.8-42

library(purrr) #purrr_1.0.2

# REFERENCES
Gillooly, J.F., Brown, J.H., West, G.B., Savage, V.M. and Charnov, E.L., 2001. Effects of size and temperature on metabolic rate. Science, 293(5538):2248-2251.

