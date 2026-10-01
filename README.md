# Latitudinal Climate Impacts
[![DOI](https://zenodo.org/badge/1171103415.svg)](https://doi.org/10.5281/zenodo.23074418)
Code for an AmNat perspective on latitudinal gradients in climate change impacts. Early conclusions that high exposure to climate change would center impacts at high latitudes were reversed by the sensitivity of tropical organisms. New research considering climate variability suggests mid-latitude vulnerability.

# GENERAL INFORMATION

This README.md file was updated on September 30, 2026 by Lauren Buckley

## A. Paper associated with this archive 
Citation: Buckley LB. Climate variability and extremes flatten latitudinal clines in organismal sensitivity and exposure to climate change. American Naturalist

Abstract: Emerging research accounting for climate variability and extremes spotlights the vulnerability of mid-latitude ecosystems to climate change. Mid-latitude organisms are relatively sensitive to their environments yet exposed to substantial environmental variation. Components of organismal sensitivity that vary latitudinally include thermal specialization and tolerance, the potential for environmental tracking and evolutionary responses, and the temperature sensitivity of biological rates. Anticipating the biodiversity impacts of climate change across latitudes will require investigating how environmental variation and biological responses at multiple timescales integrate over lifecycles to shape fitness outcomes. 

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
Code downloads the following environmental data: US National Centers for Environmental Information (NCEI) Global Surface Summary of the Day (GSOD)

## 3. Recommended citation for this data/code archive
Buckley LB. Climate variability and extremes flatten latitudinal clines in organismal sensitivity and exposure to climate change. https://github.com/lbuckley/LatitudinalClimateImpacts/. 

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

