# README file for git repository bridge.collapses

## Contents
1. Raw data from public and private sources related to collapsed bridges in the US
    - The NYSDOT database of "failed" (collapsed) bridges
    - The USDOT 2017 gis version of the US National Bridge Inventory
    - USGS station and streamflow data of various kinds
    - Related data on dams, hurricanes, etc.
2. Processed data from above in the form of .RData
3. R scripts compatible through R 3.2.1 and mostly compatible through 3.5.2 for
    - Loading and processing the raw data
    - Creating input and reading output from other software (e.g., software for flood frequency analysis)
    - Performing statistical and other analyses in R
    - Plotting results
4. R markdown notebooks for running performing a series of analyses

A related  repository is available at https://code.vt.edu/mflint/Flint.et.al.2016.Historical.Hydraulic.Bridge.Collapses

## Naming convention:
Derived (analysis) variable names are determined in the order followed, linked by underscores ("_")
  - Variable type: 
	- Q: flow or discharge, cfs
	- T: return period, years
	- DATE: YYY-MM-DD 
	- BOOL: TRUE/FALSE  or factor 
    - COMMENT: string
	- COUNT: count, integer
  - Event category: 
	- FAIL
	- FAILPM2: fail +/- 2 days
	- MAX: max recorded
	- MAXPREFAIL: max recorded up to and including failure date in overlapping existence of bridge and gauge
    - MAXFAILYEAR: maximum in year of failure
	- FAIL_05: 5% confidence interval for failure
	- FAIL_95: 95% confidence interval for failure
  - Data source: 
	- D: daily mean
	- I: instantaneous
	- P: peak, usually instantaneous
	- IP: instantaneous and/or peak
  - Analysis type:
	- HECD: HEC-SSP Bulletin 17B analysis using daily mean data
	- HECP: HEC-SSP Bulletin 17B analysis using annual peaks data
	- PARTDUR: partial duration analysis (uses daily mean data)
  - Data source:
	- USGS

Most other variable names are taken directly from Fail, NBI, or USGS databases

Column descriptions: see tab-delimited "df.Fail.NBI.Gage.Colnames.txt"



