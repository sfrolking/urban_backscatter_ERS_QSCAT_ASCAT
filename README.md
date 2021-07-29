# urban_backscatter_ERS_QSCAT_ASCAT
code to analyze urban backscatter

### Urban_microwave_backscatter_1993_2020

This repository contains the following R files and data files (csv); was generated in RStudio, Version 1.4.1106, on mac (High Sierra v.10.13.6)

## R-scripts

## make_ascat_validation_plots_stats_data_paper_1.R

* R Packages used: RColorBrewer, raster, Stats

* reads in csv files with backscatter data, building metrics data, and land cover data.

* generates a scatterplot of 2015 ASCAT mean summer backscatter vs. smoothed building volume (and height) for about 8500 urban 0.05° (lat/lon) grid cells in Europe, China, and the USA in about 215 major cities (see city_list_478_lat_lon.csv).

* fits a linear correlation to the scatterplots (using lm).

## make_invariant_summer_bakcscatter_time_series_plots_data_paper_1.R

* reads in csv files with backscatter data for four evergreen topical forest sites (11x11 grid of 0.05° lat/lon data).

* generates a mean backscatter for each of the four sites

* plots the time series.

* fits a linear trend line to data from each backscatter sensor (ERS: 1993-2000; QSCAT: 1999-2009; ASCAT: 2007-2020).

## make_summer_bakcscatter_time_series_plots_data_paper_2.R

* reads in csv files with backscatter data for twelve major cities (see city_list_12_lat_lon.csv), with data for an 11x11 grid of 0.05° lat/lon for each city.

* generates a mean backscatter for each of the cities, and plots the time series.

* fits a linear trend line to data from each backscatter sensor (ERS: 1993-2000; QuikSCAT: 1999-2009; ASCAT: 2007-2020).

## CSV files

* city_list_12_lat_lon.csv
* city_list_478_lat_lon.csv

* /invariant_grids_07_09_2021/Amazon1_invariant_grid.csv
* /invariant_grids_07_09_2021/Amazon2_invariant_grid.csv
* /invariant_grids_07_09_2021/DemRepCongo_invariant_grid.csv
* /invariant_grids_07_09_2021/PapuaNewGuinea_invariant_grid.csv

* /city_grids_wat_bldg_gfcc_07_09_2021/{cityname}_city_grid_wat_bldg_gfcc.csv

where {cityname} is one of 478 large cities (*note: not all 478 cities are used in these scripts*)

## Link to database of NetCDF files with global gridded urban seasonal backscatter data

* SEDAC URL

The data sets are currently under review at NASA’s Socioeconomic Data and Applications Center (SEDAC).  SEDAC has issued an interim URL, and if accepted will issue a permanent DOI.

## Manuscript in review at _Scientific Data_

Frolking, Steve, Tom Milliman, Richa Mahtta, Aaron Paget, David G. Long, Karen C. Seto. 2021-in review. A global urban backscatter time series data (ERS, QuikSCAT, ASCAT) set for 1993-2020. _Scientific Data_.
