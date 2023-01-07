# Instruction for Converting the Land Cover Data of California from 2001-2020 to a ".RDS" Format

This file is an instruction to acquire a ".RDS" format dataset of the California land cover information during the period of 2001-2020 from The Terra and Aqua combined Moderate Resolution Imaging Spectroradiometer (MODIS) Land Cover Type (MCD12Q1) Version 6 data product (https://lpdaac.usgs.gov/products/mcd12q1v006/).

## Step 1: Download the data

We can download the MCD12Q1 data via AppEEARS, which offers a simple and efficient way to access and transform geospatial data from a variety of federal data archives. (https://appeears.earthdatacloud.nasa.gov/) 

1. Sign in / sign up your NASA Earth science data account
2. Choose Extract Area Sample --- Start a new request
3. Draw a polygon around the California on the map or upload a shapefile of California.
4. Choose Date from 01-01-2001 to 12-21-2020
5. Choose product Combined MODIS Land Cover Type(MCD12Q1.006, 500m, yearly)
6. Select the layers in the data (LC_prop1 - LC_prop3, LC_Type1-LC_Type5, 8 variables in total)
7. Choose File Format: NetCDF-4, Projection: Geographic(Datum:WGS84,EPSG:4326,PROJ.4:+proj=longlat+datum=WGS84+no_defs)
8. Submit.
9. Find your request in Explore section of AppEEARS and download your ".nc" file dataset after 10 min


## Step 2: Process the data in R

1. Open processLandCover.Rmd file in RStudio
2. Set the value of "Dir" be the directory of the ".nc" file downloaded in Step 1
3. Set the value of "outDir" be the ideal directory of the output file
4. Run the code, then the file "LandCover.RDS" can be found under your outDir directory
(It should be a table with 162 columns and about 600,000 observations)