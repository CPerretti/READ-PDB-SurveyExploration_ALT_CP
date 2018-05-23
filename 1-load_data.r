# NOTE: set working directory in RStudio Session to Source File Location before starting

# load data to be used

# first get the tow evaluation data from all valid survey tows
# use SAGA for Academy Eel (first in list, doesn't matter but this gives 0 catch for all stations)
# for years 2009-2017  in both both spring and fall master data (Status Code 10)
# for Purpose Code 10 (NMFS NEFSC Bottom Trawl Survey)
# select all strata starting with "01" or "03" (154 strata selected)
# use default TOGA Specifications (132X)
# save sql as all_strata_2009_2017.sql
# go to Tow Evaluation Report Window 
# copy Tow Evaluation grid (5897 records into tow_evaluation_all_strata_2009_2017.csv)
# read in tow data
tow <- read.csv("data/tow_evaluation_all_strata_2009_2017.csv", header=TRUE)

# merge data for GIS (don't apply conversions cuz no catch of Academy Eel)
# merge data for input to Survan (no problems found because catch always zero)
# save GIS file as all_strata_2009_2017_gis.csv
# read in gis data (need to get season)
gis <- read.csv("data/all_strata_2009_2017_gis.csv", header=TRUE)

# get csv file detailing the ADIOS files used for stocks
adios.data <- read.csv("data/adios_data.csv", header = TRUE, stringsAsFactors = FALSE)

# define standard tow wing swept area (a = d * w)
# d = distance of tow = 1.852 kilometers (= 1 nautical mile, based on 20 minutes at 3 knots per hour)
# w = wing width = 12.6 meters
# a = wing swept area = w * d square kilometers = 0.0233352
standard_wing_swept_area = 1.852 * 12.6 / 1000
