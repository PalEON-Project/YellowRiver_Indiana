FILENAME: 
FIA_dbh_composition.csv

This file has FIA tree species and their dbh per FIA plot for plots in the Yellow River watershed.

********
HEADER:

If you have any questions about the data in general or an entry specifically, email peters.63@nd.edu.
1. ID - unique identifier that corresponds to the plt_cn but is not as long or unweildy
2. Buffer - what side of the midline each plot lands. Oak = west of the line, Other = east of the midline
3. FIAspecies - scientific species names that correspond to the FIA spcd
4. PalEON_L3 - PalEON level 3 taxa that lumps the species to a standardized level that is used across the PalEON project (www.paleonproject.org)
5. time - year plot was surveyed ÒMEASYEARÓcolumn from FIA plot table. According to the FIADB User Guide: ÒMeasurement year. The year in which the plot was completed. MEASYEAR may differ from INVYEARÓ
6. statecd - FIA state code. 18 = Indiana
7. dbh - diameter at breast height in centimeters
8. statuscd - FIA status code. A code indicating whether the sample tree is live, cut, or dead at the time of measurement. Includes dead and cut trees, which are required to estimate aboveground biomass and net annual volume for growth, mortality, and removals. This code is not used when querying data for change estimates. Note: New and replacement plots use only codes 1 and 2. All trees in tree_data5.csv have status code of 1 (live tree). 0 = no status, 1 = live tree, 2 = dead tree, 3 = removed - cut and removed by direct human activity related to harvesting , silviculture or land clearing. This tree is assumed to be utilized.	
9. spcd - FIA species numerical codes


********
Contact Information
PI: Jason McLachlan
Data Manager: Jody Peters
Contact for data:
University of Notre Dame
100 Galvin
Notre Dame IN, 46556
Emails: peters.63@nd.edu; jmclachl@nd.edu 
Phone numbers: 574-631-2175 (Jody); 574-631-1850 (Jason)

********
Date of data collection 
The forest survey conducted their surveys for these plots from 2009-2014.

******
Information about geographic location of data collection
This is the density data for FIA plots that fall within the Yellow River watershed.
Bounding box: 
NW corner - 41° 10’17.36” N, 85° 51’1.10” W
SE corner - 41° 30’47.42” N, 86° 50’34.60” W

******
Date that the file was created
* April 10, 2017 


********
Keywords used to describe the data topic
Indiana, USDA Forest Service Forest Inventory Analysis, FIA, dbh, composition


*********
Methodological information
Sean Dubois in Jack Williams lab from the University of Wisconsin, Madison compiled the FIA data from the FIA DataMart.
See the PalEON FIA GitHub repository for more details: https://github.com/PalEON-Project/PalEON-FIA/
Jody Peters used the species_plot_parameters.csv created by Sean to find the plots with actual lat/long coordinates that are in the Yellow River watershed.  That lat/long information is not available in this document or to people not on the PalEON-FIA MOU.  
Jody then selected only the plots in the Yellow River watershed from the full_fia_long_v0.1.csv also created by Sean Dubois.
Jody then converted the spcd to FIA species and PalEON Level 3 taxa. Because the plt_cn values are long and unwieldy, she added an ID column with unique identifying numbers corresponding to the plt_cn values. Finally she labeled which side of the study area (Oak dominated = "Oak" which is on the west side of the study area or Other hardwood dominated = "Other" which is on the east side of the study area) in the Buffer column. 

This file was used by Caitlin Broderick to calculate the FIA dbh and composition on the Oak dominated western side of the Yellow River study vs that of the Other tree dominated eastern side of the study area.  

********
Sharing/Access information
We are using the CC-BY 4.0 (Creative Commons Attribution 4.0 International) License for data products that get archived on NIS or elsewhere. Details of this license can be found here:
https://creativecommons.org/licenses/by/4.0/legalcode


