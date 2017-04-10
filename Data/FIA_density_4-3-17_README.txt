FILENAME: 
FIA_density_4-3-17.csv

This file has the FIA density data per species per FIA plot for plots in the Yellow River watershed.

********
HEADER:

If you have any questions about the data in general or an entry specifically, email peters.63@nd.edu.
1. spcd - FIA species codes
2. FIAspecies - scientific species names that correspond to the FIA spcd
3. PalEON_L3 - PalEON level 3 taxa that lumps the species to a standardized level that is used across the PalEON project (www.paleonproject.org)
4. plt_cn - FIA unique plot identifier
5. ID - unique identifier that corresponds to the plt_cn but is not as long or unweildy
6. Buffer - what side of the midline each plot lands. Oak = west of the line, Other = east of the midline
7. dbh - average diameter at breast height (cm) for each species within each plot
8. basal_area - sum of the basal area (meter squared per hectare) for all trees of each spcd
9. n- number of trees of each species in each plot
10. density - stems per hectare of each tree spcd in the plot



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
* April 3, 2017 


********
Keywords used to describe the data topic
Indiana, USDA Forest Service Forest Inventory Analysis, FIA, density, stems per hectare


*********
Methodological information
Sean Dubois in Jack Williams lab from the University of Wisconsin, Madison compiled the FIA data from the FIA DataMart.
See the PalEON FIA GitHub repository for more details: https://github.com/PalEON-Project/PalEON-FIA/
Jody Peters used the species_plot_parameters.csv created by Sean to find the plots with actual lat/long coordinates that are in the Yellow River watershed.  That lat/long information is not available in this document or to people not on the PalEON-FIA MOU.  
Jody then converted the spcd to FIA species and PalEON Level 3 taxa. Because the plt_cn values are long and unwieldy, she added an ID column with unique identifying numbers corresponding to the plt_cn values. Finally she labeled which side of the study area (Oak dominated = "Oak" or Other hardwood dominated = "Other") in the Buffer column. 

This file was used by Caitlin Broderick to calculate the FIA density on either side of the midline.  To do this Caitlin summed the densities of each species within each plot.  Then averaged the plot densities in the Oak and Other portions of the study area.


********
Sharing/Access information
We are using the CC-BY 4.0 (Creative Commons Attribution 4.0 International) License for data products that get archived on NIS or elsewhere. Details of this license can be found here:
https://creativecommons.org/licenses/by/4.0/legalcode


