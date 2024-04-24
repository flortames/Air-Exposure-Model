# Estimation of exposure to atmospheric pollutants with dynamic variables.
Model that assesses daily exposure to air pollution, taking into account several dynamic variables. The essence of this model focuses on integrating data related to daily routines, people's mobility and hourly concentrations of air pollutants. 
Users must provide accurate details about their daily activities, including information about their residence, activity sites, activity schedules, and the commuting. A key element is the use of the TomTom API, which determinated urban mobility 
patterns for a more complete view of travel in urban environments. The atmospheric pollutants considered in this model include are the criteria pollutants: carbon monoxide (CO), nitrogen dioxide (NO2), particulate matter (PM), ozone (O3) and 
sulfur dioxide (SO2) due to their potential health impacts. Additionally, a model with fixed variables is included, where it is assumed that a person is present 24 hours at a location (typically at home). The purpose of this function is to compare 
both methodologies and identify more realistic ways to estimate exposure and understand potential health effects.

- The folder 'Model' has the necessary scripts to estimate exposure. All functions must be run at the same time for it to work correctly.
- The folder 'Example script' has example scripts of the most important functions to calculate exposure.
- The folder 'Data' contains example datasets in shapefile format to test the exposure function. These files contain information on hourly concentrations of PM2.5 in the Metropolitan Area of Mendoza, Argentina.
- The folder 'Output' shows examples of outputs from some of the displayed functions.
