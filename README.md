<div align="center">
  <img width="300" alt="images" src="https://github.com/user-attachments/assets/5588d555-38ca-44f0-9246-1b40cb71d61f" />
</div>
# Big data analytics for electric vehicles in the smart grid
This repository contains the CSV files and R scripts of the BSc thesis, ready to execute in R environment. To produce the graphs correctly, it is recommended to change the computer's region to the United States of America so R assigns dates in English (not in Greek). Alternatively, the link in the "about" section opens a google colab notebook (with links to other notebooks) to view the executed code and graphs as well as a brief description.

The original CSV files can be found [here](https://data.nrel.gov/submissions/69).

In summary, the BSc thesis investigated the energy demand of electric vehicles charging in the smart grid from simulation data in CSV format. Next, the load shifting strategy migrated the energy demand from peak to off-peak hours with varying success rates (10% - 50%). These rates refer to a theoretical campaign which would encourage consumers to charge their vehicles during off-peak hours. To manipulate data and visualize the results, data analytics tools and descriptive statistics were used.

## The data
* ```House.csv```: the energy demand of the houses (in Watt).
* ```PEV_L1.csv```: the energy demand of vehicles charging with the L1 charging type (in Watt).
* ```PEV_L2.csv```: the energy demand of vehicles charging with the L2 charging type (in Watt).
* ```TimeZones_old.csv```: new structure that categorizes the energy demand by time zone.
* ```TimeZones.csv```: includes only the charges per time zone for a working week of January. 

## The scripts
* ```Chapter 2 Plots.R```: a first look at the dataset.
* ```Chapter 4 Plots.R```: thorough investigation of the dataset.
* ```Chapter 5 Plots.R```: TimeZones structure exploraton.
* ```Chapter 6 Plots.R```: load shifting results study.
* ```TimeZones.R```: creates the new TimeZones structure.
* ```LoadShifting.R```: applies the load shifting strategy to the TimeZones structure.
* ```L1-L2.R```: returns a LoadShifting or TimeZones structure back to the PEV_L1 and PEV_L2 structures.

<!-- ![Placeholder](https://via.placeholder.com/150) -->
