<div align="center">
  <img width="300" alt="images" src="https://github.com/user-attachments/assets/5588d555-38ca-44f0-9246-1b40cb71d61f" />
</div>

# Big data analytics for electric vehicles in the smart grid
As plug-in electric vehicle (PEV) adoption accelerates, uncoordinated charging habits during peak hours can cause power losses, overloads, and voltage fluctuations in smart grids. This thesis simulates the success rate (10% - 50%) of a hypothetical campaign encouraging consumers to charge their PEVs during off-peak hours to help balance grid load. Data analytics tools and descriptive statistics are used to manipulate data and visualize results. The findings show that even a slight shift in consumer behavior during peak hours can lead to a more balanced distribution of energy demand.

## ⚙️ System Requirements
* **R** and an **R IDE** (such as **R Studio**) are required. You can download both from the [official RStudio website](https://posit.co/download/rstudio-desktop/).
* The following R packages are required to run the scripts: `ggplot2`, `lubridate`, `dplyr`, and `glue`. You can install them by running the following command in your R console:  
```
install.packages(c("ggplot2", "lubridate", "dplyr", "glue"))
```
* To ensure correct date formatting, your computer's display language must be set to **English (United States)** in the language settings.
> [!NOTE]
> Alternatively, the link in the "about" section opens a google colab notebook (with links to other notebooks) to view the executed code and graphs as well as a brief description.

## 📊 Data Sources
The original CSV files can be found [here](https://data.nrel.gov/submissions/69).  
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
