# Endangered Species Shiny App
--------------------
## *Fresh Water Systems' Endangered Species Observatory*

Welcome! 
You can find the ShinyApp at https://soniapetrini.shinyapps.io/fw_shiny/ .
(Please note that the Application's layout was designed for desktops, but a new improved version will be available soon.)

The aim of this tool is to allow the visualization of spatial data provided by IUCN Red List (https://www.iucnredlist.org/), concerning the endangerment of animals and plants belonging to the fresh waters systems. 
All the included species are labelled with the threat category they belong to, according to the IUCN classification of risk:

![Screenshot 2021-04-25 at 00 39 44](https://user-images.githubusercontent.com/78147483/115974705-c7a63700-a55e-11eb-84fd-15afe932774a.png)

  - *Critically Endangered, Endangered and Vulnerable*: species threatened with global extinction.
  - *Near Threatened*: species close to the threatened thresholds or that would be threatened without ongoing conservation measures.
  - *Least Concern*: species evaluated with a lower risk of extinction.
  - *Data Deficient*: no assessment because of insufficient data.

By focusing on a species or on a geographic area, it is possible to plot information about it's rate of endangerment.
To get an understanding of the situation in the recent decades, I have subsetted the data to only include the records from 2000 to the current day.
Moreover, I have turned panel data into a cross-section in order to focus on the spatial dimension, by only considering the date of the last observation for each species in each area. 


--------------------
## ◊ Structure

  - ### *Main*
Get an overview of the data through an interactive worldwide map: select which kingdoms, areas, categories, and classes you want to visualize and see where the corresponding species are located on the globe. You can also see the number of threatened and near threatened species found in each area.
  
  - ### *Species*
Search a specific species in the dataset or read about a random one. 
You will get information regarding the risk of the species' class and phylum, both in absolute terms and with respect to the different areas in which the class/phylum is found.

  - ### *Area*
Focus on a specific continent. You can see the number of threatened species by class or phylum in the two different kingdoms, and the partition of the total observed species according to the IUCN categories.


--------------------
## ◊ Data Source

The data was provided by IUCN Red List, under approval:
https://www.iucnredlist.org/resources/spatial-data-download
Spatial Data Download
Freshwater Groups, point


--------------------
## ◊ Support IUCN

IUCN is the world’s most comprehensive information source on the global extinction risk status of animal, fungus and plant species.
Its goal is to reach the assessment of 160.000 species; 134,400 have been evaluated so far.
If you want to support the cause and give your contribution, you can visit https://www.iucnredlist.org/support/donate.
