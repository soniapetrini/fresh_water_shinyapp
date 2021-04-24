# Endangered Species Shiny
## *Fresh Water Systems' Endangered Species Observatory*

Welcome! This ShinyApp was built to allow the visualization of spatial data provided by IUCN Red List (https://www.iucnredlist.org/), concerning the endangerment of animals and plants belonging to the fresh waters systems.
All the included species are labelled with the threat category they belong to, according to the IUCN classification of risk:

![Screenshot 2021-04-15 at 21 59 32](https://user-images.githubusercontent.com/78147483/114930987-ea856c80-9e35-11eb-87f1-7e32456d3cf5.png)

- *Critically Endangered, Endangered and Vulnerable*: species threatened with global extinction.
- *Near Threatened*: species close to the threatened thresholds or that would be threatened without ongoing conservation measures.
- *Least Concern*: species evaluated with a lower risk of extinction.
- *Data Deficient*: no assessment because of insufficient data.

To get an understanding of the situation in the recent decades, I have subsetted the data to only include the records from 2000 to the current day.
Moreover, I have turned panel data into a cross-section in order to focus on the spatial dimension, by only considering the date of the last observation for each species in each area.



## Structure

- ### *Main*
Get an overview of the data through an interactive worldwide map: select which kingdoms, areas, categories, and classes you want to visualize and see where the corresponding species are located on the globe. You can also see the number of threatened and near threatened species found in each area.
  
- ### *Species*
Search a specific species in the dataset or read about a random one. 
You will get information regarding the risk of the species' class and phylum, both in absolute terms and with respect to the different areas in which the class/phylum is found.

- ### *Area*
Focus on a specific continent. You can see the number of threatened species by class or phylum in the two different kingdoms, and the partition of the total observed species according to the IUCN categories.



## Data Source

The data was provided by IUCN Red List, under approval:
https://www.iucnredlist.org/resources/spatial-data-download
Spatial Data Download
Freshwater Groups, point



## Support IUCN

IUCN is the world’s most comprehensive information source on the global extinction risk status of animal, fungus and plant species.
Its goal is to reach the assessment of 160.000 species; 134,400 have been evaluated so far.
If you want to support the cause and give your contribution, you can visit https://www.iucnredlist.org/support/donate.
