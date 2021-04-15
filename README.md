# fresh_waters_shinyapp
## *Fresh Water Systems' Endangered Species Observatory*

Welcome! This ShinyApp was built to allow the visualization of spatial data provided by IUCN Red List (https://www.iucnredlist.org/, data source: https://www.iucnredlist.org/resources/spatial-data-download, Freshwater Groups), concerning the endangerment of animals and plants belonging to the fresh waters systems.
All the included species are labelled with the threat category they belong to, according to the IUCN classification of risk:

![Screenshot 2021-04-15 at 21 14 04](https://user-images.githubusercontent.com/78147483/114925595-94152f80-9e2f-11eb-9dfb-1918309b2edf.png)

- *Critically Endangered, Endangered and Vulnerable*: species threatened with global extinction.
- *Near Threatened*: species close to the threatened thresholds or that would be threatened without ongoing conservation measures.
- *Least Concern*: species evaluated with a lower risk of extinction.
- *Data Deficient*: no assessment because of insufficient data.

To get an understanding of the situation in the recent decades, I have subsetted the data to only include the records from 2000 to the current day.
Moreover, I have turned panel data into a cross-section in order to focus on the spatial dimension, by only considering the date of the last observation for each species in each area.


## *Structure*
- ### Main
