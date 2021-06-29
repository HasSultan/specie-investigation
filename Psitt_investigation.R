##### - Introduction ----
#Psittaciformes, more commonly known as parrots, comprise of over 350 species categorized in 84 genera. They are most commonly found in tropical and subtropical regions with the most species richness in Australia, South America and Central America (Heatley & Cornejo, 2015). 

#Their bright colours and intelligence have captivated humans for centuries, especially in the Unites States. This has led to large international parrot trade, both legally and illegally. A study conducted at Cornell University explored establishments of parrot species in the USA. Through historical records, as well as data from citizens, it was revealed that 56 different species of parrots were sighted with 25 confirmed breeders (Uehling, Tallant & Pruett-Jones, 2019). 

#In this project, the species richness, categorized by unique BIN ID, by country is explored. Despite parrots' dominant prevalence in Australia, South America and Central America, their ability to establish breeding populations in the USA is fascinating. Furthermore, exploring species richness of parrots in countries they are not native to can be important in monitoring how species diversity changes over time as a result of human interference.

##### - Load relevant packages----
library(tidyverse)
library(vegan)
library(rworldmap)

##### - Obtain Psittaciformes data from BOLD----
dfpsitt <- read_tsv(file = "http://www.boldsystems.org/index.php/API_Public/combined?taxon=Psittaciformes&format=tsv")

#Write data to disk
write_tsv(dfpsitt, "Psittaciformes_BOLD_data.tsv")

#read file
dfpsitt <- read.delim(file='Psittaciformes_BOLD_data.tsv')

##### - Check basic attributes of dataframe----
#Checking basic attributes of data
names(dfpsitt)
summary(dfpsitt)

##### - Distribution by Country----
#Count of barcode data by country with NAs for country removed
country_dis <- dfpsitt %>%
  filter(!is.na(country)) %>%
  count(country, sort = TRUE) 

# creating map plot using rworldmap package
joinData <- joinCountryData2Map(country_dis,
                                joinCode = "NAME",
                                nameJoinColumn = "country")
colourPalette <- RColorBrewer::brewer.pal(5,'RdPu')

theMap <- mapCountryData( joinData, nameColumnToPlot="n", addLegend=TRUE, mapTitle = "Psittaciformes Data Collection by Country", oceanCol = 'lightblue', missingCountryCol = "darkgrey", borderCol = "black", colourPalette = colourPalette)

#Here is a visualization of count of barcode data by country
ggplot(data = country_dis, aes(reorder(x = country, n), y = n, fill = country)) +
  geom_bar(stat = "identity") +
  theme(legend.position = "none") +
  coord_flip() +
  ggtitle("Data Distribution by Country") +
  xlab("Country") +
  ylab("Count of Barcode Data") 

#As expected, Australia and many of the South American Countries have the most barcode data

#Here we are creating a dataframe of the count of unique BIN ID for each country. To do this, we are removing all NAs for country and bin_uri. We assume that a unique BIN ID is representative of a specie
unique.bins.by.country <- dfpsitt %>%
  filter(!is.na(country)) %>%
  filter(!is.na(bin_uri)) %>%
  group_by(country) %>%
  summarize(count.unique.bins = length(unique(bin_uri))) %>%
  arrange(desc(count.unique.bins)) %>%
  print() 

#Check class of dataframe 
class(unique.bins.by.country)

#Now we can plot a graph of the species richness by country
ggplot(data = unique.bins.by.country, aes(reorder(x = country, count.unique.bins), y = count.unique.bins, fill = country)) +
  geom_bar(stat = "identity") +
  theme(legend.position = "none") +
  coord_flip() +
  ggtitle("Species Richness by Country") +
  xlab("Country") +
  ylab("Species Richness")

#As expected, Australia and many of the South American countries also have the highest species richness

#Here a heatmap is generated to better visualize the species diversity and the count of the species by country
dfpsitt %>%
  filter(!is.na(country)) %>%
  filter(!is.na(bin_uri)) %>%
  count(country, bin_uri) %>%
  ggplot(mapping = aes(x = country, y = bin_uri)) +
  geom_tile(mapping = aes(fill =n)) +
  coord_flip() +
  ggtitle("Species Diversity and Count of Species by Country") +
  xlab("Country") +
  ylab("Species") +
  labs(fill='Count of Species') +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank())

#As was seen by the previous graphs, Australia does indeed have a high amount of species diversity as well as count of each species (indicated by the colour). Many of the species seen in Australia do not have a record in the USA. There is a lot of diversity seen in countries like Guyana, Australia, Argentinia, and Brazil. The dark blue colour also indicates a high count of each species recorded. 

##### - Species Richness and Rarefraction Curve---- 
#First the data is grouped by BIN and then the number of records in each BIN is counted
psitt.count.by.bin <- dfpsitt %>%
  group_by(bin_uri) %>%
  count(bin_uri)

#Preview of data
psitt.count.by.bin

#Check class
class(psitt.count.by.bin)

#The data is reshaped so that the column is the BIN and the values are counts of individuals per BIN
psitt.spread <- spread(data = psitt.count.by.bin, key  = bin_uri, value = n)

#A rarefraction curve is built to assess species richness from sampling
psitt.rare.curve <- rarecurve(psitt.spread, main = "Rarefraction Curve for Psittaciformes Data", xlab = "Individuals Barcoded", ylab = "Species Richness")

#As more individuals are barcoded, the rate of species richness is decreasing indicated by the slope. This is because as you sample more individuals, there are less new species remaining to be discovered. The slope of the curve can be measured at different individuals barcoded to measure this.

rareslope(psitt.spread, 250)
#slope = 0.159

rareslope(psitt.spread, 500)
#slope = 0.086

rareslope(psitt.spread, 10000)
#slope = 0

#As we increase the number of individuals barcoded, the BIN richness decreases because there is only a finite number of species. This also highlights the importance of taking into account sampling size when comparing species richness among countries. Countries with less sampling will in turn also have less species richness.

##### - Species Distribution and Accumulation Curve----

#Produce count of BINs, by country, with NA for country and bin_uri removed
bin_count <- dfpsitt%>%
  group_by(country, bin_uri)%>%
  filter(!is.na(country))%>%
  filter(!is.na(bin_uri)) %>%
  count(bin_uri)

#Spread data so that BINS are columns, and values are counts of BINs by country
bin.spread.by.country <- spread(data =bin_count, key = bin_uri, value = n)

#Check class
class(bin.spread.by.country)

#Replace NA values with 0
bin.spread.by.country[is.na(bin.spread.by.country)] <- 0

#Check names 
names(bin.spread.by.country)

#The rownames are set as country rather than as a data column
bin.spread.by.country <- bin.spread.by.country %>%
  remove_rownames %>%
  column_to_rownames(var = "country")

#Check names again
names(bin.spread.by.country)

#Create accumulation curve
AccumCurve <- specaccum(bin.spread.by.country)

#Plot accumulation curve
plot(AccumCurve, main = "Accumulation Curve for Psittaciformes Data", xlab = "Countries Sampled", ylab = "Species Richness")

#This plot describes the relationship between countries sampled and species richness. As more countries are sampled, species are discovered at a much higher rate. As more countries and areas are sampled, more species diversity will also be revealed.

##### - Results and Discussion ----

#In this exploratory analysis, species richness by country was investigated. As expected, Australia and many South American countries had the highest species richness as well as the most sampling of barcode data. The United States had a small amount of species richness and sampling compared to other countries. Surprisingly, Canada had a higher species richness than the United States, and instead, similar to that of India’s. These unexpected results can likely be tied back to the variation in recorded barcode data across different countries. In addition, the large number of missing data for country and BIN ID could also be a contributing factor. Furthermore, species richness and the number of individuals barcoded is correlated. As explored through the rarefaction curve, as more individuals are sampled, more species diversity is also revealed. Similarly, the accumulation curve clarifies that as more locations and countries are sampled, species richness increases more steeply. 