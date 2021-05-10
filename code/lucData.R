### Working with the FAO LUC data from the LEAP model to derive global LUC values for different crops

library(tidyverse)

### To dos:
# Calculate average LUC from the data
# Data are in per dry matter so need to convert to total plant
# Fill in additional data for missing crops

faoLuc<-read_csv("faoDat/faoLUCdata_convtnlTillage.csv") ##Emissions factors for land use change(tons CO2eq/kg DM *year), 2010 from "Global database of GHG emissions related to feed crops" 2017


# FAO provide data for: Barley, Maize, soybeans, wheat and cassava

# Crops needed: Barley, fresh grass, grains, hay, legumes & silage, maize, oilseed meals, rapeseed meal, soybean meal, wheat, wheat bran

# proxies: 
#   - barley: Barley
#   - Maize: maize
#   - soybeans: soybean meal, oilseed meals, rapeseed meal(?), legumes
#   - wheat: wheat, wheat bran, fresh grass(?), hay
#   - Barley & wheat: grains

### Moisture content of crops

#   - barley: 12% https://www.agric.wa.gov.au/barley/barley-production-harvest-and-grain-quality
#   - Maize: 20% http://www.fao.org/3/T0395E/T0395E04.htm#:~:text=In%20both%20situations%2C%20maize%20is,content%2C%20the%20less%20the%20damage.
#   - soybeans: 14% at harvest
#   - wheat: 15% at harvest http://www2.ca.uky.edu/agcomm/pubs/id/id125/10.pdf


### Data averages

### Conventiona tillage

lucAg<-faoLuc%>%
  filter(!Crop=="Cassava")%>%
  group_by(Crop)%>%
  summarise(lucEm=mean(WeightedAve)) %>%## tons of co2/kg dry matter
  mutate(moistureCont=c(0.12,0.20,0.14,0.15),
         lucPlant=lucEm*(1-moistureCont), ## tons of co2/kg plant
         crop=tolower(Crop),
         luc=lucPlant*1000)%>%  ## kg co2/kg plant
  select(crop,luc)

lucAg[nrow(lucAg)+1,]<-c("oilseedmeals",lucAg[lucAg$crop=="soybeans","luc"])
lucAg[nrow(lucAg)+1,]<-c("oilseedmeal",lucAg[lucAg$crop=="soybeans","luc"])
lucAg[nrow(lucAg)+1,]<-c("legumesandsilage",lucAg[lucAg$crop=="soybeans","luc"])
lucAg[nrow(lucAg)+1,]<-c("rapeseedmeal",lucAg[lucAg$crop=="soybeans","luc"])
lucAg[nrow(lucAg)+1,]<-c("wheatbran",lucAg[lucAg$crop=="wheat","luc"])
lucAg[nrow(lucAg)+1,]<-c("freshgrass",lucAg[lucAg$crop=="wheat","luc"])
lucAg[nrow(lucAg)+1,]<-c("hay",lucAg[lucAg$crop=="wheat","luc"])
lucAg[nrow(lucAg)+1,]<-c("grains",(lucAg[lucAg$crop=="wheat","luc"]+lucAg[lucAg$crop=="barley","luc"])/2)


### No till practices
### J. Six et al. 2004 for more on the GWP of no till farming


faoLucNT<-read_csv("faoDat/faoLUCdata_noTill.csv")

lucAgNT<-faoLucNT%>%
  filter(!Crop=="Cassava")%>%
  group_by(Crop)%>%
  summarise(lucEm=mean(WeightedAve)) %>%## tons of co2/kg dry matter
  mutate(moistureCont=c(0.12,0.20,0.14,0.15),
         lucPlant=lucEm*(1-moistureCont), ## tons of co2/kg plant
         crop=tolower(Crop),
         luc=lucPlant*1000)%>%  ## kg co2/kg plant
  select(crop,luc)

lucAgNT[nrow(lucAgNT)+1,]<-c("oilseedmeals",lucAgNT[lucAgNT$crop=="soybeans","luc"])
lucAgNT[nrow(lucAgNT)+1,]<-c("oilseedmeal",lucAgNT[lucAgNT$crop=="soybeans","luc"])
lucAgNT[nrow(lucAgNT)+1,]<-c("legumesandsilage",lucAgNT[lucAgNT$crop=="soybeans","luc"])
lucAgNT[nrow(lucAgNT)+1,]<-c("rapeseedmeal",lucAgNT[lucAgNT$crop=="soybeans","luc"])
lucAgNT[nrow(lucAgNT)+1,]<-c("wheatbran",lucAgNT[lucAgNT$crop=="wheat","luc"])
lucAgNT[nrow(lucAgNT)+1,]<-c("freshgrass",lucAgNT[lucAgNT$crop=="wheat","luc"])
lucAgNT[nrow(lucAgNT)+1,]<-c("hay",lucAgNT[lucAgNT$crop=="wheat","luc"])
lucAgNT[nrow(lucAgNT)+1,]<-c("grains",(lucAgNT[lucAgNT$crop=="wheat","luc"]+lucAgNT[lucAgNT$crop=="barley","luc"])/2)

lucAg2<-lucAgNT%>%
  mutate(farming="noTill")%>%
  bind_rows(.,lucAg%>%mutate(farming="conventionalTill"))

# write_csv(lucAg2,"data/derived/lucValsKgCo2_kg.csv")

  

