### Calculating feed compositions for livestock weighted by production and region

# Based on most 2018 feed formulations (gleam) weighted by 2018 production data (FAOSTAT)

livProd<-read_csv("faoDat/livestockFeeds/FAOSTAT_data_4-13-2021.csv") ## pulled data for the last 3 years: 2017,2018,2019 in case want to check out differences, start with 2019 tho --> replaced with all years of data in Aprilt

glmFull<-read_csv("faoDat/livestockFeeds/GLEAM_Data_public_release_tab1.csv",skip = 1, locale = locale(encoding = "latin1"))
protInMeat<-data.frame(product=c("cattle","chicken","pig","goat","sheep"),
                       meatProt=c(0.175,0.186,0.166,0.175,0.175)) 

livP<-livProd%>%
  #filter(Year==2019)%>%
  mutate(area=sapply(strsplit(Area, ","), function(x) x[1]))%>%
  mutate(country=sapply(strsplit(area, " \\("), function(x) x[1]))%>%
  select(country,itmCode=`Item Code`,Item,Year,Value,Flag,`Flag Description`)

livP%>%
  group_by(Year)%>%
  summarise(gloProd_mt=sum(Value,na.rm = T)/1000000,.groups="drop")%>%
  ggplot()+
  geom_col(aes(x=Year,y=gloProd_mt))+
  labs(y="Global meat production (mt)",title="Land-based livestock")+
  scale_y_continuous(expand = c(0,0))+
  theme_bw()


### GLEAM feed rations data
## Pulled data for beef cattle, pork and poultry, but also have for: buffalo and small ruminants (sheep/goats)

gleam<-read_csv("faoDat/livestockFeeds/feedCompsGLEAM2combo.csv", na=c("","-"))%>%
  rename(material=`Feed material`)

gleam2S<-gleam%>%
  gather(key="regionCode",value="propFeed",`NA`:SSA)%>%
  filter(!is.na(propFeed),
         product %in% c("industrialPork","industrialBoilerPoultry","feedlotBeef","smallRuminants"))%>%
  group_by(product,regionCode)%>% ### some feeds didn't total to 100 so propfeed isn't a percent so am calculating this manually
  mutate(totFeed=sum(propFeed))%>%
  ungroup()%>%
  mutate(percFeed=propFeed/totFeed)%>%
  rename(product2=product)
  

globLivProd<-livP%>%
  filter(!is.na(Value))%>%
  group_by(Item)%>%
  summarise(itmProd=sum(Value))%>%
  ungroup()%>%
  mutate(prodProp=(itmProd/sum(itmProd))*100,
         product=sapply(strsplit(Item,split=" "), function(x) x[2]))%>%
  full_join(gfGlo,.)%>%
  mutate(propProd=case_when(is.na(propProd) ~ 1,
                            TRUE ~ propProd),
         scale=case_when(is.na(scale) ~ "local",
                         TRUE ~ scale),
         scaleProd=itmProd*propProd,
         product=fct_reorder(product,prodProp),
         scale=fct_relevel(scale,level=c("local","industrial")))


antjn<-livP%>%
  anti_join(.,regDat1,by="country") %>%
  filter(!duplicated(country))%>%
  select(country)%>%
  mutate(regionCode = "fill")

antjn[antjn$country=="Cabo Verde","regionCode"] <- "SSA"
antjn[antjn$country=="Cook Islands","regionCode"] <- "OCE"
antjn[antjn$country=="Eswatini","regionCode"] <- "SSA"
antjn[antjn$country=="Faroe Islands","regionCode"] <- "WE"
antjn[antjn$country=="French Polynesia","regionCode"] <- "OCE"
antjn[antjn$country=="Iran","regionCode"] <- "SA"
antjn[antjn$country=="Luxembourg","regionCode"] <- "WE"
antjn[antjn$country=="Micronesia","regionCode"] <- "OCE"
antjn[antjn$country=="New Caledonia","regionCode"] <- "OCE"
antjn[antjn$country=="Niue","regionCode"] <- "OCE"
antjn[antjn$country=="North Macedonia","regionCode"] <- "WE"
antjn[antjn$country=="Palestine","regionCode"] <- "NENA"
antjn[antjn$country=="Republic of Moldova","regionCode"] <- "EE"
antjn[antjn$country=="Sao Tome and Principe","regionCode"] <- "SSA"
antjn[antjn$country=="Serbia","regionCode"] <- "EE"
antjn[antjn$country=="Tokelau","regionCode"] <- "OCE"
antjn[antjn$country=="Czechia","regionCode"] <- "WE"

regDat2<-bind_rows(regDat1,antjn)

### apply region codes to countries using above fixes
livRegS<-livP%>%
  filter(Year==max(Year))%>%
  left_join(.,regDat2,by='country')%>%
  filter(!is.na(Value))%>% # should remove everything that is missing, checked the other data qualifiers and there is nothing alarming, some calculations/estimates, but that's better than excluding at this point
  mutate(product=str_replace(Item,"Meat, ",""))%>%
  filter(product %in% c("cattle","pig","chicken","sheep","goat"))%>%
  group_by(regionCode,product)%>%
  summarise(totProd=sum(Value),.groups="drop")%>%
  group_by(product)%>%
  mutate(prodProp=totProd/sum(totProd),
         product2=case_when(product=="cattle" ~ "feedlotBeef",
                            product=="pig" ~ "industrialPork",
                            product=="chicken" ~ "industrialBoilerPoultry",
                            TRUE ~ "smallRuminants"))%>%
  left_join(.,gleam2S, by=c("product2","regionCode"))%>%
  filter(!is.na(material))%>%
  mutate(globMatIn=prodProp*percFeed)%>%
  group_by(product,material)%>%
  summarise(globProp=sum(globMatIn))

### manually scale cattle feed since don't add to 1 (bc some countries were removed in !is.na() call)
livRegS[livRegS$product=="cattle","globProp"]<-as.vector(livRegS[livRegS$product=="cattle","globProp"]/sum(as.vector(livRegS[livRegS$product=="cattle","globProp"])))

livRegS<-livRegS%>%
  mutate(material=sapply(strsplit(material,split=" \\("),function(x) x[1]))%>%
  filter(globProp>=0.05)

livRegS[livRegS$material=="Wet distilleries grain","material"]<-"Grains"
livRegS[livRegS$material=="Dry by-product grain industries","material"]<-"Grains"
livRegS[livRegS$material=="Leaves","material"]<-"Crop residues"

# write_csv(livRegS,"data/derived/wtdFeedProps_livestock.csv")
