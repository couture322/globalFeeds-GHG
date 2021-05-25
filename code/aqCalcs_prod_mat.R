### Aquaculture feed compositions calculations
### J. L. Couture



### Global Aquaculture production (FAO) from 2009-2018
# "Environment" col is fresh (1), brackish (2), marine(3)
# "Quantity_Symbol" column rates the value all data look like they are legit
# "Quantities are given in tonnes. The value of aquaculture, converted from local currencies, is reported in 1000 US dollars using appropriate exchange rates." - Aqua_E.html

faoProd<-read_csv("data/faoData/Aquaculture_2020.1.0/TS_FI_AQUACULTURE.csv")%>%
  left_join(.,read_csv("data/faoData/Aquaculture_2020.1.0/CL_FI_SPECIES_GROUPS.csv")%>%select(`3Alpha_Code`,Taxonomic_Code),
            by=c("SPECIES"="3Alpha_Code"))%>%
  left_join(.,read_csv("data/faoData/unqSppFAO_diet.csv"))%>%
  rename(faoGrp=fao2002Grp)


### Create protein content data
fryTacon<-data.frame(faoGrp=unique(faoProd$faoGrp),
                     pcFeed_low=c(30,25,20,25,NA,NA,9,18,30,9,NA,NA,30,35,18,26,30,30,17,40),
                     pcFeed_high=c(45,45,32,45,NA,NA,22,38,45,22,NA,NA,45,45,38,32,45,45,45,47), ## eel data from Tibbets et al. 1999; flatfish (ie: sole) from Bai & Katya 2013 GAA
                     fcr=c(1.6,1.5,1.6,1.8,NA,NA,1.6,1.6,1.6,1.6,NA,NA,1.5,1.3,1.3,1.3,1.6,1.3,1.6,1.3), # Tacon, Hasan & metian 2011 --> 2020 projections, missing values use general other value of 1.6
                     fmIncl=c(10,8,1,8,NA,NA,2,2,8,1,NA,NA,30,12,2,2,2,12,1,12),
                     soyIncl_low=c(8,5,20,15,NA,NA,35,20,20,35,NA,NA,8,10,2,22,20,13,5,3), #catfish data from GAA report (Engel et al.); salmon data adjusted based on Couture et al. 2019
                     soyIncl_high=c(30,40,60,25,NA,NA,40,40,40,40,NA,NA,10,20,2,51,40,15,25,35),# FM and soy inclusion data from Tacon 2015 except where indicated
                     percIndFd=c(90,87,95,60,NA,NA,55,75,60,50,NA,NA,98,100,75,82,60,85,60,100)) # % industrial feed from Tacon & Metian 2015: feed matters (2020 projections)

### apply diets data to production data
aqProd<-faoProd%>%
  left_join(fryTacon)%>%
  filter(fed==1)%>%
  ungroup()%>%
  mutate(prod=QUANTITY*0.86, # Fry 2016 calculates 86% of seafood is human edible
         pcFeed_mid=(pcFeed_high+pcFeed_low)/2,
         protReq=QUANTITY*fcr*(pcFeed_mid/100),
         faoGrp=fct_reorder(faoGrp,QUANTITY))

grpProd<-aqProd%>%
  filter(YEAR==max(YEAR))%>%
  group_by(faoGrp)%>%
  summarise(totProd=sum(QUANTITY),
            totInd=sum(prod))%>%
  ungroup()%>%
  mutate(faoGrp=fct_reorder(faoGrp,totProd))


#############################################################################
### Fishmeal inclusion
# Froehlich et al. estimate limit of 30mt of forage fish for fishmeal --> 5.2:1 conversion = 5.8mt of fishmeal
# ecologically available protein from fishmeal: 5.8 * 0.68 = 3.944 mt protein

fmDem<-aqProd%>%
  filter(YEAR==2018,
         !is.na(fmIncl))%>%
  mutate(fmDemand=QUANTITY*fcr*(fmIncl/100))%>%
  group_by(faoGrp)%>%
  summarise(totFmDem=sum(fmDemand))%>%
  mutate(faoGrp=fct_reorder(faoGrp,totFmDem))%>%
  ungroup()

#######################################
### Soy inlcusion

soyDem<-aqProd%>%
  filter(YEAR==2018,
         !is.na(soyIncl_low))%>%
  mutate(soyDemLo=QUANTITY*fcr*(soyIncl_low/100),
         soyDemHi=QUANTITY*fcr*(soyIncl_high/100))%>%
  group_by(faoGrp)%>%
  summarise(low=sum(soyDemLo),
            high=sum(soyDemHi))%>%
  pivot_longer(cols=c(low,high),names_to = "bound",values_to = "soyIncl")%>%
  mutate(faoGrp=fct_reorder(faoGrp,soyIncl))
# ungroup()

soyMid<-soyDem%>%
  pivot_wider(names_from = "bound",values_from = "soyIncl")%>%
  mutate(mid=(low+high)/2)
totSoy<-sum(soyMid$mid)

troelProps<-data.frame(input=c("soybeanMeal","rapeseedMeal","maize","wheat","wheatBran"),
                       amtMt=c(6.5,5,4,4,3))%>%
  mutate(propInput=amtMt/sum(amtMt),
         relInput=totSoy*(amtMt/6.5),
         protContr=relInput*c(0.46,0.40,0.10,0.12,0.09))

cropProt<-sum(troelProps$protContr)

####################################################
### Other ingredients brought in from Troell et al. 2014

cciDat<-read_csv("data/recipeCCIs.csv")%>%
  filter(!duplicated(ingredient))

aqIn2018<-troelProps%>%
  select(input,relInput)%>%
  mutate(input=str_replace(input,"soybeanMeal","soybean meal"),
         input=tolower(str_replace(input," ","")))%>%
  add_row(input="fishmeal",relInput=sum(fmDem$totFmDem))%>%
  mutate(cciPer=unlist(c(cciDat[cciDat$ingredient=="Soybean meal","CCI_kg_kg"],
                         cciDat[cciDat$ingredient=="Oilseed meals","CCI_kg_kg"],
                         cciDat[cciDat$ingredient=="Maize","CCI_kg_kg"],
                         cciDat[cciDat$ingredient=="Wheat","CCI_kg_kg"],
                         cciDat[cciDat$ingredient=="Wheat","CCI_kg_kg"],
                         cciDat[cciDat$ingredient=="Fishmeal","CCI_kg_kg"])),
         totCCI=relInput*cciPer,
         incl=relInput/sum(relInput))

aqYrs<-aqProd%>%
  group_by(YEAR)%>%
  summarise(totProd=sum(QUANTITY,na.rm = T))%>%
  crossing(.,aqIn2018)%>%
  mutate(inputContr=totProd*1.6*incl, ## production * ave fcr * inclusion rates
         cciContr=cciPer*inputContr)


aqFdUse<-aqYrs%>%
  ggplot(aes(YEAR,inputContr/1000000))+
  geom_col(aes(fill=input))+
  scale_y_continuous(expand=c(0,0))+
  labs(x="Year",y="Feed inputs (mt)")+
  theme_bw()

ccimps<-aqYrs%>%
  ggplot(aes(x=YEAR,y=cciContr/1000000))+
  geom_col(aes(fill=input))+
  scale_y_continuous(expand=c(0,0))+
  labs(x="Year",y="CCI (mt CO2eq)")+
  theme_bw()

