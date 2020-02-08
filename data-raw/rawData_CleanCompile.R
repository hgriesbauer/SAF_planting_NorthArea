# Script to compile planted Bl and Ba in north regions

library(tidyverse)


##############################
# load original dataset
dat<-readr::read_csv("data-raw/Bl_Ba_Planting_4Regions_February2020.csv",guess_max = 10000)

# Save raw dat file
save(dat,file="data-raw/datRaw.RData")

##############################
# Determine if there are any rows that are duplicated
# Remove any duplicated rows
# Nothing duplicated so that's good
identical(nrow(dat),nrow(dplyr::distinct(dat)))

#################################
# Select and rename columns of choice for easier analysis
blDatRaw<-  
  dat %>% 
  dplyr::select(ACTIVITY_TREATMENT_UNIT_ID,
                OPENING_ID,
                AREA=ACTUAL_TREATMENT_AREA, # note: area is total unit area, not prorated for species comp
                SPECIES=SILV_TREE_SPECIES_CODE,
                NUMBER_PLANTED,
                SEEDLOT_NUMBER,
                PLANTING_DATE=ATU_COMPLETION_DATE, # I'm confident this is the planting date, but could do a bit more digging to double check
                REGION=REGION_CODE,
                DISTRICT=DISTRICT_CODE,
                BGC_ZONE=contains("BGC_ZONE_CODE"),
                BGC_SUBZONE=contains("BGC_SUBZONE_CODE")
                ) %>% 
  mutate(YEAR=lubridate::year(PLANTING_DATE)) %>%  # extract year
  mutate(BGC_SUBZONE=tolower(BGC_SUBZONE)) %>% # ensure that all subzones are lower case
  mutate(BGC=paste(BGC_ZONE,BGC_SUBZONE,sep="")) # create BGC unit column

############################################
# Remove records with species == "Bb
blDatRaw<-
  blDatRaw %>% 
  filter(SPECIES%in%c("BA","BL"))

############################################
### Fix missing BGC values
# Download geometry for these openings and get BEC data for them in a separate file
blDatRaw[blDatRaw$BGC=="NANA",] %>% 
  group_by(SPECIES) %>% 
  summarise(Num.Openings=n(),
            Num.Trees=sum(NUMBER_PLANTED))

# Can see that there are 81 openings with missing BGC data, affects
# close to 600,000 trees
OpeningID<-
  blDatRaw %>% 
  filter(BGC=="NANA")%>% # filter for missing BGC values
  distinct(OPENING_ID) %>% # remove entries with same Opening ID
  pull(OPENING_ID)

# Now query BC data to extract spatial information
# This is the planting spatial file
# download BEC v11 to use in the spatial join
bgc<-bcmaps::bec() 

x<-
  bcdata::bcdc_query_geodata("53a17fec-e9ad-4ac0-95e6-f5106a97e677") %>% # download opening spatial data
  filter(OPENING_ID%in%OpeningID) %>% # filter for our openings of interest
  collect() %>% # collect from BCdata
  sf::st_join(bgc[,c("ZONE","SUBZONE")]) %>% # join with BEC data
  mutate(BGC=paste(ZONE,SUBZONE,sep="")) %>% # create new BGC column
  dplyr::select(OPENING_ID,BGC) %>%  # select for ease of use
  sf::st_drop_geometry() %>% 
  distinct(OPENING_ID,.keep_all = TRUE) # some openings have more than one BGC, select the first one

# now replace BGC in original dataset
blDatRaw<-
  blDatRaw %>% 
  left_join(x,by=c("OPENING_ID")) %>% 
  mutate(BGC=BGC.x) 

  # Replace
  blDatRaw$BGC[blDatRaw$BGC=="NANA"]=blDatRaw$BGC.y[blDatRaw$BGC=="NANA"]
  
  blDatRaw<-
    blDatRaw %>% 
    dplyr::select(-BGC.x,-BGC.y) # remove these columns


# Now there is only one opening with missing BGC data
# It was planted with Amabalis fir
blDatRaw[blDatRaw$BGC=="NANA",]

#############################
# Fix old BGC codes 
bgcLookup<- readr::read_csv("data-raw/oldBGC_Lookup.csv")

# how many have old codes?
length(blDatRaw$BGC[which(blDatRaw$BGC%in%bgcLookup$Old==TRUE)])
bgcInd<-which(blDatRaw$BGC%in%bgcLookup$Old==TRUE)

  # Change old codes  
  X<-
    blDatRaw %>% 
    left_join(bgcLookup,by=c("BGC"="Old"))

    X$BGC[bgcInd]=X$New[bgcInd]

    # Double check to make sure it worked (yes)
    which(X$BGC%in%bgcLookup$Old==TRUE)  
    
    # Some final formatting
    X<-  
      X %>% 
      dplyr::select(-BGC_ZONE,-BGC_SUBZONE) %>% 
      mutate(Zone=stringr::str_sub(BGC,end=-3)) %>% 
      mutate(Subzone=stringr::str_sub(BGC,start=-2)) %>% 
      dplyr::select(-New)
    
    # Deal with one subzone entered incorrectly as ws1
    X$Subzone[which(X$Zone=="CWHw")]="ws"
    X$Zone[which(X$Zone=="CWHw")]="CWH"
    
    
    
#####################
# Rename final varuiable
    blDat<-X
      
#####################
# save file for further screening and cleaning
save(blDat,file="data-raw/blData.RData")





