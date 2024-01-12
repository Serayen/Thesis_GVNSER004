#Serayen Govender

library(tidyverse)
setwd("C:/Users/seray/Research Work/Data")
#data <- read.csv("Demography_Final_May2019.csv")
#data <- read.csv("20201027_demography-survey-rawdata.csv")
data <- read.csv("Demography_Final_April_2020.csv") 
head(data)
view(data)
data <- as_tibble(data)

#######################################
#Corrections in dataset
#Use only before saving 
#Species2 sprout/seed correction
data <- data %>% filter(SppName == "Elytropappus.rhinocerotis") %>% mutate(Group = "Woody.resprouts", Sprout.Seed = "Sprout")
#Species2 sprout/seed correction
data <- data %>% filter(SppName == "Agathosma.capensis") %>% mutate(Group = "Woody.resprouts", Sprout.Seed = "Sprout")
data <- data %>% filter(SppName == "Clutia.rubricaulis?") %>% mutate(Group = "Woody.resprouts", Sprout.Seed = "Sprout")
filter(SppName == "Clutia.rubricaulis?") %>%filter(Sprout.Seed == "Sprout")
#################################
# Exploratory Analysis
#Count Unique Elements of Factors
count(data, F=="F")
data %>% group_by(Site) %>% summarize(count = n()) %>% arrange(desc(count))
data %>% group_by(PlotTag) %>% summarize(count = n()) %>% arrange(desc(count))
data %>% group_by(Grid) %>% summarize(count = n()) # %>% arrange(desc(count))
data %>% group_by(FuncGroup) %>% summarize(count = n()) %>% arrange(desc(count))
data %>% group_by(Group) %>% summarize(count = n()) %>% arrange(desc(count))
data %>% group_by(SppName) %>% summarize(count = n()) %>% arrange(desc(count))
data %>% group_by(WorkingName) %>% summarize(count = n()) %>% arrange(desc(count))
data %>% group_by(Survey) %>% summarize(count = n()) %>% arrange(desc(count))
data %>% group_by(Sprout.Seed) %>% summarize(count = n()) %>% arrange(desc(count))

#Grouping data by Functional Groups
#Gram Species
data_gram <- data %>% filter(FuncGroup == "Graminoid")
data_gram %>% group_by(FuncGroup) %>% group_by(SppName) %>% summarize(count = n()) %>% arrange(desc(count))
data_gram %>% group_by(FuncGroup) %>% group_by(Group) %>% summarize(count = n()) %>% arrange(desc(count))


#Geophyte Species
data_geo <- data %>% filter(FuncGroup == "Geophyte.bulb")
data_geo %>% group_by(FuncGroup) %>% group_by(SppName) %>% summarize(count = n()) %>% arrange(desc(count))
data_geo %>% group_by(FuncGroup) %>% group_by(Group) %>% summarize(count = n()) %>% arrange(desc(count))


#HS Species
data_hs <- data %>% filter(FuncGroup == "Herb/Shrub")
data_hs %>% group_by(FuncGroup) %>% group_by(SppName) %>% summarize(count = n()) %>% arrange(desc(count))
data_hs %>% group_by(FuncGroup) %>% group_by(Group) %>% summarize(count = n()) %>% arrange(desc(count))
data_hs %>% group_by(SppName) %>% group_by(Sprout.Seed) %>% summarize(count = n()) %>% arrange(desc(count))



#TS Species
data_ts <- data %>% filter(FuncGroup == "Tall.shrub")
data_ts %>% group_by(FuncGroup) %>% group_by(SppName) %>% summarize(count = n()) %>% arrange(desc(count))


#RP Species
data_rp <- data %>% filter(FuncGroup == "Root.parasite")
data_rp %>% group_by(FuncGroup) %>% group_by(SppName) %>% summarize(count = n()) %>% arrange(desc(count))




data %>% filter(SppName == "Microdon.polygaloides") %>% group_by(FuncGroup) %>% summarize(count = n()) %>% arrange(desc(count))
data %>% filter(SppName == "Passerina.obtusifolia") %>% group_by(FuncGroup) %>% summarize(count = n()) %>% arrange(desc(count))
a <- data %>% group_by(SppName, FuncGroup, Group, Sprout.Seed) %>% summarize(count = n()) %>% arrange(desc(count))
view(a)
b <- data %>% group_by(SppName, FuncGroup) %>% summarize(count = n()) %>% arrange(desc(count))
view(b %>% filter(FuncGroup == "Herb/Shrub" | FuncGroup == "Tall.shrub" ))
view(b %>% filter(FuncGroup == "Graminoid"))
view(b %>% filter(FuncGroup == "Graminoid"))


data %>% filter(SppName == "Unknown") %>% group_by(WorkingName) %>% summarize(count = n()) %>% arrange(desc(count))


data %>% group_by(Survey) %>% summarize(count = n()) %>% arrange(desc(count))
data %>% group_by(Grid) %>% summarize(count = n()) %>% arrange(desc(count))
data %>% group_by(CT) %>% summarize(count = n()) %>% arrange(desc(count))


###################################################

#Divide Data by time step
data.a <- filter(data, Survey == 'a')
data.b <- filter(data, Survey == 'b')
data.c <- filter(data, Survey == 'c')
data.d <- filter(data, Survey == 'd')
data.e <- filter(data, Survey == 'e')
data.f <- filter(data, Survey == 'f')


#Counts By species and plot number

datA_Grouped1 <- data.a %>% group_by(Plot, Site,  SppName, FuncGroup, Group, Sprout.Seed, CT) %>%
  select( CT, Plot, FuncGroup,SppName, Group, Sprout.Seed, Volume) %>% summarize(Count = n())

datB_Grouped1 <- data.b %>% group_by(Plot, Site,  SppName, FuncGroup, Group, Sprout.Seed, CT) %>%
  select( CT, Site, FuncGroup, Group, Sprout.Seed, Volume) %>% summarize(Count = n())

datC_Grouped1 <- data.c %>% group_by(Plot, Site,  SppName, FuncGroup, Group, Sprout.Seed, CT) %>%
  select( CT, Site, FuncGroup, Group,Sprout.Seed,Volume) %>% summarize(Count = n())

datD_Grouped1 <- data.d %>% group_by(Plot, Site,  SppName, FuncGroup, Group, Sprout.Seed, CT) %>%
  select( CT, Site, FuncGroup, Group ,Sprout.Seed, Volume) %>% summarize(Count = n())

datE_Grouped1 <- data.e %>% group_by(Plot, Site,  SppName, FuncGroup, Group, Sprout.Seed, CT) %>%
  select( CT, Site, FuncGroup, Group, Sprout.Seed, Volume) %>% summarize(Count = n())

datF_Grouped1 <- data.f %>% group_by(Plot, Site,  SppName, FuncGroup, Group, Sprout.Seed, CT) %>%
  select( CT, Site, FuncGroup, Group, Sprout.Seed, Volume) %>% summarize(Count = n())


datAB_Grouped1 <- full_join(datA_Grouped1, datB_Grouped1, by = c("Plot", "SppName", "Site", "FuncGroup", "Group", "Sprout.Seed", "CT"), suffix = c(".A", ".B"))
datABC_Grouped1 <- full_join(datAB_Grouped1, datC_Grouped1, by = c("Plot", "SppName", "Site", "FuncGroup" , "Group", "Sprout.Seed",  "CT"), suffix = c(".B", ".C")) 
datABCD_Grouped1 <- full_join(datABC_Grouped1, datD_Grouped1, by = c("Plot", "SppName", "Site", "FuncGroup", "Group", "Sprout.Seed",  "CT"), suffix = c(".C", ".D"))
datABCDE_Grouped1 <- full_join(datABCD_Grouped1, datE_Grouped1, by = c("Plot", "SppName", "Site", "FuncGroup", "Group", "Sprout.Seed",  "CT"), suffix = c(".D", ".E"))
datABCDEF_Grouped1 <- full_join(datABCDE_Grouped1, datF_Grouped1, by = c("Plot", "SppName", "Site", "FuncGroup", "Group", "Sprout.Seed",  "CT"), suffix = c(".E", ".F"))

datABCDEF_Grouped1 <- datABCDEF_Grouped1 %>% replace_na(list(Count.A = 0, Count.B = 0, Count.C = 0, Count.D = 0, Count.E = 0, Count.F = 0))

#datABCDEF_Grouped1 %>% filter(SppName == "Elytropappus.rhinocerotis") %>% mutate(Group = "Woody.resprouts", Sprout.Seed = "Sprout")



dataGraminoid.C <- datABCDEF_Grouped1 %>% filter(FuncGroup == "Graminoid")
dataGeoBulb.C <- datABCDEF_Grouped1 %>% filter(FuncGroup == "Geophyte.bulb")
dataHS.C <- datABCDEF_Grouped1 %>% filter(FuncGroup == "Herb/Shrub")
dataRP.C <- datABCDEF_Grouped1 %>% filter(FuncGroup == "Root.parasite")
dataTS.C <- datABCDEF_Grouped1 %>% filter(FuncGroup == "Tall.shrub")


##Renosterveld Species
#Reseeders
data.Microdon.C <- datABCDEF_Grouped1 %>% filter(SppName == "Microdon.polygaloides")
data.Othonna.C <- datABCDEF_Grouped1 %>% filter(SppName == "Othonna.paviflora")




#Resprouters
data.Elytropappus.C <- datABCDEF_Grouped1 %>% filter(SppName == "Elytropappus.rhinocerotis")
data.Passerina.C <- datABCDEF_Grouped1 %>% filter(SppName == "Passerina.obtusifolia")


save(data.Microdon.C, file = "MicrodonDat_Plot_C.RData")
save(data.Othonna.C, file = "Othonna_Plot_C.RData")

save(data.Elytropappus.C, file = "Elytropappus_Plot_C.RData")
save(data.Passerina.C, file = "Passerina_Plot_C.RData")


##Fynbos Species
#Reseeders
data.Selago.C <- datABCDEF_Grouped1 %>% filter(SppName == "Selago.glabrata")
data.Aspalathus.C <- datABCDEF_Grouped1 %>% filter(SppName == "Aspalathus.shawii")


#Resprouters 
data.Agathosma.C <- datABCDEF_Grouped1 %>% filter(SppName == "Agathosma.capensis")
data.Wahlenbergia.C <- datABCDEF_Grouped1 %>% filter(SppName == "Wahlenbergia.nodosa")

save(data.Selago.C, file = "Selago_Plot_C.RData")
save(data.Aspalathus.C, file = "Aspalathus_Plot_C.RData")

save(data.Agathosma.C, file = "Agathosma_Plot_C.RData")
save(data.Wahlenbergia.C, file = "Wahlenbergia_Plot_C.RData")

#Parasite
data.Thesium.C <- datABCDEF_Grouped1 %>% filter(SppName == "Thesium.strictum")

save(data.Thesium.C, file = "Thesium_Plot_C.RData")

######################################################################################







