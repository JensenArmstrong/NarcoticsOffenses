library(tidyverse)
library(DT)
library(data.table)
library(dplyr)
library(stringr)
library(PGRdup)
library(gender)
library(ggplot2)

###IMPORT
GenderedData <- read_csv("C:/Users/Jensen/Documents/_Professional/_Applications/_OJO Data Analyst/OJOGenderData.csv")

###DEFINING DATAFRAMES
##drug-related offenses
DrugDF <- GenderedData %>% 
  filter(str_detect(ct_desc, "POS|CDS|CTRL|CONTR|CONTOR|PRESC|METH|DRUG
                              |SCHED|MARIJ|MARIH|HEROIN|OPIOID|PARA
                              |DIST|MANU|PREC|OXY|TRAFF|COC|ECST")) %>% 
  filter(!str_detect(ct_desc, "FIRE|GUN|WEAP|VEH|PROP|BURG|BUCK|RADIO"))

##POSSESSION
PossessionDF <- DrugDF %>% 
  select(County, ct_desc, disp, disp_type, gender) %>% 
  filter(str_detect(ct_desc, "POS|METH|HERO|COC|PREC|PRESC|OPIOID|OXY|ECST|MARI|PARA")) %>% 
  filter(!str_detect(ct_desc, "DIST|TRAF|MANU"))
#meth
PossessionDF$meth_count <- PossessionDF$ct_desc %>% 
  str_count("METH")
MethDF <-PossessionDF %>% 
  select(County, meth_count) %>% 
  count(County, 
        classification = meth_count, 
        name = "Methamphetamine.P") %>% 
  filter(classification, c(1, 2, 3, 4)) %>% 
  group_by(County) %>% 
  summarize(Methamphetamine.P = sum(Methamphetamine.P))
#marijuana
PossessionDF$marijuana_count <-PossessionDF$ct_desc %>% 
  str_count("MARI")
MarijuanaDF <-PossessionDF %>% 
  select(County, marijuana_count) %>% 
  count(County, 
        classification = marijuana_count, 
        name = "Marijuana.P") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Marijuana.P = sum(Marijuana.P))
#heroin
PossessionDF$heroin_count <-PossessionDF$ct_desc %>% 
  str_count("HERO")
HeroinDF <-PossessionDF %>% 
  select(County, heroin_count) %>% 
  count(County, 
        classification = heroin_count, 
        name = "Heroin.P") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Heroin.P = sum(Heroin.P))
#precursor
PossessionDF$precursor_count <-PossessionDF$ct_desc %>% 
  str_count("PREC")
PrecursorDF <-PossessionDF %>% 
  select(County, precursor_count) %>% 
  count(County, 
        classification = precursor_count, 
        name = "Precursor.P") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Precursor.P = sum(Precursor.P))
#prescription
PossessionDF$prescription_count <-PossessionDF$ct_desc %>% 
  str_count("PRESC")
PrescriptionDF <-PossessionDF %>% 
  select(County, prescription_count) %>% 
  count(County, 
        classification = prescription_count, 
        name = "Prescription.P") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Prescription.P = sum(Prescription.P))
#oxy
PossessionDF$oxycontin_count <-PossessionDF$ct_desc %>% 
  str_count("OXY")
OxycontinDF <-PossessionDF %>% 
  select(County, oxycontin_count) %>% 
  count(County, 
        classification = oxycontin_count, 
        name = "Oxycontin.P") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Oxycontin.P = sum(Oxycontin.P))
#opioid
PossessionDF$opioid_count <-PossessionDF$ct_desc %>% 
  str_count("OPIOID")
OpioidDF <-PossessionDF %>% 
  select(County, opioid_count) %>% 
  count(County, 
        classification = opioid_count, 
        name = "Opioid.P") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Opioid.P = sum(Opioid.P))
#cocaine
PossessionDF$cocaine_count <-PossessionDF$ct_desc %>% 
  str_count("COC")
CocaineDF <-PossessionDF %>% 
  select(County, cocaine_count) %>% 
  count(County, 
        classification = cocaine_count, 
        name = "Cocaine.P") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Cocaine.P = sum(Cocaine.P))
#ecstasy
PossessionDF$ecstasy_count <-PossessionDF$ct_desc %>% 
  str_count("ECST")
EcstasyDF <-PossessionDF %>% 
  select(County, ecstasy_count) %>% 
  count(County, 
        classification = ecstasy_count, 
        name = "Ecstasy.P") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Ecstasy.P = sum(Ecstasy.P))
#paraphernalia
PossessionDF$paraphernalia_count <-PossessionDF$ct_desc %>% 
  str_count("PARA")
ParaphernaliaDF <-PossessionDF %>% 
  select(County, paraphernalia_count) %>% 
  count(County, 
        classification = paraphernalia_count, 
        name = "Paraphernalia.P") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Paraphernalia.P = sum(Paraphernalia.P))
##JOIN possession type tables
PossessionType <-
  left_join(FIPS, MarijuanaDF, by = NULL)
PossessionType <-
  left_join(PossessionType, EcstasyDF, by = NULL)
PossessionType <-
  left_join(PossessionType, MethDF, by = NULL)
PossessionType <-
  left_join(PossessionType, CocaineDF, by = NULL)
PossessionType <-
  left_join(PossessionType, HeroinDF, by = NULL)
PossessionType <-
  left_join(PossessionType, OpioidDF, by = NULL)
PossessionType <-
  left_join(PossessionType, OxycontinDF, by = NULL)
PossessionType <-
  left_join(PossessionType, PrescriptionDF, by = NULL)
PossessionType <-
  left_join(PossessionType, PrecursorDF, by = NULL)
PossessionType <-
  left_join(PossessionType, ParaphernaliaDF, by = NULL)
##cleaning table
PossessionType[is.na(PossessionType)] <- 0
##export DF
write_csv(PossessionType, "C:/Users/Jensen/Documents/_Graduate/Multimedia Journalism/Dataframes/PossessionType.csv")

##DISTRIBUTION
DistributionDF <- DrugDF %>% 
  select(County, ct_desc, disp, disp_type, gender) %>% 
  filter(str_detect(ct_desc, "DIST|METH|HERO|COC|PREC|PRESC|OPIOID|OXY|ECST|MARI|PARA")) %>% 
  filter(!str_detect(ct_desc, "POS|MANU|TRAF"))
#meth
DistributionDF$meth_count <-DistributionDF$ct_desc %>% 
  str_count("METH")
MethDF <-DistributionDF %>% 
  select(County, meth_count) %>% 
  count(County, 
        classification = meth_count, 
        name = "Methamphetamine.D") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Methamphetamine.D = sum(Methamphetamine.D))
#marijuana
DistributionDF$marijuana_count <-DistributionDF$ct_desc %>% 
  str_count("MARI")
MarijuanaDF <-DistributionDF %>% 
  select(County, marijuana_count) %>% 
  count(County, 
        classification = marijuana_count, 
        name = "Marijuana.D") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Marijuana.D = sum(Marijuana.D))
#heroin
DistributionDF$heroin_count <-DistributionDF$ct_desc %>% 
  str_count("HERO")
HeroinDF <-DistributionDF %>% 
  select(County, heroin_count) %>% 
  count(County, 
        classification = heroin_count, 
        name = "Heroin.D") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Heroin.D = sum(Heroin.D))
#precursor
DistributionDF$precursor_count <-DistributionDF$ct_desc %>% 
  str_count("PREC")
PrecursorDF <-DistributionDF %>% 
  select(County, precursor_count) %>% 
  count(County, 
        classification = precursor_count, 
        name = "Precursor.D") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Precursor.D = sum(Precursor.D))
#prescription
DistributionDF$prescription_count <-DistributionDF$ct_desc %>% 
  str_count("PRESC")
PrescriptionDF <-DistributionDF %>% 
  select(County, prescription_count) %>% 
  count(County, 
        classification = prescription_count, 
        name = "Prescription.D") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Prescription.D = sum(Prescription.D))
#oxy
DistributionDF$oxycontin_count <-DistributionDF$ct_desc %>% 
  str_count("OXY")
OxycontinDF <-DistributionDF %>% 
  select(County, oxycontin_count) %>% 
  count(County, 
        classification = oxycontin_count, 
        name = "Oxycontin.D") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Oxycontin.D = sum(Oxycontin.D))
#opioid
DistributionDF$opioid_count <-DistributionDF$ct_desc %>% 
  str_count("OPIOID")
OpioidDF <-DistributionDF %>% 
  select(County, opioid_count) %>% 
  count(County, 
        classification = opioid_count, 
        name = "Opioid.D") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Opioid.D = sum(Opioid.D))
#cocaine
DistributionDF$cocaine_count <-DistributionDF$ct_desc %>% 
  str_count("COC")
CocaineDF <-DistributionDF %>% 
  select(County, cocaine_count) %>% 
  count(County, 
        classification = cocaine_count, 
        name = "Cocaine.D") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Cocaine.D = sum(Cocaine.D))
#ecstasy
DistributionDF$ecstasy_count <-DistributionDF$ct_desc %>% 
  str_count("ECST")
EcstasyDF <-DistributionDF %>% 
  select(County, ecstasy_count) %>% 
  count(County, 
        classification = ecstasy_count, 
        name = "Ecstasy.D") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Ecstasy.D = sum(Ecstasy.D))
#paraphernalia
DistributionDF$paraphernalia_count <-DistributionDF$ct_desc %>% 
  str_count("PARA")
ParaphernaliaDF <-DistributionDF %>% 
  select(County, paraphernalia_count) %>% 
  count(County, 
        classification = paraphernalia_count, 
        name = "Paraphernalia.D") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Paraphernalia.D = sum(Paraphernalia.D))
##JOIN possession type tables
DistributionType <-
  left_join(FIPS, MarijuanaDF, by = NULL)
DistributionType <-
  left_join(DistributionType, EcstasyDF, by = NULL)
DistributionType <-
  left_join(DistributionType, MethDF, by = NULL)
DistributionType <-
  left_join(DistributionType, CocaineDF, by = NULL)
DistributionType <-
  left_join(DistributionType, HeroinDF, by = NULL)
DistributionType <-
  left_join(DistributionType, OpioidDF, by = NULL)
DistributionType <-
  left_join(DistributionType, OxycontinDF, by = NULL)
DistributionType <-
  left_join(DistributionType, PrescriptionDF, by = NULL)
DistributionType <-
  left_join(DistributionType, PrecursorDF, by = NULL)
DistributionType <-
  left_join(DistributionType, ParaphernaliaDF, by = NULL)
##cleaning table
DistributionType[is.na(DistributionType)] <- 0
##export DF
write_csv(DistributionType,"C:/Users/Jensen/Documents/_Graduate/Multimedia Journalism/Dataframes/DistributionType.csv")


##trafficking
TraffickingDF <- DrugDF %>% 
  select(County, ct_desc, disp, disp_type, gender) %>% 
  filter(str_detect(ct_desc, "TRAF|METH|HERO|COC|PREC|PRESC|OPIOID|OXY|ECST|MARI|PARA")) %>% 
  filter(!str_detect(ct_desc, "POS|DIST|MANU"))
#meth
TraffickingDF$meth_count <-TraffickingDF$ct_desc %>% 
  str_count("METH")
MethDF <-TraffickingDF %>% 
  select(County, meth_count) %>% 
  count(County, 
        classification = meth_count, 
        name = "Methamphetamine.T") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Methamphetamine.T = sum(Methamphetamine.T))
#marijuana
TraffickingDF$marijuana_count <-TraffickingDF$ct_desc %>% 
  str_count("MARI")
MarijuanaDF <-TraffickingDF %>% 
  select(County, marijuana_count) %>% 
  count(County, 
        classification = marijuana_count, 
        name = "Marijuana.T") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Marijuana.T = sum(Marijuana.T))
#heroin
TraffickingDF$heroin_count <-TraffickingDF$ct_desc %>% 
  str_count("HERO")
HeroinDF <-TraffickingDF %>% 
  select(County, heroin_count) %>% 
  count(County, 
        classification = heroin_count, 
        name = "Heroin.T") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Heroin.T = sum(Heroin.T))
#precursor
TraffickingDF$precursor_count <-TraffickingDF$ct_desc %>% 
  str_count("PREC")
PrecursorDF <-TraffickingDF %>% 
  select(County, precursor_count) %>% 
  count(County, 
        classification = precursor_count, 
        name = "Precursor.T") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Precursor.T = sum(Precursor.T))
#prescription
TraffickingDF$prescription_count <-TraffickingDF$ct_desc %>% 
  str_count("PRESC")
PrescriptionDF <-TraffickingDF %>% 
  select(County, prescription_count) %>% 
  count(County, 
        classification = prescription_count, 
        name = "Prescription.T") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Prescription.T = sum(Prescription.T))
#oxy
TraffickingDF$oxycontin_count <-TraffickingDF$ct_desc %>% 
  str_count("OXY")
OxycontinDF <-TraffickingDF %>% 
  select(County, oxycontin_count) %>% 
  count(County, 
        classification = oxycontin_count, 
        name = "Oxycontin.T") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Oxycontin.T = sum(Oxycontin.T))
#opioid
TraffickingDF$opioid_count <-TraffickingDF$ct_desc %>% 
  str_count("OPIOID")
OpioidDF <-TraffickingDF %>% 
  select(County, opioid_count) %>% 
  count(County, 
        classification = opioid_count, 
        name = "Opioid.T") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Opioid.T = sum(Opioid.T))
#cocaine
TraffickingDF$cocaine_count <-TraffickingDF$ct_desc %>% 
  str_count("COC")
CocaineDF <-TraffickingDF %>% 
  select(County, cocaine_count) %>% 
  count(County, 
        classification = cocaine_count, 
        name = "Cocaine.T") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Cocaine.T = sum(Cocaine.T))
#ecstasy
TraffickingDF$ecstasy_count <-TraffickingDF$ct_desc %>% 
  str_count("ECST")
EcstasyDF <-TraffickingDF %>% 
  select(County, ecstasy_count) %>% 
  count(County, 
        classification = ecstasy_count, 
        name = "Ecstasy.T") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Ecstasy.T = sum(Ecstasy.T))
#paraphernalia
TraffickingDF$paraphernalia_count <-TraffickingDF$ct_desc %>% 
  str_count("PARA")
ParaphernaliaDF <-TraffickingDF %>% 
  select(County, paraphernalia_count) %>% 
  count(County, 
        classification = paraphernalia_count, 
        name = "Paraphernalia.T") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Paraphernalia.T = sum(Paraphernalia.T))
##JOIN possession type tables
TraffickingType <-
  left_join(FIPS, MarijuanaDF, by = NULL)
TraffickingType <-
  left_join(TraffickingType, EcstasyDF, by = NULL)
TraffickingType <-
  left_join(TraffickingType, MethDF, by = NULL)
TraffickingType <-
  left_join(TraffickingType, CocaineDF, by = NULL)
TraffickingType <-
  left_join(TraffickingType, HeroinDF, by = NULL)
TraffickingType <-
  left_join(TraffickingType, OpioidDF, by = NULL)
TraffickingType <-
  left_join(TraffickingType, OxycontinDF, by = NULL)
TraffickingType <-
  left_join(TraffickingType, PrescriptionDF, by = NULL)
TraffickingType <-
  left_join(TraffickingType, PrecursorDF, by = NULL)
TraffickingType <-
  left_join(TraffickingType, ParaphernaliaDF, by = NULL)
##cleaning table
TraffickingType[is.na(TraffickingType)] <- 0
##export DF
write_csv(TraffickingType,"C:/Users/Jensen/Documents/_Graduate/Multimedia Journalism/Dataframes/TraffickingType.csv")

##MANUFACTURING
ManufacturingDF <- DrugDF %>% 
  select(County, ct_desc, disp, disp_type, gender) %>% 
  filter(str_detect(ct_desc, "MANU|METH|HERO|COC|PREC|PRESC|OPIOID|OXY|ECST|MARI|PARA")) %>% 
  filter(!str_detect(ct_desc, "POS|DIST|TRAF"))
#meth
ManufacturingDF$meth_count <-ManufacturingDF$ct_desc %>% 
  str_count("METH")
MethDF <-ManufacturingDF %>% 
  select(County, meth_count) %>% 
  count(County, 
        classification = meth_count, 
        name = "Methamphetamine.M") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Methamphetamine.M = sum(Methamphetamine.M))
#marijuana
ManufacturingDF$marijuana_count <-ManufacturingDF$ct_desc %>% 
  str_count("MARI")
MarijuanaDF <-ManufacturingDF %>% 
  select(County, marijuana_count) %>% 
  count(County, 
        classification = marijuana_count, 
        name = "Marijuana.M") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Marijuana.M = sum(Marijuana.M))
#heroin
ManufacturingDF$heroin_count <-ManufacturingDF$ct_desc %>% 
  str_count("HERO")
HeroinDF <-ManufacturingDF %>% 
  select(County, heroin_count) %>% 
  count(County, 
        classification = heroin_count, 
        name = "Heroin.M") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Heroin.M = sum(Heroin.M))
#precursor
ManufacturingDF$precursor_count <-ManufacturingDF$ct_desc %>% 
  str_count("PREC")
PrecursorDF <-ManufacturingDF %>% 
  select(County, precursor_count) %>% 
  count(County, 
        classification = precursor_count, 
        name = "Precursor.M") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Precursor.M = sum(Precursor.M))
#prescription
ManufacturingDF$prescription_count <-ManufacturingDF$ct_desc %>% 
  str_count("PRESC")
PrescriptionDF <-ManufacturingDF %>% 
  select(County, prescription_count) %>% 
  count(County, 
        classification = prescription_count, 
        name = "Prescription.M") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Prescription.M = sum(Prescription.M))
#oxy
ManufacturingDF$oxycontin_count <-ManufacturingDF$ct_desc %>% 
  str_count("OXY")
OxycontinDF <-ManufacturingDF %>% 
  select(County, oxycontin_count) %>% 
  count(County, 
        classification = oxycontin_count, 
        name = "Oxycontin.M") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Oxycontin.M = sum(Oxycontin.M))
#opioid
ManufacturingDF$opioid_count <-ManufacturingDF$ct_desc %>% 
  str_count("OPIOID")
OpioidDF <-ManufacturingDF %>% 
  select(County, opioid_count) %>% 
  count(County, 
        classification = opioid_count, 
        name = "Opioid.M") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Opioid.M = sum(Opioid.M))
#cocaine
ManufacturingDF$cocaine_count <-ManufacturingDF$ct_desc %>% 
  str_count("COC")
CocaineDF <-ManufacturingDF %>% 
  select(County, cocaine_count) %>% 
  count(County, 
        classification = cocaine_count, 
        name = "Cocaine.M") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Cocaine.M = sum(Cocaine.M))
#ecstasy
ManufacturingDF$ecstasy_count <-ManufacturingDF$ct_desc %>% 
  str_count("ECST")
EcstasyDF <-ManufacturingDF %>% 
  select(County, ecstasy_count) %>% 
  count(County, 
        classification = ecstasy_count, 
        name = "Ecstasy.M") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Ecstasy.M = sum(Ecstasy.M))
#paraphernalia
ManufacturingDF$paraphernalia_count <-ManufacturingDF$ct_desc %>% 
  str_count("PARA")
ParaphernaliaDF <-ManufacturingDF %>% 
  select(County, paraphernalia_count) %>% 
  count(County, 
        classification = paraphernalia_count, 
        name = "Paraphernalia.M") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Paraphernalia.M = sum(Paraphernalia.M))
##JOIN possession type tables
ManufacturingType <-
  left_join(FIPS, MarijuanaDF, by = NULL)
ManufacturingType <-
  left_join(ManufacturingType, EcstasyDF, by = NULL)
ManufacturingType <-
  left_join(ManufacturingType, MethDF, by = NULL)
ManufacturingType <-
  left_join(ManufacturingType, CocaineDF, by = NULL)
ManufacturingType <-
  left_join(ManufacturingType, HeroinDF, by = NULL)
ManufacturingType <-
  left_join(ManufacturingType, OpioidDF, by = NULL)
ManufacturingType <-
  left_join(ManufacturingType, OxycontinDF, by = NULL)
ManufacturingType <-
  left_join(ManufacturingType, PrescriptionDF, by = NULL)
ManufacturingType <-
  left_join(ManufacturingType, PrecursorDF, by = NULL)
ManufacturingType <-
  left_join(ManufacturingType, ParaphernaliaDF, by = NULL)
##cleaning table
ManufacturingType[is.na(ManufacturingType)] <- 0
##export DF
write_csv(ManufacturingType,"C:/Users/Jensen/Documents/_Graduate/Multimedia Journalism/Dataframes/ManufacturingType.csv")

##TOTALS
TotalDF <- DrugDF %>% 
  select(County, ct_desc, disp, disp_type, gender) %>% 
  filter(str_detect(ct_desc, "METH|HERO|COC|PREC|PRESC|OPIOID|OXY|ECST|MARI|PARA"))
#meth
TotalDF$meth_count <-TotalDF$ct_desc %>% 
  str_count("METH")
MethDF <-TotalDF %>% 
  select(County, meth_count) %>% 
  count(County, 
        classification = meth_count, 
        name = "Methamphetamine_Total") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Methamphetamine_Total = sum(Methamphetamine_Total))
#marijuana
TotalDF$marijuana_count <-TotalDF$ct_desc %>% 
  str_count("MARI")
MarijuanaDF <-TotalDF %>% 
  select(County, marijuana_count) %>% 
  count(County, 
        classification = marijuana_count, 
        name = "Marijuana_Total") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Marijuana_Total = sum(Marijuana_Total))
#heroin
TotalDF$heroin_count <-TotalDF$ct_desc %>% 
  str_count("HERO")
HeroinDF <-TotalDF %>% 
  select(County, heroin_count) %>% 
  count(County, 
        classification = heroin_count, 
        name = "Heroin_Total") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Heroin_Total = sum(Heroin_Total))
#precursor
TotalDF$precursor_count <-TotalDF$ct_desc %>% 
  str_count("PREC")
PrecursorDF <-TotalDF %>% 
  select(County, precursor_count) %>% 
  count(County, 
        classification = precursor_count, 
        name = "Precursor_Total") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Precursor_Total = sum(Precursor_Total))
#prescription
TotalDF$prescription_count <-TotalDF$ct_desc %>% 
  str_count("PRESC")
PrescriptionDF <-TotalDF %>% 
  select(County, prescription_count) %>% 
  count(County, 
        classification = prescription_count, 
        name = "Prescription_Total") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Prescription_Total = sum(Prescription_Total))
#oxy
TotalDF$oxycontin_count <-TotalDF$ct_desc %>% 
  str_count("OXY")
OxycontinDF <-TotalDF %>% 
  select(County, oxycontin_count) %>% 
  count(County, 
        classification = oxycontin_count, 
        name = "Oxycontin_Total") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Oxycontin_Total = sum(Oxycontin_Total))
#opioid
TotalDF$opioid_count <-TotalDF$ct_desc %>% 
  str_count("OPIOID")
OpioidDF <-TotalDF %>% 
  select(County, opioid_count) %>% 
  count(County, 
        classification = opioid_count, 
        name = "Opioid_Total") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Opioid_Total = sum(Opioid_Total))
#cocaine
TotalDF$cocaine_count <-TotalDF$ct_desc %>% 
  str_count("COC")
CocaineDF <-TotalDF %>% 
  select(County, cocaine_count) %>% 
  count(County, 
        classification = cocaine_count, 
        name = "Cocaine_Total") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Cocaine_Total = sum(Cocaine_Total))
#ecstasy
TotalDF$ecstasy_count <-TotalDF$ct_desc %>% 
  str_count("ECST")
EcstasyDF <-TotalDF %>% 
  select(County, ecstasy_count) %>% 
  count(County, 
        classification = ecstasy_count, 
        name = "Ecstasy_Total") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Ecstasy_Total = sum(Ecstasy_Total))
#paraphernalia
TotalDF$paraphernalia_count <-TotalDF$ct_desc %>% 
  str_count("PARA")
ParaphernaliaDF <-TotalDF %>% 
  select(County, paraphernalia_count) %>% 
  count(County, 
        classification = paraphernalia_count, 
        name = "Paraphernalia_Total") %>% 
  filter(classification, c(1, 2, 3)) %>% 
  group_by(County) %>% 
  summarize(Paraphernalia_Total = sum(Paraphernalia_Total))
##JOIN possession type tables
TotalType <-
  left_join(FIPS, MarijuanaDF, by = NULL)
TotalType <-
  left_join(TotalType, EcstasyDF, by = NULL)
TotalType <-
  left_join(TotalType, MethDF, by = NULL)
TotalType <-
  left_join(TotalType, CocaineDF, by = NULL)
TotalType <-
  left_join(TotalType, HeroinDF, by = NULL)
TotalType <-
  left_join(TotalType, OpioidDF, by = NULL)
TotalType <-
  left_join(TotalType, OxycontinDF, by = NULL)
TotalType <-
  left_join(TotalType, PrescriptionDF, by = NULL)
TotalType <-
  left_join(TotalType, PrecursorDF, by = NULL)
TotalType <-
  left_join(TotalType, ParaphernaliaDF, by = NULL)
##cleaning table
TotalType[is.na(TotalType)] <- 0
TotalType <- TotalType %>% 
  subset(select = -c(FIPS))
###export DF
write_csv(TotalType,"C:/Users/Jensen/Documents/_Graduate/Multimedia Journalism/Dataframes/TotalType.csv")
