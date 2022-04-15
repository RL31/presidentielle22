library(tidyverse)
library(sf)

##########################################################################################
# Programme qui équirépartit les données RP 2018 à l'Iris  dans les bureaux de vote 2022 #
##########################################################################################

RP <- read.csv("donnees/base-ic-activite-residents-2018.csv",sep=";")

fond_iris <- st_read("donnees/CONTOURS-IRIS_2-1__SHP__FRA_2020-01-01/CONTOURS-IRIS/1_DONNEES_LIVRAISON_2020-12-00282/CONTOURS-IRIS_2-1_SHP_LAMB93_FXX-2020/CONTOURS-IRIS.shp") %>% 
  filter(INSEE_COM=="31555")
fond_bv17 <- st_read("donnees/elections-2017-decoupage-des-bureaux-de-vote.shp")
fond_bv22 <- st_read("donnees/elec-2021-decoupage-bdv.shp")

intersection_bv_iris <- st_intersection(fond_iris %>% 
                                          mutate(surface_iris=st_area(.)),
                                        fond_bv22 %>%
                                          st_transform(2154) %>% 
                                          mutate(surface_bv=st_area(.))) %>% 
  mutate(surface_intersection=st_area(.),
         part_iris=as.numeric(surface_intersection/surface_iris))

bv22_infosRP <- intersection_bv_iris %>% 
  left_join(RP,by=c("CODE_IRIS"="IRIS")) %>% 
  mutate(CHOM = P18_CHOM1564*part_iris,
         NSAL = P18_NSAL15P*part_iris,
         AGRI = C18_ACTOCC1564_CS1*part_iris,
         ART_COM = C18_ACTOCC1564_CS2*part_iris,
         CAD = C18_ACTOCC1564_CS3*part_iris,
         P_INT = C18_ACTOCC1564_CS4*part_iris,
         EMP = C18_ACTOCC1564_CS5*part_iris,
         OUV = C18_ACTOCC1564_CS6*part_iris,
         RETR = P18_RETR1564*part_iris,
         ETUD = P18_ETUD1564 *part_iris,
         POP=P18_POP1564*part_iris,
         VELO=C18_ACTOCC15P_VELO*part_iris,
         ACTOPP=C18_ACTOCC15P*part_iris) %>% 
  group_by(uniq_bdv) %>% 
  summarise(tx_chom=sum(CHOM)/sum(POP)*100,
            tx_nsal=sum(NSAL)/sum(POP)*100,
            tx_agri=sum(AGRI)/sum(POP)*100,
            tx_artcom=sum(ART_COM)/sum(POP)*100,
            tx_cad=sum(CAD)/sum(POP)*100,
            tx_pint=sum(P_INT)/sum(POP)*100,
            tx_emp=sum(EMP)/sum(POP)*100,
            tx_etud = sum(ETUD)/sum(POP)*100,
            tx_retr = sum(RETR)/sum(POP)*100,
            tx_ouv=sum(OUV)/sum(POP)*100,
            tx_velo=sum(VELO)/sum(ACTOPP)*100) %>% 
  ungroup()
