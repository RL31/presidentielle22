library(sf)
library(tidyverse)

# Problème de doublons sur Toulouse
# grenoble en geojson
#
# Chargement des contours trouvés sur open data
# homogénéisation des contours autres villes
lyon <- st_read("donnees/vdl_vie_citoyenne.contour_de_bureau_de_vote.geojson") %>% select(num_bureau) %>% 
  mutate(bdv=as.character(num_bureau),
         Libellé.de.la.commune="Lyon")%>%
 filter(!bdv %in% c("604","350")) %>% #pb géométrie
  # st_set_crs(4326) %>% 
 st_transform(2154)%>%
  select(bdv,Libellé.de.la.commune) %>%
  st_make_valid()
nantes <- st_read("donnees/244400404_decoupage-geographique-bureaux-vote-nantes.shp") %>% select(numero_bure) %>% 
  mutate(bdv=as.character(numero_bure),
         Libellé.de.la.commune="Nantes")%>%
  st_transform(2154) %>%
  select(bdv,Libellé.de.la.commune)# On en perd 1 avec 1 votant
orleans <- st_read("donnees/administratifadm_secteurs_vote.shp") %>% select(num_bv) %>% 
  mutate(bdv=as.character(num_bv),
         Libellé.de.la.commune="Orléans")%>%
  st_transform(2154) %>%
  select(bdv,Libellé.de.la.commune)
strasbourg <- st_read("donnees/bureaux-de-vote.shp") %>% select(id_bureau) %>% 
  mutate(bdv=as.character(id_bureau),
         Libellé.de.la.commune="Strasbourg")%>%
  st_transform(2154) %>%
  select(bdv,Libellé.de.la.commune)
havre <- st_read("donnees/OD_BUREAU_VOTE_2020.shp") %>% select(NUM_BUREAU) %>% 
  mutate(bdv=as.character(NUM_BUREAU),
         Libellé.de.la.commune="Le Havre")%>%
  st_transform(2154)%>%
  select(bdv,Libellé.de.la.commune)
brest <-  st_read("donnees/LIM_ADM_BureauxVote_s.shp") %>%  select(BVOTE,DEPCO) %>% 
  mutate(bdv=as.character(BVOTE),
         Libellé.de.la.commune=if_else(DEPCO=="29019","Brest","Gouesnou"))%>%
  st_transform(2154) %>%
  select(bdv,Libellé.de.la.commune)
rennes <-  st_read("donnees/perimetres-bureaux-de-vote.shp") %>% select(num_bureau) %>% 
  mutate(bdv=as.character(num_bureau),
         Libellé.de.la.commune="Rennes")%>%
  st_transform(2154) %>%
  select(bdv,Libellé.de.la.commune)# manque 1 bureau de 7 votants
roubaix <-  st_read("donnees/delimitation-bureaux-de-vote-2017.shp") %>% select(numero_1) %>% 
  mutate(bdv=as.character(numero_1),
         Libellé.de.la.commune="Roubaix")%>%
  st_transform(2154) %>%
  select(bdv,Libellé.de.la.commune)
nimes <- st_read("donnees/decoupage-geographique-des-bureaux-de-vote-de-la-ville-de-nimes.shp") %>% select(nom_prefect)  %>% 
  mutate(bdv=as.character(as.integer(nom_prefect)),
         Libellé.de.la.commune="Nîmes")%>%
  st_transform(2154) %>%
  select(bdv,Libellé.de.la.commune)
bordeaux <- st_read("donnees/el_bureauvote_s.shp") %>% select(code) %>% 
  mutate(bdv=as.character(code),
         Libellé.de.la.commune="Bordeaux")%>%
  st_transform(2154) %>%
  select(bdv,Libellé.de.la.commune)# manque 1 bureau de 2 votants
angers <- st_read("donnees/election-presidentielle-2022-premier-tour-angers.shp") %>% select(bv) %>% 
  mutate(bdv=as.character(bv),
         Libellé.de.la.commune="Angers")%>%
  st_transform(2154) %>%
  select(bdv,Libellé.de.la.commune)
colmar <- st_read("donnees/colmar.geojson")  %>% filter(com_insee=="68066") %>% select(num_bureau_vote) %>% 
  mutate(bdv=as.character(num_bureau_vote),
         Libellé.de.la.commune="Colmar")%>%
  st_set_crs(3948) %>%
  st_transform(2154) %>%
  select(bdv,Libellé.de.la.commune) # manque 1
mulhouse <- st_read("donnees/perimetres_des_bureaux_de_vote_depuis_2015_sur_mulhouse.shp")  %>% select(bureau_num) %>% 
  mutate(bdv=as.character(bureau_num),
         Libellé.de.la.commune="Mulhouse")%>%
  st_transform(2154) %>%
  select(bdv,Libellé.de.la.commune)
grenoble <- st_read("donnees/BUREAUX_DE_VOTE_EPSG4326.json")  %>% select(SDEC_NUM) %>% 
  mutate(bdv=as.character(SDEC_NUM),
         Libellé.de.la.commune="Grenoble")%>%
  st_transform(2154) %>%
  select(bdv,Libellé.de.la.commune) # manque 1
toulouse <-  st_read("donnees/elec-2021-decoupage-bdv.shp") %>% select(uniq_bdv) %>% 
  mutate(bdv=as.character(uniq_bdv),
         Libellé.de.la.commune="Toulouse")%>%
  st_transform(2154) %>%
  select(bdv,Libellé.de.la.commune)
poitiers <- st_read("donnees/citoyennete-decoupage-bureaux-de-vote-grand-poitiers-donnees-de-reference.shp") %>% select(num_bureau) %>% 
  mutate(bdv=as.character(num_bureau),
         Libellé.de.la.commune="Poitiers")%>%
  st_transform(2154) %>%
  select(bdv,Libellé.de.la.commune)

# On regroupe les différents zonages
contours <-  bordeaux %>% 
  bind_rows(brest,havre,lyon,nantes,nimes,orleans,rennes,roubaix,strasbourg,
            angers,colmar,grenoble,mulhouse,toulouse,poitiers) 


# On charge les résultats par bureau de vote
resultats <-  read.csv("donnees/resultats-par-niveau-burvot-t1-france-entiere.csv",sep=";") %>% 
  filter(Libellé.de.la.commune %in% c("Lyon","Bordeaux","Roubaix","Rennes","Nîmes","Colmar","Mulhouse",
                                      "Grenoble","Angers","Toulouse","Poitiers",
                                      "Nantes","Le Havre","Orléans","Gouesnou","Brest","Strasbourg")) %>% 
  rename(bdv=Code.du.b.vote) %>% 
  mutate(bdv=if_else(Libellé.de.la.commune=="Toulouse",str_pad(bdv,3,"left","0"),bdv))

# Fusion des contours avec leur résultat
base_resultats <- contours %>% 
  inner_join(resultats,by=c("Libellé.de.la.commune","bdv")) #%>%

# base_resultats %>%
#   st_drop_geometry() %>%
#   as.data.frame() %>%
#   count(Libellé.de.la.commune) %>%
#   # summarise(n=sum(n))
#   view()
# 
# base_resultats %>%
#   st_drop_geometry()%>%
#   as.data.frame()%>%
#   ungroup()%>%
#   summarise(tot=sum(Exprimés))

##########################################################################################
# Programme qui équirépartit les données RP 2018 à l'Iris  dans les bureaux de vote 2022 #
##########################################################################################

RP <- read.csv("donnees/base-ic-activite-residents-2018.csv",sep=";")
RP_dipl <- read.csv("donnees/base-ic-diplomes-formation-2018.csv",sep=";") %>%
  mutate(P18_NSCOL15P_SUP = P18_NSCOL15P_SUP2+P18_NSCOL15P_SUP34+P18_NSCOL15P_SUP5) %>% 
  select(IRIS,P18_NSCOL15P,P18_NSCOL15P_SUP)
RP_immig <- read.csv("donnees/base-ic-evol-struct-pop-2018.csv",sep=";") %>%
  select(IRIS,P18_POP ,P18_POP_IMM )

liste_codes <- c("33063","29061","29019","76351","69123","44109","30189","45234","35238","59512","67482","31555","49007",
                 "68224","68066","38185","86194",
                 "69381","69382","69383","69384","69385","69386","69387","69388","69389")

fond_iris <- st_read("donnees/CONTOURS-IRIS_2-1__SHP__FRA_2020-01-01/CONTOURS-IRIS/1_DONNEES_LIVRAISON_2020-12-00282/CONTOURS-IRIS_2-1_SHP_LAMB93_FXX-2020/CONTOURS-IRIS.shp") %>% 
  filter(INSEE_COM %in% liste_codes)

intersection_bv_iris <- st_intersection(fond_iris %>% select(CODE_IRIS,geometry) %>%
                                          mutate(surface_iris=st_area(.)),
                                        base_resultats %>%
                                          mutate(surface_bv=st_area(.))) %>% 
  mutate(surface_intersection=st_area(.),
         part_iris=as.numeric(surface_intersection/surface_iris))

base_resultats %>% 
    count(Libellé.de.la.commune) %>% view()

bv22_RP_toutes_villes <- intersection_bv_iris %>% 
  left_join(RP,by=c("CODE_IRIS"="IRIS")) %>% 
  left_join(RP_dipl,by=c("CODE_IRIS"="IRIS")) %>% 
  left_join(RP_immig,by=c("CODE_IRIS"="IRIS")) %>% 
  mutate(IMMIG = P18_POP_IMM*part_iris,
         P18_POP = P18_POP*part_iris,
         DSUP = P18_NSCOL15P_SUP*part_iris,
         P18_NSCOL15P = P18_NSCOL15P*part_iris,
         CHOM = P18_CHOM1564*part_iris,
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
         ACTOPP=C18_ACTOCC15P*part_iris,
         SAL=P18_SAL15P*part_iris,
         CDI=P18_SAL15P_CDI *part_iris,
         CDD=P18_SAL15P_CDD *part_iris,
         INTERIM=P18_SAL15P_INTERIM *part_iris,
         EMPAID=P18_SAL15P_EMPAID *part_iris) %>% 
  group_by(bdv,Libellé.de.la.commune) %>% 
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
            tx_dsup = sum(P18_NSCOL15P_SUP)/sum(P18_NSCOL15P)*100,
            tx_immig = sum(IMMIG)/sum(P18_POP)*100,
            
            tx_cdi = sum(CDI)/sum(SAL)*100,
            tx_cdd = sum(CDD)/sum(SAL)*100,
            tx_interim = sum(INTERIM)/sum(SAL)*100,
            tx_empaid = sum(EMPAID)/sum(SAL)*100,
            
            tx_velo=sum(VELO)/sum(ACTOPP)*100) %>% 
  ungroup() %>%
  left_join(resultats,by=c("bdv","Libellé.de.la.commune"))

filosofi_par_bdv22_toutes_villes <- calculAgregatsZones(cheminFichierContoursSHP=base_resultats %>%
                                                          select(geometry,bdv,Libellé.de.la.commune), 
                                                        SHP=0,
                                                        cheminFichierCSV="donnees/Filosofi2017_carreaux_200m_met.csv", 
                                                        listeCodesCommune=liste_codes,
                                                        listeIndic=listeIndicDef)


donnes_reg_toutes_villes <- bv22_RP_toutes_villes %>% 
  left_join(filosofi_par_bdv22_toutes_villes)%>%# ,by=c("bdv","Libellé.de.la.commune")) %>% 
  mutate(ndv=Ind_snv/Ind/1000,
         mpauv = Men_pauv/Men*100,
         mprop = Men_prop/Men*100,
         ind65p = (Ind_65_79 + Ind_80p) / Ind *100,
         ind1824 = (Ind_18_24) / Ind *100,
        ind2539 = (Ind_25_39) / Ind *100,
        evo_demo = case_when(Libellé.de.la.commune %in% c("Mulhouse","Brest","Le Havre")~"0",
                                Libellé.de.la.commune %in% c("Angers","Gouesnou","Grenoble","Orléans","Roubaix")~"1",
                                Libellé.de.la.commune %in% c("Colmar","Poitiers","Rennes","Strasbourg")~"2",
                                TRUE~"3")     ) %>% 
  st_drop_geometry() %>% 
  as.data.frame() %>% 
  select(bdv,Libellé.de.la.commune, Inscrits,Abstentions,Exprimés,
         macron,melenchon,lepen,jadot,zemmour,pecresse, evo_demo,
         ndv,mpauv, mprop,ind1824,ind2539,ind65p,starts_with("tx_")) %>%
  left_join(readRDS("donnees/bdv_militaires.RDS") ,by=c("bdv","Libellé.de.la.commune")) %>%
  mutate(militaires = if_else(is.na(militaires),"0","1"))


# Modèles pour chaque candidat
modele_JLM <- lm(formula = melenchon/Inscrits*100 ~ tx_ouv +  tx_retr +  ndv+ tx_cdd+ tx_nsal+ tx_chom+ tx_cad+
                   mprop +ind1824+ind2539 +  tx_immig + militaires+
                   relevel(as.factor(Libellé.de.la.commune),ref="Colmar"),
                 #+  mprop tx_artcom + tx_chom + mpauv + tx_dsup + tx_cad + ndv+ ind65p
                 data = donnes_reg_toutes_villes)
summary(modele_JLM)
modele_MLP <- lm(formula = lepen/Inscrits*100 ~  tx_ouv +  tx_retr +  ndv+ tx_cdd+ tx_nsal+ tx_chom+ tx_cad+
                   mprop +ind1824+ind2539 +  tx_immig + militaires+
                   relevel(as.factor(Libellé.de.la.commune),ref="Colmar"),
                 #+  mprop tx_artcom + tx_chom + mpauv + tx_dsup + tx_cad + ndv+ ind65p
                 data = donnes_reg_toutes_villes)

modele_EM <- lm(formula = macron/Inscrits*100 ~ tx_ouv +  tx_retr +  ndv+ tx_cdd+ tx_nsal+ tx_chom+ tx_cad+
                  mprop +ind1824+ind2539 +  tx_immig + militaires+
                  relevel(as.factor(Libellé.de.la.commune),ref="Colmar"),
                #+  mprop tx_artcom + tx_chom + mpauv + tx_dsup + tx_cad + ndv+ ind65p
                data = donnes_reg_toutes_villes)
modele_YJ <- lm(formula = jadot/Inscrits*100 ~  tx_ouv +  tx_retr +  ndv+ tx_cdd+ tx_nsal+ tx_chom+ tx_cad+
                  mprop +ind1824+ind2539 +  tx_immig + militaires+
                  relevel(as.factor(Libellé.de.la.commune),ref="Colmar"),
                #+  mprop tx_artcom + tx_chom + mpauv + tx_dsup + tx_cad + ndv+ ind65p
                data = donnes_reg_toutes_villes)
modele_EZ <- lm(formula = zemmour/Inscrits*100 ~ tx_ouv +  tx_retr +  ndv+ tx_cdd+ tx_nsal+ tx_chom+ tx_cad+
                  mprop +ind1824+ind2539 +  tx_immig + militaires+
                  relevel(as.factor(Libellé.de.la.commune),ref="Colmar"),
                #+  mprop tx_artcom + tx_chom + mpauv + tx_dsup + tx_cad + ndv+ ind65p
                data = donnes_reg_toutes_villes)
modele_VP <- lm(formula = pecresse/Inscrits*100 ~ tx_ouv +  tx_retr +  ndv+ tx_cdd+ tx_nsal+ tx_chom+ tx_cad+
                  mprop +ind1824+ind2539 +  tx_immig + militaires+
                  relevel(as.factor(Libellé.de.la.commune),ref="Colmar"),
                #+  mprop tx_artcom + tx_chom + mpauv + tx_dsup + tx_cad + ndv+ ind65p
                data = donnes_reg_toutes_villes)

modelisation <- summary(modele_JLM)$coefficients %>% as.data.frame() %>%  mutate(candidat="JLM",
                                                                                 variable=row.names(.) ) %>% 
  bind_rows(summary(modele_EM)$coefficients %>% as.data.frame() %>% mutate(candidat="EM",
                                                                           variable=row.names(.))) %>% 
  bind_rows(summary(modele_YJ)$coefficients %>% as.data.frame() %>% mutate(candidat="YJ",
                                                                           variable=row.names(.))) %>% 
  bind_rows(summary(modele_EZ)$coefficients %>% as.data.frame() %>% mutate(candidat="EZ",
                                                                           variable=row.names(.))) %>% 
  bind_rows(summary(modele_VP)$coefficients %>% as.data.frame() %>% mutate(candidat="VP",
                                                                           variable=row.names(.))) %>% 
  bind_rows(summary(modele_MLP)$coefficients %>% as.data.frame() %>% mutate(candidat="MLP",
                                                                            variable=row.names(.))) %>% 
  mutate(significativite=`Pr(>|t|)`<0.001,
         variable2=str_remove_all(variable,"relevel\\(as.factor\\(Libellé.de.la.commune\\), ref ="),
         variable2=str_remove_all(variable2,"\"Colmar\"\\)")) %>%
  filter(!str_detect(variable,"Gouesnou"))

summary(modele_EZ)
summary(modele_MLP)
summary(modele_JLM)
summary(modele_YJ)
summary(modele_EM)
summary(modele_VP)

modelisation %>% 
  filter(!str_detect(variable,"Libellé") & variable != "(Intercept)") %>% 
  ggplot()+
  geom_point(aes(x = variable,y=Estimate,color=candidat,shape=significativite),size=6,alpha=.9)+
  geom_hline(yintercept = 0)+
  geom_text(label="pour deux bureaux similaires,\nsi le niveau de vie moyen est plus\nélevé de 1000 euros dans l'un,\nMacron y enregistrera 1,1 point\nsupplémentaire",
#            lorsque le niveau de vie moyen\ndu bureau de vote augmente\nde 1 000 euros, le vote Macron\naugmente de 1,1 point",
            x="tx_nsal",y=.55, fontface="italic", hjust=0,color="gray20",size=4 )+
  geom_curve(   x = "tx_chom", y = .75, xend = "ndv", yend = 1.1, curvature = .3, angle = 80,
    arrow = arrow(length = unit(0.03, "npc"))  )+
  scale_x_discrete(name="",
                   labels=c("tx_retr"="% de retraité-e-s",
                            "tx_ouv"="% d'ouvrier-ère-s",
                            "tx_immig"="% d'immigré-e-s",
                            "tx_cad"="% de cadres",
                            "tx_cdd"="% de salarié-e-s en CDD",
                            "tx_nsal"="% de non salarié-e-s",
                            "tx_dsup"="% de diplômé-e-s du supérieur",
                            "tx_cdi"="% de salarié-e-s en CDI",
                             "mpauv"="% de ménages pauvres",
                            "mprop"="% de ménages propriétaires\n de leur logement",
                            "tx_chom"="% de chômeur-se-s",
                            "militaires1"="Bureau de vote avec\nprésence de militaires",
                            "ndv"="Niveau de vie annuel moyen\n(en milliers d'euros)",
                            "ind65p"="% de 65 ans et +",
                            "ind2539"="% des 25-39 ans",
                            "ind1824"="% des 18-24 ans"))+
  scale_color_manual(name="",values = c("EM"="darkorchid4","JLM"="red2","MLP"="peru",
                                        "YJ"="chartreuse4","EZ"="tan4","VP"="blue"),
                     labels = c("EM"="Macron (R²=0,81)",
                                "JLM"="Mélenchon (R²=0,66)",
                                "VP"="Pécresse (R²=0,74)",
                                "MLP"="Le Pen (R²=0,72)",
                                "YJ"="Jadot (R²=0,72)",
                                "EZ"="Zemmour (R²=0,66)"))+
  scale_shape_manual(name="", values = c("TRUE"=15,"FALSE"=13),
                     labels=c("TRUE"="Significativement différent de 0","FALSE"="Non significativement différent de 0"))+
  labs(title="Modélisation du vote urbain au 1er tour de l'élection présidentielle 2022 (1/2) : caractéristiques sociales",
       subtitle = "à l'échelle du bureau de vote, en % des inscrit-e-s, pour les villes d'Angers, Bordeaux, Brest (et Gouesnou), Colmar, Grenoble, Le Havre, Lyon, Mulhouse, Nantes, Nîmes, Orléans,\nPoitiers, Rennes, Roubaix, Strasbourg et Toulouse",
       y="Coefficient estimé",
       caption = "Sources : Open data des villes étudiées, Découpage des bureaux de vote,\nMinistère de l'Intérieur, Résultats du 1er tour de l'élection présidentielle 2022\nInsee, Recensement 2018, Filosofi 2017\nLes contributeur-ice-s d'OpenStreetMap, Gendarmeries et sites militaires\nTraitements et erreurs : @Re_Mi_La")+
  coord_flip()+
  theme_minimal()+
  theme(text=element_text(size = 14),
        plot.subtitle = element_text(face="italic",size=10),
        plot.title.position = "plot",
        legend.position = "top",
        plot.caption = element_text(size=8))+
  guides(shape="none")



modelisation %>% 
  filter(str_detect(variable,"Libellé") & variable != "(Intercept)") %>% 
  ggplot()+
  geom_point(aes(x = variable2, y = Estimate, color = candidat, shape = significativite),
             size = 6,alpha = .9)+
  geom_hline(yintercept = 0)+
  scale_color_manual(name="",values = c("EM"="darkorchid4","JLM"="red2","MLP"="peru",
                                        "YJ"="chartreuse4","EZ"="tan4","VP"="blue"),
                     labels = c("EM"="Macron (R²=0,81)",
                                "JLM"="Mélenchon (R²=0,66)",
                                "VP"="Pécresse (R²=0,74)",
                                "MLP"="Le Pen (R²=0,72)",
                                "YJ"="Jadot (R²=0,72)",
                                "EZ"="Zemmour (R²=0,66)"))+
  scale_shape_manual(name="", values = c("TRUE"=15,"FALSE"=13),
                     labels=c("TRUE"="Significativement différent de 0","FALSE"="Non significativement différent de 0"))+
  labs(title="Modélisation du vote urbain au 1er tour de l'élection présidentielle 2022 (2/2) : effets ville",
       subtitle = "à l'échelle du bureau de vote, en % des inscrit-e-s, référence = Colmar",
       y="Coefficient estimé",
       x="",
       caption = "Sources : Open data des villes étudiées, Découpage des bureaux de vote,\nMinistère de l'Intérieur, Résultats du 1er tour de l'élection présidentielle 2022\nInsee, Recensement 2018, Filosofi 2017\nLes contributeur-ice-s d'OpenStreetMap, Gendarmeries et sites militaires\nTraitements et erreurs : @Re_Mi_La")+
  coord_flip()+
  theme_minimal()+
  theme(text=element_text(size = 14),
        plot.subtitle = element_text(face="italic",size=10),
        plot.title.position = "plot",
        legend.position = "top",
        plot.caption = element_text(size=8))+
  guides(shape="none")

modele_JLM2 <- lm(formula = melenchon/Inscrits*100 ~ relevel(as.factor(Libellé.de.la.commune),ref="Colmar"),
                 #+  mprop tx_artcom + tx_chom + mpauv + tx_dsup + tx_cad + ndv+ ind65p
                 data = donnes_reg_toutes_villes)
modele_MLP2 <- lm(formula = lepen/Inscrits*100 ~ relevel(as.factor(Libellé.de.la.commune),ref="Colmar"),
                 #+  mprop tx_artcom + tx_chom + mpauv + tx_dsup + tx_cad + ndv+ ind65p
                 data = donnes_reg_toutes_villes)
modele_EM2 <- lm(formula = macron/Inscrits*100 ~relevel(as.factor(Libellé.de.la.commune),ref="Colmar"),
                #+  mprop tx_artcom + tx_chom + mpauv + tx_dsup + tx_cad + ndv+ ind65p
                data = donnes_reg_toutes_villes)
modele_YJ2 <- lm(formula = jadot/Inscrits*100 ~ relevel(as.factor(Libellé.de.la.commune),ref="Colmar"),
                #+  mprop tx_artcom + tx_chom + mpauv + tx_dsup + tx_cad + ndv+ ind65p
                data = donnes_reg_toutes_villes)
modele_EZ2 <- lm(formula = zemmour/Inscrits*100 ~ relevel(as.factor(Libellé.de.la.commune),ref="Colmar"),
                #+  mprop tx_artcom + tx_chom + mpauv + tx_dsup + tx_cad + ndv+ ind65p
                data = donnes_reg_toutes_villes)
modele_VP2 <- lm(formula = pecresse/Inscrits*100 ~  relevel(as.factor(Libellé.de.la.commune),ref="Colmar"),
                #+  mprop tx_artcom + tx_chom + mpauv + tx_dsup + tx_cad + ndv+ ind65p
                data = donnes_reg_toutes_villes)

modelisation2 <- summary(modele_JLM2)$coefficients %>% as.data.frame() %>%  mutate(candidat="JLM",
                                                                                 variable=row.names(.) ) %>% 
  bind_rows(summary(modele_EM2)$coefficients %>% as.data.frame() %>% mutate(candidat="EM",
                                                                           variable=row.names(.))) %>% 
  bind_rows(summary(modele_YJ2)$coefficients %>% as.data.frame() %>% mutate(candidat="YJ",
                                                                           variable=row.names(.))) %>% 
  bind_rows(summary(modele_EZ2)$coefficients %>% as.data.frame() %>% mutate(candidat="EZ",
                                                                           variable=row.names(.))) %>% 
  bind_rows(summary(modele_VP2)$coefficients %>% as.data.frame() %>% mutate(candidat="VP",
                                                                           variable=row.names(.))) %>% 
  bind_rows(summary(modele_MLP2)$coefficients %>% as.data.frame() %>% mutate(candidat="MLP",
                                                                            variable=row.names(.))) %>% 
  mutate(significativite=`Pr(>|t|)`<0.001,
         variable2=str_remove_all(variable,"relevel\\(as.factor\\(Libellé.de.la.commune\\), ref ="),
         variable2=str_remove_all(variable2,"\"Colmar\"\\)")) %>%
  filter(!str_detect(variable,"Gouesnou"))

summary(modele_EZ2)
summary(modele_MLP2)
summary(modele_JLM2)
summary(modele_YJ2)
summary(modele_EM2)
summary(modele_VP2)

modelisation2 %>% 
  filter(str_detect(variable,"Libellé") & variable != "(Intercept)") %>% 
  ggplot()+
  geom_point(aes(x = variable2, y = Estimate, color = candidat, shape = significativite),
             size = 6,alpha = .9)+
  geom_hline(yintercept = 0)+
  scale_color_manual(name="",values = c("EM"="darkorchid4","JLM"="red2","MLP"="peru",
                                        "YJ"="chartreuse4","EZ"="tan4","VP"="blue"),
                     labels = c("EM"="Macron (R²=0,23)",
                                "JLM"="Mélenchon (R²=0,22)",
                                "VP"="Pécresse (R²=0,16)",
                                "MLP"="Le Pen (R²=0,43)",
                                "YJ"="Jadot (R²=0,34)",
                                "EZ"="Zemmour (R²=0,19)"))+
   scale_shape_manual(name="", values = c("TRUE"=15,"FALSE"=13),
                      labels=c("TRUE"="Significativement différent de 0","FALSE"="Non significativement différent de 0"))+
  labs(title="Modélisation sans variables socio-économiques",
       subtitle = "à l'échelle du bureau de vote, en % des inscrit-e-s, référence = Colmar",
       y="Coefficient estimé",
       x="",
       caption = "Source : Open data des villes étudiées, Découpage des bureaux de vote,\nMinistère de l'Intérieur, Résultats du 1er tour de l'élection présidentielle 2022\nInsee, Recensement 2018, Filosofi 2017\nTraitements et erreurs : @Re_Mi_La")+
  coord_flip()+
  theme_minimal()+
  theme(text=element_text(size = 14),
        plot.subtitle = element_text(face="italic",size=10),
        plot.title.position = "plot",
        legend.position = "top",
        plot.caption = element_text(size=8))+
  guides(shape="none")



#install.packages("jtools")
# install.packages("ggstance")
# https://cran.r-project.org/web/packages/jtools/vignettes/summ.html#plot_summs_and_plot_coefs
jtools::plot_coefs(modele_JLM, modele_MLP,modele_EM,model.names = c("Mélenchon", "Le Pen", "Macron"), scale = TRUE)
jtools::plot_coefs(modele_JLM,model.names = c("Mélenchon"), scale = TRUE)

coefficients <- as.data.frame(modele_EM$coefficients) %>% mutate(variable=row.names(.) ) %>% 
  bind_cols(as.data.frame(modele_JLM$coefficients)) %>% 
  bind_cols(as.data.frame(modele_MLP$coefficients))


summary(modele_EM)




# Pas à pas
modelisation2 <- 
  summary(lm(formula = melenchon/Inscrits*100 ~ relevel(as.factor(Libellé.de.la.commune),ref="Bordeaux"),
             #+  mprop tx_artcom + tx_chom + mpauv + tx_dsup + tx_cad + ndv+ ind65p
             data = donnes_reg_toutes_villes))$coefficients %>% as.data.frame() %>%  mutate(candidat="Sans",
                                                                                 variable=row.names(.) ) %>% 
  bind_rows(summary(lm(formula = melenchon/Inscrits*100 ~ tx_ouv +  tx_retr +   ndv+ tx_cdd+ tx_nsal+
                         ind1824+ind2539 +  tx_immig + relevel(as.factor(Libellé.de.la.commune),ref="Bordeaux"),
                       #+  mprop tx_artcom + tx_chom + mpauv + tx_dsup + tx_cad + ndv+ ind65p
                       data = donnes_reg_toutes_villes))$coefficients %>% as.data.frame() %>% mutate(candidat="Avec",
                                                                           variable=row.names(.))) %>% 
  mutate(significativite=`Pr(>|t|)`<0.001,
         variable2=str_remove_all(variable,"relevel\\(as.factor\\(Libellé.de.la.commune\\), ref ="),
         variable2=str_remove_all(variable2,"\"Bordeaux\"\\)")) %>%
  filter(!str_detect(variable,"Gouesnou"))



modelisation2 %>% 
  filter(str_detect(variable,"Libellé") & variable != "(Intercept)") %>% 
  ggplot()+
  geom_point(aes(x = variable2,y=Estimate,color=candidat,shape=significativite),size=6,alpha=.8)+
  geom_hline(yintercept = 0)+
  scale_color_manual(name="",values = c("Avec"="red2", "Sans"="coral"))+
  scale_shape_manual(name="", values = c("TRUE"=15,"FALSE"=13),
                     labels=c("TRUE"="Significativement différent de 0","FALSE"="Non significativement différent de 0"))+
  labs(title="Modélisation du vote urbain au 1er tour de l'élection présidentielle 2022 (2/2) : effets ville",
       subtitle = "à l'échelle du bureau de vote, en % des inscrit-e-s, référence = Bordeaux",
       y="Coefficient estimé",
       caption = "Source : Open data des villes étudiées, Découpage des bureaux de vote,\nMinistère de l'Intérieur, Résultats du 1er tour de l'élection présidentielle 2022\nInsee, Recensement 2018, Filosofi 2017\nTraitements et erreurs : @Re_Mi_La")+
  coord_flip()+
  theme_minimal()+
  theme(text=element_text(size = 14),
        plot.subtitle = element_text(face="italic",size=10),
        plot.title.position = "plot",
        legend.position = "top",
        plot.caption = element_text(size=8))+
  guides(shape="none")

donnes_reg_toutes_villes %>%
  filter(Libellé.de.la.commune=="Roubaix") %>%
  summarise(tx_pec=sum(pecresse)/sum(Inscrits)*100,
            tx_zem=sum(zemmour)/sum(Inscrits)*100,
            tx_mel=sum(melenchon)/sum(Inscrits)*100,
            tx_jad=sum(jadot)/sum(Inscrits)*100)

summary(modele_JLM2)

library(lme4)
install.packages("sjPlot")
library(lmerTest)
#> 
#> Attaching package: 'lmerTest'
#> The following object is masked from 'package:lme4':
#> 
#>     lmer
#> The following object is masked from 'package:stats':
#> 
#>     step
library(sjplot)
library(sjmisc)
library(multilevelTools)
xyplot(incidence/size ~ period|herd, cbpp, type=c('g','p','l'),
       layout=c(3,5), index.cond = function(x,y)max(y))

m1 <- lmer( mel~ (1+ ndv|Libellé.de.la.commune),
              data = donnes_reg_toutes_villes %>% mutate(mel=melenchon/Inscrits*100))

fit <- lmer(weight ~ Time * Diet + (1 + Time | Chick), data=ChickWeight, REML=T)
sjmisc::sjp.lmer(fit, y.offset = .4)


summary(gm1)
md <- modelDiagnostics(gm1, ev.perc = .001)

plot(md, ask = FALSE, ncol = 2, nrow = 3)

## linear mixed models - reference values from older code
(fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy))
summary(fm1)# (with its own print method; see class?merMod % ./merMod-class.Rd
str(terms(fm1))
stopifnot(identical(terms(fm1, fixed.only=FALSE),
                    terms(model.frame(fm1))))
attr(terms(fm1, FALSE), "dataClasses") # fixed.only=FALSE needed for dataCl.
## Maximum Likelihood (ML), and "monitor" iterations via 'verbose':
fm1_ML <- update(fm1, REML=FALSE, verbose = 1)
(fm2 <- lmer(Reaction ~ Days + (Days || Subject), sleepstudy))
anova(fm1, fm2)
sm2 <- summary(fm2)
print(fm2, digits=7, ranef.comp="Var") # the print.merMod() method
54 lmerControl
print(sm2, digits=3, corr=FALSE) # the print.summary.merMod() method
(vv <- vcov.merMod(fm2, corr=TRUE))
as(vv, "corMatrix")# extracts the ("hidden") 'correlation' entry in @factors
## Fit sex-specific variances by constructing numeric dummy variables
## for sex and sex:age; in this case the estimated variance differences
## between groups in both intercept and slope are zero ...
data(Orthodont,package="nlme")
Orthodont$nsex <- as.numeric(Orthodont$Sex=="Male")
Orthodont$nsexage <- with(Orthodont, nsex*age)
lmer(distance ~ age + (age|Subject) + (0+nsex|Subject) +
       (0 + nsexage|Subject), data=Orthodont)


modele_VP <- lm(formula = pecresse/Inscrits*100 ~ tx_ouv +  tx_retr +   ndv+ tx_cdd+ tx_nsal+ tx_chom+ tx_cad+
                  mprop +ind1824+ind2539 +  tx_immig +  relevel(as.factor(Libellé.de.la.commune),ref="Colmar"),
                #+  mprop tx_artcom + tx_chom + mpauv + tx_dsup + tx_cad + ndv+ ind65p
                data = donnes_reg_toutes_villes)