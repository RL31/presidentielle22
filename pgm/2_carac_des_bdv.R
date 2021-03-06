library(tidyverse)
library(patchwork)

# Description selon le taux de pauvrete
pauvres <- st_read("donnees/elections-2017-decoupage-des-bureaux-de-vote.shp") %>% 
  st_transform(2154) %>% 
  left_join(filosofi_par_bdv17,by="bv2017") %>% #uniq_bdv" 
  mutate(tx_pauvrete=Men_pauv/Men*100,
         tr_pauvrete=cut(tx_pauvrete,breaks = c(10,20,30))) %>% 
  ggplot()+
  geom_sf(aes(fill=tx_pauvrete),color="white")+
  scale_fill_stepsn(name="Taux de pauvreté (%)",colors = c("white","firebrick2"))+
  theme_void()+
  theme(legend.position = "top")

# Description selon la part de logements sociaux
logsoc <- st_read("donnees/elections-2017-decoupage-des-bureaux-de-vote.shp") %>% 
  st_transform(2154) %>% 
  left_join(filosofi_par_bdv17,by="bv2017") %>% #uniq_bdv" 
  mutate(tx_logsoc=Log_soc/(Log_av45+Log_45_70+Log_70_90+Log_ap90+Log_inc)*100) %>% 
  ggplot()+
  geom_sf(aes(fill=tx_logsoc),color="white")+
  scale_fill_stepsn(name="Part de\nlogements sociaux (%)",colors = c("white","hotpink4"))+
  theme_void()+
  theme(legend.position = "top")

# Description selon la part de ménages propriétaires
proprio <- st_read("donnees/elections-2017-decoupage-des-bureaux-de-vote.shp") %>% 
  st_transform(2154) %>% 
  left_join(filosofi_par_bdv17,by="bv2017") %>% #uniq_bdv" 
  mutate(tx_proprio=Men_prop/Men*100) %>% 
  ggplot()+
  geom_sf(aes(fill=tx_proprio),color="white")+
  scale_fill_stepsn(name="Part de\npropriétaires (%)",colors = c("white","seagreen"))+
  theme_void()+
  theme(legend.position = "top")

ndv <- st_read("donnees/elections-2017-decoupage-des-bureaux-de-vote.shp") %>% 
  st_transform(2154) %>% 
  left_join(filosofi_par_bdv17,by="bv2017") %>% #uniq_bdv" 
  mutate(ndv=Ind_snv/Ind/1000) %>% 
  ggplot()+
  geom_sf(aes(fill=ndv),color="white")+
  scale_fill_stepsn(name="Niveau de vie moyen\n(milliers d'€)",colors = c("white","slateblue4"))+
  theme_void()+
  theme(legend.position = "top")

# Description selon les plus de 64 ans
vieux <- st_read("donnees/elections-2017-decoupage-des-bureaux-de-vote.shp") %>% 
  st_transform(2154) %>% 
  left_join(filosofi_par_bdv17,by="bv2017") %>% 
  mutate(tx_64p=(Ind_65_79+Ind_80p)/Ind*100,
         tr_64p=cut(tx_64p,breaks = c(10,20,30))) %>% 
  ggplot()+
  geom_sf(aes(fill=tx_64p),color="white")+
  scale_fill_stepsn(name="Part des plus\nde 64 ans (%)",colors = c("white","darkolivegreen3"))+
  labs(caption = "Source : Insee, Zonage à façon sur les données carroyées à 200m de Filosofi 2017\nMairie de Toulouse, Découpage des bureaux de vote\nTraitements et erreurs : @Re_Mi_La")+
  theme_void()+
  theme(legend.position = "top",
        plot.caption = element_text(size=8))


# Description selon les enfants
enfants <- st_read("donnees/elections-2017-decoupage-des-bureaux-de-vote.shp") %>% 
  st_transform(2154) %>% 
  left_join(filosofi_par_bdv17,by="bv2017") %>% 
  mutate(tx_m11=(Ind_0_3+Ind_4_5+Ind_6_10)/Ind*100,
         tr_m11=cut(tx_m11,breaks = c(10,20,30))) %>% 
  ggplot()+
  geom_sf(aes(fill=tx_m11),color="white")+
  scale_fill_stepsn(name="Part des moins\nde 11 ans (%)",colors = c("white","skyblue3"))+
  theme_void()+
  theme(legend.position = "top")

(ndv+pauvres+proprio)/(logsoc+enfants+vieux)
