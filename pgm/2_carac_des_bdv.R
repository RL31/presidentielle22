library(tidyverse)
library(patchwork)

# Description selon le taux de pauvrete
pauvres <- st_read("donnees/elec-2021-decoupage-bdv.shp") %>% 
  st_transform(2154) %>% 
  left_join(filosofi_par_bdv,by="uniq_bdv") %>% 
  mutate(tx_pauvrete=Men_pauv/Men*100,
         tr_pauvrete=cut(tx_pauvrete,breaks = c(10,20,30))) %>% 
  ggplot()+
  geom_sf(aes(fill=tx_pauvrete),color="white")+
  scale_fill_stepsn(name="Taux de pauvreté (%)",colors = c("white","firebrick2"))+
  # labs(caption = "Source : Mairie de Toulouse, Découpage des bureaux de vote\nInsee, Filosofi 2017\n
  #      Traitements et erreurs : @Re_Mi_La")+
  theme_void()+
  theme(legend.position = "top")

# Description selon les plus de 64 ans
vieux <- st_read("donnees/elec-2021-decoupage-bdv.shp") %>% 
  st_transform(2154) %>% 
  left_join(filosofi_par_bdv,by="uniq_bdv") %>% 
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
enfants <- st_read("donnees/elec-2021-decoupage-bdv.shp") %>% 
  st_transform(2154) %>% 
  left_join(filosofi_par_bdv,by="uniq_bdv") %>% 
  mutate(tx_m11=(Ind_0_3+Ind_4_5+Ind_6_10)/Ind*100,
         tr_m11=cut(tx_m11,breaks = c(10,20,30))) %>% 
  ggplot()+
  geom_sf(aes(fill=tx_m11),color="white")+
  scale_fill_stepsn(name="Part des moins\nde 11 ans (%)",colors = c("white","skyblue3"))+
  # labs(caption = "Source : Mairie de Toulouse, Découpage des bureaux de vote\nInsee, Filosofi 2017\n
  #      Traitements et erreurs : @Re_Mi_La")+
  theme_void()+
  theme(legend.position = "top")

pauvres+enfants+vieux
