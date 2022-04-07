
library(eph)
library(tidyverse)
library(sf)
library(geoAr)
library(datawizard)
library(ggbump)



hog_eph <- get_microdata(year = 2021, trimester = 2, type = "hogar")
ind_eph <- get_microdata(year = 2021, trimester = 2, type = "individual")


ind_sexo <- ind_eph %>% 
  mutate(id_unico = paste(CODUSU, NRO_HOGAR, COMPONENTE, sep = "_")) %>% 
  select(id_unico, CH04) %>% 
  mutate(CH04 = case_when(CH04 == 1 ~ "Varón",
                          CH04 == 2 ~ "Mujer"))

hog_tareas <- hog_eph %>% 
  mutate(id_unico = paste(CODUSU, NRO_HOGAR, VII1_1, sep = "_")) %>% 
  select(AGLOMERADO, id_unico, PONDERA) %>% 
  left_join(ind_sexo, by = "id_unico")

tabulado_tareas_hog <- calculate_tabulates(hog_tareas, "AGLOMERADO", "CH04", 
                                           add.percentage = "row", 
                                           weights = "PONDERA",
                                           affix_sign = F) %>% 
  select(1:2) %>% as_tibble() %>% 
  mutate(Mujer = as.numeric(Mujer)) %>% 
  rename(AGLOMERADO = 1)




### Mapitash

aglos <- sf::st_read('data/aglomerados_eph.json')

mapa_arg <- get_geo("ARGENTINA", level = "provincia")


centroides_aglomerados <-  aglos %>%
  filter(!st_is_empty(geometry)) %>%
  group_by(eph_codagl) %>%
  summarise(AGLOMERADO = as.integer(paste(unique(eph_codagl))),
            nombre_aglomerado = unique(eph_aglome)[1]) %>%
  st_centroid() %>% 
  st_transform('+proj=longlat +datum=WGS84')

centroides_aglomerados <- centroides_aglomerados %>% 
  bind_cols(
    sf::st_coordinates(centroides_aglomerados) %>%
      as_tibble() %>%
      rename(lon=X, lat=Y)) %>%
  as_tibble()



tabla_graf <- centroides_aglomerados %>% 
  mutate(AGLOMERADO = as.factor(AGLOMERADO)) %>%
  left_join(tabulado_tareas_hog) %>% 
  mutate(
    col_y = rank(-Mujer) %>% 
      data_rescale(to=c(-15, -65)),
    sigmoid_end = 22,
    col_x_start = 25,
    col_x_end = Mujer %>%  
      data_rescale(to=c(25, 110), range = c(0, max(Mujer))),
    area_label = paste0(round(Mujer, 1), "%")
  )


ggplot(tabla_graf) +
  geom_sf(data=mapa_arg, size = .3, fill = "transparent", color = "gray40") +
  geom_point(aes(x=lon, y=lat, color=Mujer)) +
  geom_sigmoid(
    aes(x=lon, y=lat, xend=sigmoid_end, yend=col_y, group=nombre_aglomerado, color=Mujer)
  ) +
  geom_text(
    aes(label=nombre_aglomerado, x=sigmoid_end, y=col_y, color=Mujer), 
    hjust=1, size=4, vjust=0, nudge_y = 0.5, alpha=0.8,
  ) +
  geom_segment(
    aes(x=col_x_start, xend=col_x_end, y=col_y, yend=col_y, color=Mujer), 
    size = 4.2, alpha = .9, size = 1, 
    lineend = "round"
  ) +
  geom_text(
    aes(label=area_label, x=col_x_end, y=col_y, color=Mujer), 
    hjust=-0.1, size=4, nudge_x = .3
  ) +
  labs(
    title="Porcentaje de hogares en donde la Mujer realiza las principales tareas del hogar",
    subtitle = "",
    caption = "Elaboración propia en base a la EPH-INDEC"
  ) +
  coord_sf(clip = "off") +
  #scale_fill_viridis_c() +
  dnmye::scale_fill_dnmye(discrete = F) +
  #scale_color_viridis_c() +
  dnmye::scale_color_dnmye(discrete = F) +
  theme_void() +
  theme(plot.margin = margin(.5, 1, .5, .5, "cm"),
        legend.position = "none",
        plot.background = element_rect(fill = "black"),
        plot.caption = element_text(color = "gray40", size = 15),
        plot.title = element_text(color = "gray40", size = 35, family = "Helvetica", face = "bold"),
        plot.subtitle = element_text(color = "gray40", size = 20))

ggsave("salidas/ranking_tareas_del_hogar.png",width = 25, height = 10, dpi = 1000)

