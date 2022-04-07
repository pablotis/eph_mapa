

library(eph)
library(tidyverse)
library(sf)
library(geoAr)
library(datawizard)
library(ggbump)

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
    


eph_2021_t2 <- get_microdata(2021,2)

tasa_des <- eph_2021_t2 %>%
  mutate(AGLOMERADO = as.factor(AGLOMERADO)) %>% 
  group_by(AGLOMERADO) %>% 
  summarise(pob_desocupada = sum(PONDERA[ESTADO == 2]),
            pob_total      = sum(PONDERA[ESTADO %in% c(1,2)]),
            tasa_desocupacion = round(pob_desocupada / pob_total * 100, 2))

tabla_graf <- centroides_aglomerados %>% 
  mutate(AGLOMERADO = as.factor(AGLOMERADO)) %>%
  left_join(select(tasa_des, AGLOMERADO, tasa_desocupacion), 
            by = "AGLOMERADO") %>% 
  mutate(
    col_y = rank(-tasa_desocupacion) %>% 
      data_rescale(to=c(-15, -65)),
    sigmoid_end = 22,
    col_x_start = 25,
    col_x_end = tasa_desocupacion %>%  
      data_rescale(to=c(25, 110), range = c(0, max(tasa_desocupacion))),
    area_label = paste0(round(tasa_desocupacion, 1), "%")
  )


ggplot(tabla_graf) +
  geom_sf(data=mapa_arg, size = .3, fill = "transparent", color = "gray", expand = TRUE) +
  geom_point(aes(x=lon, y=lat, color=tasa_desocupacion)) +
  geom_sigmoid(
    aes(x=lon, y=lat, xend=sigmoid_end, yend=col_y, group=nombre_aglomerado, color=tasa_desocupacion)
  ) +
  geom_text(
    aes(label=nombre_aglomerado, x=sigmoid_end, y=col_y, color=tasa_desocupacion), 
    hjust=1, size=4, vjust=0, nudge_y = 0.5, alpha=0.8,
  ) +
  geom_segment(
    aes(x=col_x_start, xend=col_x_end, y=col_y, yend=col_y, color=tasa_desocupacion), 
    size = 4.2, alpha = .9, size = 1, 
    lineend = "round"
  ) +
  geom_text(
    aes(label=area_label, x=col_x_end, y=col_y, color=tasa_desocupacion), 
    hjust=-0.1, size=4, nudge_x = .3
  ) +
  labs(
    title="Tasa de desocupación por Aglomerados urbanos",
    subtitle = "",
    caption = "Elaboración propia en base a la EPH-INDEC"
  ) +
  coord_sf(clip = "off") +
  #scale_fill_viridis_c() +
  dnmye::scale_fill_dnmye(discrete = F) +
  #scale_color_viridis_c() +
  dnmye::scale_color_dnmye(discrete = F) +
  theme(plot.margin = margin(.5, 1, .5, .5, "cm"),
        legend.position = "none",
        plot.background = element_rect(fill = "black"),
        plot.caption = element_text(color = "gray40"),
        plot.title = element_text(color = "gray40", size = 16, family = "Helvetica", face = "bold"),
        plot.subtitle = element_text(color = "gray40", size = 8))

ggsave("salidas/ranking_tasa_desocupacion.png",width = 25, height = 10, dpi = 300)

