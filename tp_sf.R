
# TP1 

# 0 - les packages nécessaires ####

library(sf)
library(dplyr)

# 1 - Import du fond communal ####

# OPTION 1: si vous avez uploadé les données dans votre projet ####
metro_sf <- st_read(
  dsn = "fonds/commune_francemetro_2021.shp", 
  options = "ENCODING=WINDOWS-1252"
)
str(metro_sf)
summary(metro_sf)
# # OPTION 2: Par téléchargement ####
# # Téléchargement depuis un espace de stockage S3 public
# temp_file <- download.file("")
# metro_sf <- aws.s3::s3read_using(
#   FUN = sf::st_read,
#   object = "/public/commune_francemetro_2021.gpkg",
#   bucket = "julienjamme",
#   opts = list("region" = "")
# )
# str(metro_sf)



# 2 - Résumé stat ####

str(metro_sf)
summary(metro_sf)

# 3 - View ####

head(metro_sf, 10)

metro_sf %>% slice(1:10) %>% View()

# BONUS: représenter une première carte ####

plot(metro_sf %>% st_geometry())

# 4 - système de projection ####

st_crs(metro_sf) 

# 5 - Communes de Bretagne ####

communes_bretagne <- metro_sf %>% 
  # récupérer le code géo:
  # https://www.insee.fr/fr/statistiques/zones/1405599?debut=0&q=territoire
  filter(reg == 53) %>% 
  select(code, libelle, epc, dep, surf)

str(communes_bretagne) 

communes_bretagne %>% st_drop_geometry() %>% str()

# 6 - On a conservé la géométrie ####

class(communes_bretagne)

# 7 - plot ####

plot(communes_bretagne, lwd = 0.3)
plot(communes_bretagne %>% st_geometry(), lwd = 0.3)
ggplot() +
  geom_sf(data = communes_bretagne, fill = "steelblue", col = "grey45") +
  theme_void()
  

# 9 - surfaces ####

communes_bretagne <- communes_bretagne %>% 
  mutate(surf2 = st_area(geometry))

str(communes_bretagne)
summary(communes_bretagne$surf2) # en m2
summary(communes_bretagne$surf) # en km2

# 10 - conversion ####

communes_bretagne <- communes_bretagne %>% 
  mutate(surf2 = units::set_units(surf2, "km^2")) # ou bien transformer en double et diviser surf2 par 1e6
str(communes_bretagne)

# 11 - égalité ####
all.equal(
  communes_bretagne$surf, 
  round(as.double(communes_bretagne$surf2),2)
)

# 12 - départements bretons ####

depts_bretagne <- communes_bretagne %>% 
  group_by(dep) %>% 
  summarise(
    surf = sum(surf)
  )
str(depts_bretagne) # tjrs objet sf
plot(depts_bretagne %>% st_geometry())

# 13 - par union des géométries ####

communes_bretagne %>% st_union() %>% st_geometry() %>% plot()

depts_bretagne_geo <- communes_bretagne %>% 
  group_by(dep) %>% 
  summarise( 
    geometry = st_union(geometry)
  )
plot(depts_bretagne_geo)
str(depts_bretagne_geo)
# on a gardé seulement dep et geometry

# 14 - centroides ####

centr_depts_bretagne <- depts_bretagne_geo %>% 
  st_centroid()
str(centr_depts_bretagne)
st_crs(centr_depts_bretagne)

plot(depts_bretagne_geo %>% st_geometry())
plot(centr_depts_bretagne %>% st_geometry(), add = TRUE)

ggplot() +
  geom_sf(data = depts_bretagne_geo) +
  geom_sf(data = centr_depts_bretagne)+
  theme_void()

lib_depts <- data.frame(
  code = as.character(c(22,29,35,56)), 
  lib = c("Cotes-d'Armor", "Finistère", "Ille-et-Vilaine", "Morbihan")
)
centr_depts_bretagne <- centr_depts_bretagne %>% 
  left_join(
    lib_depts,
    by = c("dep" = "code")
  )

coords_centr <- st_coordinates(centr_depts_bretagne) %>% 
  bind_cols(
    centr_depts_bretagne %>%
      select(dep, lib) %>% 
      st_drop_geometry()
  )
str(coords_centr)

plot(depts_bretagne_geo %>% st_geometry())
plot(centr_depts_bretagne %>% st_geometry(), add = TRUE)
text(coords_centr, labels = coords_centr$lib, adj = c(0.5,-0.2))

ggplot() +
  geom_sf(data = depts_bretagne_geo) +
  geom_sf(data = centr_depts_bretagne)+
  geom_sf_text(
    data = centr_depts_bretagne, 
    aes(label = lib), size = 4
  ) +
  theme_void()

# 15 - intersections ####

communes_centr_depts <- st_intersects(
  centr_depts_bretagne,
  communes_bretagne
)
# st_intersects(
#   communes_bretagne,centr_depts_bretagne
# )
str(communes_centr_depts)
# Liste de 4 éléments 
# Pour chaque centre (numéroté de 1 à 4) est indiqué l'index de la commune correspondante

# communes_centr_depts_sf <- purrr::map_dfr(communes_centr_depts, function(i) communes_bretagne %>% slice(i))
# ou beaucoup plus directement
communes_poly_centr_depts_sf <- 
  communes_bretagne[unlist(communes_centr_depts),]
str(communes_poly_centr_depts_sf)
plot(communes_poly_centr_depts_sf %>% st_geometry())
plot(centr_depts_bretagne %>% st_geometry(), add = TRUE)

# 16 - st_intersection et st_within ####
communes_centr_depts_sf <- st_intersection(
  centr_depts_bretagne,
  communes_bretagne
)
str(communes_centr_depts_sf)
#fourni directement un objet sf
# ATTENTION ici on a un fond de centroides!

communes_centr_depts <- st_within(
  centr_depts_bretagne,
  communes_bretagne
)
str(communes_centr_depts)
# là encore une liste

plot(depts_bretagne_geo %>% st_geometry())
plot(communes_poly_centr_depts_sf %>% st_geometry(), col = "red", add = TRUE)
plot(centr_depts_bretagne %>% st_geometry(), add = TRUE)
text(coords_centr, labels = coords_centr$lib, adj = c(0.5,-0.2))
# expliquer le résultat visuel !

# 17 - distances ####

chefs_lieux <- communes_bretagne %>% 
  filter(libelle %in% c("Rennes","Saint-Brieuc", "Quimper", "Vannes")) %>% 
  st_centroid()

distances <- st_distance(
  chefs_lieux %>% arrange(code), 
  communes_centr_depts_sf %>% arrange(code), # arrange pour s'assurer qu'on a me bon prdre
  by_element = TRUE # pour avoir la distance uuniquement entre les éléments de même position
  # distance entre x1 et y1 puis entre x2 et y2 etc. 
  # avec FALSE, on récupère une matrice
)
names(distances) <- c(22,29,35,56)


# 18 - communes à moins de 20 ####
# a buffer 
buff_centr_sf <- st_buffer(communes_centr_depts_sf, 20000)
buff_centr_sf %>%  str()
buff_centr_sf %>% st_geometry() %>% plot()
plot(communes_centr_depts_sf %>% st_geometry(), add = TRUE)

#b
# Attention: Deux résultats possibles
# Intersection
# Ne retenir que la partie des communes intersectantes qui est dans le buffer
com_buff_sf <- st_intersection(
  communes_bretagne,
  buff_centr_sf
)
str(com_buff_sf)
plot(com_buff_sf %>% st_geometry())

# OU st_intersects
# Permet de récupérer l'intégralité des polygones des communes intersectantes

com_buff2_index <- st_intersects(buff_centr_sf, communes_bretagne)
com_buff2_sf <- communes_bretagne[unlist(com_buff2_index),]

library(ggplot2)

ggplot() +
  geom_sf(data = com_buff2_sf, fill = NA, colour = "grey75") +
  geom_sf(data = com_buff_sf, fill = "steelblue") +
  theme_void()

com_buff2_sf %>% st_drop_geometry() %>%  count(dep)
com_buff_sf %>% st_drop_geometry() %>%  count(dep)
# même résultat en termes de nb de communes (attendu)


# 19 - st_transform -------------------------------------------------------

communes_bretagne_wgs84 <- communes_bretagne %>% 
  st_transform(crs = 4326) %>% 
  mutate(surf3 = st_area(geometry))
st_crs(communes_bretagne_wgs84)

par(mfcol = c(1,2))
plot(communes_bretagne_wgs84 %>% st_geometry(), col = "steelblue")
plot(communes_bretagne %>% st_geometry(), col = "steelblue")
dev.off()

ggplot() +
  geom_sf(data = communes_bretagne, fill = NA) +
  theme_void()

communes_bretagne_wgs84 %>% 
  mutate(surf3 = units::set_units(surf3, "km^2")) %>%
  select(starts_with("surf")) %>% 
  summary()

        