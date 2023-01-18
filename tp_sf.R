
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

# OPTION 2: Si vous n'avez pas uploadé les données ####
metro_sf <- aws.s3::s3read_using(
  st_read,
  dsn = "commune_francemetro_2021.shp", 
  options = "ENCODING=WINDOWS-1252",
  bucket = "julienjamme",
  object = "public"
)

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
# 6 - On a conservé la géométrie ####

class(communes_bretagne)

# 7 - plot ####

plot(communes_bretagne, lwd = 0.3)
plot(communes_bretagne %>% st_geometry(), lwd = 0.3)

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
all.equal(communes_bretagne$surf, round(as.double(communes_bretagne$surf2),2))

# 12 - départements bretons ####

depts_bretagne <- communes_bretagne %>% 
  group_by(dep) %>% 
  summarise(
    surf = sum(surf)
  )
str(depts_bretagne) # tjrs objet sf
plot(depts_bretagne)

# 13 - par union des géométries ####

depts_bretagne_geo <- communes_bretagne %>% 
  group_by(dep) %>% 
  summarise( 
    geometry = st_union(geometry)
  )
plot(depts_bretagne_geo)


# 14 - centroides ####

centr_depts_bretagne <- depts_bretagne_geo %>% 
  st_centroid()
str(centr_depts_bretagne)
st_crs(centr_depts_bretagne)

plot(depts_bretagne_geo %>% st_geometry())
plot(centr_depts_bretagne %>% st_geometry(), add = TRUE)

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

# 15 - intersections ####

communes_centr_depts <- st_intersects(
  centr_depts_bretagne,
  communes_bretagne
)
str(communes_centr_depts)
# Liste de 4 éléments 

communes_centr_depts_sf <- purrr::map_dfr(communes_centr_depts, function(i) communes_bretagne %>% slice(i))

# 16 - st_intersection et st_within ####
communes_centr_depts_sf <- st_intersection(
  centr_depts_bretagne,
  communes_bretagne
)
str(communes_centr_depts_sf)
#fourni directement un objet sf

communes_centr_depts <- st_within(
  centr_depts_bretagne,
  communes_bretagne
)
str(communes_centr_depts)
# là encore une liste

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
names(distances) <- c(22,29,35,53)

# 18 - communes à moins de 20 ####

st_buffer(
  communes_bretagne, 
)

