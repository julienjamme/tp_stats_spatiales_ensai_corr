library(dplyr)
library(sf)
library(mapsf)
library(classInt)
library(leaflet)
library(ggplot2)



v <- 1:10
ifelse(v %% 2 == 0, v*2, v)





# Exercice 1 --------------------------------------------------------------

# 1 
pop19 <- readxl::read_xlsx("data/Pop_legales_2019.xlsx") %>% 
  # arrondissements de Paris
  mutate(
    COM = ifelse(substr(COM,1,2) == "75", "75056", COM)
  ) %>% 
  group_by(COM) %>% 
  summarise(PMUN19 = sum(PMUN19), .groups = 'drop')
str(pop19)

metro_sf <- st_read(
  "fonds/France_metro/commune_francemetro_2021.shp", 
  options = "ENCODING=WINDOWS-1252"
) %>% 
  left_join(
    pop19,
    by = c('code' = 'COM')
  ) %>% 
  mutate(
    DENSITE = PMUN19/surf
  )
str(metro_sf)

# 2 
summary(metro_sf$DENSITE)
quantile(metro_sf$DENSITE)
quantile(metro_sf$DENSITE, probs = seq(0,1,0.2))

hist(metro_sf$DENSITE, breaks = seq(0,30000,100))

# 3
plot(metro_sf["DENSITE"], border = FALSE)

# 4
plot(metro_sf["DENSITE"], border = FALSE, breaks = "quantile")
plot(metro_sf["DENSITE"], border = FALSE, breaks = "jenks")
plot(metro_sf["DENSITE"], border = FALSE, breaks = "pretty")

# 5

# a 
decoupage_quantile <- classIntervals(
  metro_sf$DENSITE,
  style = "quantile",
  n = 5
)
str(decoupage_quantile)

# l'objet est composé de deux éléments
# var = variable originale
# brks = les bornes des intervalles demandées

decoupage_quantile$brks
# => 1er intervalle: 0 à 15.35

# b 

pal1 <- RColorBrewer::brewer.pal(n=5, name = "YlOrRd")

plot(decoupage_quantile, pal = pal1, main = "quantile")
table(
  cut(
    metro_sf$DENSITE,
    breaks = decoupage_quantile$brks,
    include.lowest = TRUE, 
    right = FALSE
  )
)

# c
decoupage_jenks <- classIntervals(
  metro_sf$DENSITE,
  style = "jenks",
  n = 5
)
decoupage_jenks$brks

plot(decoupage_jenks, pal = pal1, main = "jenks")
table(
  cut(
    metro_sf$DENSITE,
    breaks = decoupage_jenks$brks,
    include.lowest = TRUE, 
    right = FALSE
  )
)

decoupage_pretty <- classIntervals(
  metro_sf$DENSITE,
  style = "pretty",
  n = 5
)
decoupage_pretty$brks
plot(decoupage_pretty, pal = pal1, main = "pretty")
table(
  cut(
    metro_sf$DENSITE,
    breaks = decoupage_pretty$brks,
    include.lowest = TRUE, 
    right = FALSE
  )
)

# d

metro_sf <- metro_sf %>% 
  mutate(
    DENSITE_cat = cut(
      DENSITE,
      breaks = c(0,40,162,1000,8000,27310),
      include.lowest = TRUE,
      right = FALSE,
      ordered_result = TRUE
    )
  )

table(metro_sf$DENSITE_cat, useNA = "always")
metro_sf %>% 
  ggplot() +
  geom_bar(aes(x=DENSITE_cat))


plot(metro_sf["DENSITE_cat"], border = FALSE, pal = pal1)


# Exercice 2 --------------------------------------------------------------

dep_sf <- st_read(
  "fonds/France_metro/dep_francemetro_2021.shp",
  options = "ENCODING=WINDOWS-1252"
)
str(dep_sf)

tx_pauvrete <- readxl::read_xlsx("data/Taux_pauvrete_2018.xlsx")
str(tx_pauvrete)

mer <- st_read("fonds/merf_2021/merf_2021.shp")

# Jointure pour rajouter le taux de pauvreté à notre fond départemental
dep_sf <- dep_sf %>% 
  left_join(
    tx_pauvrete %>% select(-Dept),
    by=c("code"="Code")
  )
str(dep_sf)
summary(dep_sf$Tx_pauvrete)
boxplot(dep_sf$Tx_pauvrete)

# Palette
pal2 <- rev(mf_get_pal(4,"Mint"))

# Methode de Fisher
mf_map(
  x = dep_sf, 
  var = "Tx_pauvrete", 
  type = "choro",
  nbreaks = 4,
  breaks= "fisher"
)

# Methode des classes de même amplitude
mf_map(
  x = dep_sf, 
  var = "Tx_pauvrete", 
  type = "choro",
  nbreaks = 4,
  breaks= "equal"
)

# Methode des quantiles
mf_map(
  x = dep_sf, 
  var = "Tx_pauvrete", 
  type = "choro",
  nbreaks = 4,
  breaks= "quantile"
)

# 2

dep_idf <- dep_sf  %>% 
  filter(code %in% c("75","92","93","94"))

pdf(file = "macarte.pdf", width = 9, height = 11)
mf_map(
  x = dep_sf, 
  var = "Tx_pauvrete", 
  type = "choro",
  breaks= c(0,13,17,25,max(dep_sf$Tx_pauvrete)),
  # pal = pal2,
  leg_pos = NA
)
#Ouverture d'un encadré
mf_inset_on(
  x = dep_sf, 
  pos = "topright", 
  cex = .2
)
mf_init(dep_idf)
mf_map(
  dep_idf, 
  var = "Tx_pauvrete", 
  type = "choro",
  breaks= c(0,13,17,25,max(dep_sf$Tx_pauvrete)),
  leg_pos = NA,
  add = TRUE
)
# Labels
mf_label(
  dep_idf,
  var = "code", 
  col = "black"
)
# Fermetur encadré
mf_inset_off()
# Si l'on veut rajouter la mer
mf_map(mer, col = "steelblue", add=TRUE)
# Légende
mf_legend(
  type="choro",
  title = "Taux de pauvreté",
  val=c("","Moins de 13","De 13 à moins de 17","De 17 à moins de 25","25 ou plus"),
  pal="Mint",
  pos = "left"
)
# Dans l'idéal on ajoute les pays étrangers
mf_layout(
  title = "Taux de pauvreté par département en 2018",
  credits = "Source : Insee - © IGN - Insee - 2021"
)
dev.off()

# 3
st_write(dep_sf,"dept_tx_pauvrete_2018.gpkg")


# Exercice 3 --------------------------------------------------------------


# Import du fond regional
region <- st_read("fonds/reg_francemetro_2021.shp")
# Import des donnees de population
pop_reg<-readxl::read_xlsx("data/pop_region_2019.xlsx")
# Import du fond de la mer (optionnel)
mer<-st_read("fonds/Merf_region.shp")
# On fait maintenant la jointure avec notre fond
region_pop<-region %>% 
  left_join(pop_reg, by=c("code"="reg")) %>% 
  mutate(densite=pop/surf)
# Creation de la carte
# Représentation du fond de la carte
mf_map(region_pop)
mf_map(mer, add=TRUE, border = "steelblue", col = "steelblue")
# Superposition des ronds proportionnels
mf_map(
  region_pop,
  var = c("pop", "densite"),
  nbreaks = 3,
  breaks = "fisher",
  leg_val_rnd = c(-2,0),
  leg_title = c("Population", "Nombre d'habitants au km²"),
  type = "prop_choro"
)
mf_layout(
  title = "Densité et population des régions françaises en 2018",
  credits = "Source : Insee"
)


# Exercice 4 --------------------------------------------------------------

# 1
# a
com_sf <- st_read(
  "fonds/France_metro/commune_francemetro_2021.shp",
  options = "ENCODING=WINDOWS-1252"
)
str(com_sf)


# b

#Utiliser l'argument n_max=1000 dans un premier temps pour vérifier le bon chargement des données puis le retirer
bpe_loisir_df <- read.csv2(
  "data/bpe20_sport_loisir_xy.csv",
)
str(bpe_loisir_df)
head(bpe_loisir_df)

#2

map <- leaflet() %>% 
  setView(lng = 2.6, lat = 47.2, zoom = 5) %>%
  addTiles()
map

# 3

# a

bowlings_df <- bpe_loisir_df %>%
  filter(
    TYPEQU == "F119" & 
      REG > 10 & 
      !(is.na(LAMBERT_X)) & 
      !(is.na(LAMBERT_Y))
  )


# b

bowlings_sf <- bowlings_df %>%
  st_as_sf(
    coords = c("LAMBERT_X", "LAMBERT_Y"), 
    crs = 2154
  ) %>%
  st_transform(crs = 4326)
str(bowlings_sf)
st_crs(bowlings_sf)

# c

map %>%
  addMarkers(
    data = bowlings_sf
  )

# d

map %>% 
  addMarkers(
    data = bowlings_sf,
    popup = ~DEPCOM
  )

# 4

# a

# Le Rhône 45.7519,4.8038
map2 <- leaflet() %>%
  setView(lng=4.8,lat=45.8,zoom=8) %>%
  addTiles()
map2

# b

popcomd40_df <- read.csv(
  "data/base-cc-evol-struct-pop-2018_echantillon.csv"
) %>%
  filter(substr(CODGEO,1,2) == "69") %>% 
  # ou
  # filter(stringr::str_detect(CODGEO, "^40")) %>%
  select(CODGEO, P18_POP, P18_POP0014) %>%
  mutate(PART18_POP0014 = P18_POP0014/P18_POP*100)


# c

popcomd40_sf <- com_sf %>%
  right_join(
    popcomd40_df, by = c("code" = "CODGEO")
  ) %>%
  st_transform(crs = 4326)

# d

map2 %>%
  addPolygons(
    data = popcomd40_sf,
    weight = 0.5, #width of the borders
    color = "purple",
    fill = NA
  )

# e

pal <- colorQuantile(
  "Blues", 
  domain = popcomd40_sf$PART18_POP0014, 
  n = 5
)

# f

map_choro <- map2 %>%
  addPolygons(
    data = popcomd40_sf,
    weight = 0.5, #width of the borders
    color = "purple",
    fillOpacity = 0.5,
    fillColor = ~pal(PART18_POP0014)
  )
map_choro


# g

map_choro_leg <- map_choro %>%
  addLegend(
    pal = pal,
    values = popcomd40_sf$PART18_POP0014
  )
map_choro_leg

# h

popcomd40_sf$contenu_popup <- paste0(
  "<b>",popcomd40_sf$libelle,"</b>",
  "<br>",
  "Population: ",
  format(popcomd40_sf$P18_POP,big.mark = " "),
  "<br>",
  "Part moins de 15 ans: ",
  round(popcomd40_sf$PART18_POP0014,1),
  "%"
)

map_choro_popup <- map2 %>%
  addPolygons(
    data = popcomd40_sf,
    weight = 0.5, #width of the borders
    color = "purple",
    fillOpacity = 0.5,
    fillColor = ~pal(PART18_POP0014),
    popup = ~contenu_popup
  ) %>%
  addLegend(
    pal = pal,
    values = popcomd40_sf$PART18_POP0014
  )
map_choro_popup

# i

natation_d40 <- bpe_loisir_df %>%
  filter(TYPEQU == "F101" & DEP == "69" & !(is.na(LAMBERT_X)) & !(is.na(LAMBERT_Y)))

natation_sf <- natation_d40  %>%
  st_as_sf(coords = c("LAMBERT_X", "LAMBERT_Y"), crs = 2154) %>%
  st_transform(crs = 4326)
str(natation_sf)
st_crs(natation_sf)

map_choro_popup %>%
  addMarkers(
    data = natation_sf,
    label = ~as.character(DEPCOM)
  )

# j

leaflet() %>%
  setView(lng=4.8,lat=45.8,zoom=9) %>%
  addTiles() %>%
  addPolygons(
    data = popcomd40_sf,
    weight = 0.5, #width of the borders
    color = "purple",
    fill = NA,
    group = "limites communales"
  ) %>%
  addPolygons(
    data = popcomd40_sf,
    weight = 0.5, #width of the borders
    color = NA,
    fillOpacity = 0.5,
    fillColor = ~pal(PART18_POP0014),
    popup = ~contenu_popup,
    group = "Analyse thématique"
  ) %>%
  addLegend(
    pal = pal,
    values = popcomd40_sf$PART18_POP0014
  ) %>%
  addMarkers(
    data = natation_sf,
    label = ~as.character(DEPCOM),
    group = "Bassins de natation"
  ) %>%
  addLayersControl(
    overlayGroups = c("limites communales", "Analyse thématique", "Bassins de natation")
  )


# Avec Mapview
library(mapview)

qt_part <- quantile(popcomd40_sf$PART18_POP0014, probs = seq(0,1,.2))

qt_part_arr <- c(
  floor(qt_part[1]*10)/10,
  round(qt_part[2:5], 1),
  ceiling(qt_part[6]*10)/10
)


mapview(
  popcomd40_sf, 
  z = c("PART18_POP0014"),
  at = qt_part_arr,
  alpha.regions = 0.35,
  layer.name = "Moins de 14 ans (en %)",
  #Info affichée quand on survol le polygône => ici = libellé de la commune
  label = "libelle",
  # Infos affichées quand on clique sur un polygône
  popup = leafpop::popupTable(popcomd40_sf, z = c("code","libelle","P18_POP", "PART18_POP0014"))
) +
  mapview(
    # sf composé de points 
    natation_sf %>%
      # on en profite pour transformer la variable COUVERT en facteur
      mutate(
        COUVERT = factor(
          COUVERT, 
          levels = 0:1, 
          labels = c("Non couverte","Couverte")
        )
      ), 
    z = "COUVERT",
    # les couleurs des points selon la catégorie de la variable COUVERT
    col.regions = c("steelblue", "coral"),
    # transparences
    alpha = 0.8,
    alpha.regions = 0.7,
    # Nom de la couche
    layer.name = "Piscines",
    # Popup: affichage quand on clique sur un point
    popup = leafpop::popupTable(natation_sf, z = "NB_AIREJEU")
  )



