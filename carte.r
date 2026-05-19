library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)
library(jsonlite)

Sys.setenv(VROOM_CONNECTION_SIZE = 50000000)

corresp_geo <- fromJSON(
  "https://geo.api.gouv.fr/communes?fields=code,codeRegion&format=json"
) |>
  as.data.frame() |>
  dplyr::select(code, codeRegion)

regions_sf <- st_read(
  "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/regions-version-simplifiee.geojson",
  quiet = TRUE
) |>
  st_simplify(preserveTopology = TRUE, dTolerance = 0.05)

# Pondération des blocs sur un axe gauche (-) / droite (+)
poids_bloc <- c(
  Extrême_Gauche = -2,
  Gauche         = -1,
  Centre         =  0,
  Droite         =  1,
  Extrême_Droite =  2
)

df_regions_pres <- df_clean_pres |>
  mutate(code = sub("-.*", "", id_bureau)) |>
  inner_join(corresp_geo, by = "code") |>
  group_by(codeRegion) |>
  summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") |>
  # Score = moyenne pondérée des voix par position sur l'axe gauche-droite
  mutate(
    total_voix = Extrême_Gauche + Gauche + Centre + Droite + Extrême_Droite,
    score_lr = (
      Extrême_Gauche * poids_bloc["Extrême_Gauche"] +
      Gauche         * poids_bloc["Gauche"]         +
      Centre         * poids_bloc["Centre"]         +
      Droite         * poids_bloc["Droite"]         +
      Extrême_Droite * poids_bloc["Extrême_Droite"]
    ) / total_voix
  ) |>
  dplyr::select(codeRegion, score_lr)

map_data <- regions_sf |>
  inner_join(df_regions_pres, by = c("code" = "codeRegion"))

ggplot(map_data) +
  geom_sf(aes(fill = score_lr), color = "white", linewidth = 0.2) +
  scale_fill_gradient2(
    low      = "#CC2443",   # rouge = gauche
    mid      = "#F5F5F5",   # blanc = centre
    high     = "#0D378A",   # bleu = droite
    midpoint = 0,
    limits   = c(-2, 2),
    name     = "Score\ngauche ←→ droite"
  ) +
  theme_void() +
  labs(title = "Orientation politique par région (Présidentielle 2022 - 1er tour)")