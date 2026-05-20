library(dplyr)
library(tidyr)
library(readxl)
library(arrow)
library(httr)
library(tibble)

options(timeout = 300)

# ==========================================
# 1. LÉGISLATIVES 2024
# ==========================================
url_opendata_gen <- function(x) {
  sprintf("https://opendata.paris.fr/api/datasets/1.0/elections-legislatives-2024-2emetour/attachments/ddct_berp_legislatives_2024_tour2_circ_%02d_20240707_xlsx/", x)
}

# Dictionnaire d'affectation des candidats aux blocs politiques
mapping_partis_l <- c(
  "NFP" = "KEMPF Raphaël", "ENS" = "MAILLARD Sylvain", "NFP" = "LAUSSUCQ Jean",
  "ENS" = "ROSSET Marine", "ENS" = "GUERINI Stanislas", "NFP" = "BALAGE EL MARIKY Léa",
  "ENS" = "PANOSYAN-BOUVET Astrid", "LR" = "BOULARD Geoffroy", "NFP" = "HERVIEU Céline",
  "ENS" = "GATEL Maud", "ENS" = "GREGOIRE Olivia", "NFP" = "MALAISÉ Celine",
  "ENS" = "AMIEL David", "NFP" = "NIAKATÉ Aminata", "ENS" = "HADDAD Benjamin",
  "UXD" = "PIQUET Louis", "DVG" = "SIMONNET Danielle", "NFP" = "VERZELETTI Céline"
)

# Extraction itérative des données par circonscription
donnees_votes_l <- data.frame()
for (i in c(1,2,3,4,11,12,13,14,15)) {
  chemin <- paste0("./data/legislative/opendata_paris_", i, ".xlsx")
  GET(url_opendata_gen(i), write_disk(chemin, overwrite = TRUE))
  donnees_tmp <- read_excel(chemin) |> rename(any_of(mapping_partis_l))
  donnees_votes_l <- bind_rows(donnees_votes_l, donnees_tmp)
}

# Agrégation des voix par grands blocs politiques
df_clean_legis <- donnees_votes_l |>
  mutate(across(all_of(names(mapping_partis_l)), ~ replace_na(., 0))) |>
  mutate(
    Gauche = NFP + DVG,
    Centre = ENS,
    Droite = LR,
    Extrême_Droite = UXD
  ) |>
  dplyr::select(ID_BVOTE, NB_EXPRIM, Gauche, Centre, Droite, Extrême_Droite)

df_pca_l <- df_clean_legis |>
  filter(NB_EXPRIM > 0) |>
  mutate(across(-c(ID_BVOTE, NB_EXPRIM), ~ . / NB_EXPRIM * 100)) |>
  column_to_rownames(var = "ID_BVOTE")

# ==========================================
# 2. PRÉSIDENTIELLE 2022
# ==========================================
url_pres <- "https://data.smartidf.services/api/explore/v2.1/catalog/datasets/elections-france-presidentielles-2022-1er-tour-par-bureau-de-vote/exports/parquet?lang=fr&timezone=Europe%2FParis"
GET(url_pres, write_disk("./data/presidentielle/donnes_votesp", overwrite = TRUE))

# Structuration par bureau de vote et agrégation par blocs
df_clean_pres <- read_parquet("./data/presidentielle/donnes_votesp") |>
  mutate(id_bureau = paste0(com_code, "-", code_du_b_vote)) |>
  pivot_wider(names_from = nom, values_from = voix, values_fill = 0) |>
  group_by(id_bureau, exprimes) |> 
  summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = 'drop') |>
  mutate(
    Extrême_Gauche = rowSums(pick(any_of(c("ARTHAUD", "POUTOU"))), na.rm = TRUE),
    Gauche         = rowSums(pick(any_of(c("HIDALGO", "JADOT", "MÉLENCHON", "ROUSSEL"))), na.rm = TRUE),
    Centre         = rowSums(pick(any_of(c("MACRON"))), na.rm = TRUE),
    Droite         = rowSums(pick(any_of(c("PÉCRESSE", "LASSALLE"))), na.rm = TRUE),
    Extrême_Droite = rowSums(pick(any_of(c("LE PEN", "ZEMMOUR", "DUPONT-AIGNAN"))), na.rm = TRUE)
  ) |>
  dplyr::select(id_bureau, exprimes, Extrême_Gauche, Gauche, Centre, Droite, Extrême_Droite)

df_pca_p <- df_clean_pres |>
  filter(exprimes > 0) |>
  group_by(id_bureau) |>
  summarise(across(everything(), sum, na.rm = TRUE)) |>
  mutate(across(-c(id_bureau, exprimes), ~ . / exprimes * 100)) |>
  column_to_rownames(var = "id_bureau")

# ==========================================
# 3. MUNICIPALES 2020
# ==========================================
url_muni <- "https://hub.huwise.com/api/explore/v2.1/catalog/datasets/election-france-municipale-2020-premier-tour/exports/parquet/?lang=fr&timezone=Europe%2FParis"
GET(url_muni, write_disk("./data/municipales/donnes_votesm", overwrite = TRUE))  

# Fonction de catégorisation des nuances politiques
grouper_nuances <- function(n) {
  case_when(
    n %in% c("LEXG") ~ "Extrême_Gauche",
    n %in% c("LUG", "LFI", "LCOM", "LSOC", "LVEC", "LDVG") ~ "Gauche",
    n %in% c("LREM", "LMDM", "LUDI", "LUC", "LDVC", "NC") ~ "Centre_et_Divers",
    n %in% c("LLR", "LUD", "LDVD") ~ "Droite",
    n %in% c("LRN", "LEXD") ~ "Extrême_Droite",
    TRUE ~ "Centre_et_Divers"
  )
}

# Application du regroupement et calcul des totaux
df_clean_muni <- read_parquet("./data/municipales/donnes_votesm") |> 
  mutate(
    id_bureau = paste0(com_code, "-", office),
    Bloc_Politique = grouper_nuances(nuance)
  ) |> 
  group_by(id_bureau, Bloc_Politique) |> 
  summarise(total_voix = sum(vote, na.rm = TRUE), .groups = 'drop') |>
  pivot_wider(names_from = Bloc_Politique, values_from = total_voix, values_fill = 0) |>
  mutate(total_calcule = rowSums(across(where(is.numeric)), na.rm = TRUE)) |>
  filter(total_calcule > 0)

df_pca_m <- df_clean_muni |>
  mutate(across(-c(id_bureau, total_calcule), ~ . / total_calcule * 100)) |>
  column_to_rownames(var = "id_bureau") |>
  dplyr::select(-total_calcule)

# Nettoyage de l'environnement de travail
rm(list=setdiff(ls(), c("df_clean_legis", "df_clean_pres", "df_clean_muni")))