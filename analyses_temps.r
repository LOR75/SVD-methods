library(dplyr)
library(FactoMineR)
library(factoextra)
library(tibble)
library(CCA)

# ==========================================
# 1. UNIFORMISATION DES IDENTIFIANTS
# ==========================================
# Formatage des identifiants de la Ville de Paris au format INSEE standard
df_l_paris <- df_clean_legis |>
  mutate(
    arrondissement = as.numeric(sub("-.*", "", ID_BVOTE)),
    bureau = as.numeric(sub(".*-", "", ID_BVOTE)),
    id_bureau = paste0("751", sprintf("%02d", arrondissement), "-", bureau)
  ) |>
  dplyr::select(-ID_BVOTE, -NB_EXPRIM, -arrondissement, -bureau) |>
  group_by(id_bureau) |>
  summarise(across(everything(), sum, na.rm = TRUE)) |>
  rename_with(~paste0(., "_L24"), -id_bureau)

# Fonction de traduction des codes électoraux du Ministère de l'Intérieur
traduire_etat <- function(df) {
  df |>
    filter(grepl("^75056", id_bureau)) |>
    mutate(
      b_code = sub(".*-", "", id_bureau),
      new_commune = paste0("751", substr(b_code, 1, 2)),
      new_bureau = as.character(as.numeric(substr(b_code, 3, nchar(b_code)))),
      id_bureau = paste0(new_commune, "-", new_bureau)
    ) |>
    dplyr::select(-b_code, -new_commune, -new_bureau) |>
    group_by(id_bureau) |>
    summarise(across(everything(), sum, na.rm = TRUE))
}

# Application de l'uniformisation aux jeux de données
df_p_paris <- df_clean_pres |> 
  dplyr::select(-exprimes) |> 
  traduire_etat() |>
  rename_with(~paste0(., "_P22"), -id_bureau)

df_m_paris <- df_clean_muni |> 
  dplyr::select(-total_calcule) |> 
  traduire_etat() |>
  rename_with(~paste0(., "_M20"), -id_bureau)

# ==========================================
# 2. FUSION ET ANALYSE MULTIDIMENSIONNELLE
# ==========================================
# Jointure des trois années électorales par bureau de vote
df_acc <- df_l_paris |>
  inner_join(df_p_paris, by = "id_bureau") |>
  inner_join(df_m_paris, by = "id_bureau") |>
  column_to_rownames("id_bureau")

# Définition dynamique des groupes de variables pour l'AFM
taille_l <- ncol(df_l_paris) - 1
taille_p <- ncol(df_p_paris) - 1
taille_m <- ncol(df_m_paris) - 1

# Exécution de l'Analyse Factorielle Multiple (AFM)
res_afm <- MFA(df_acc, 
               group = c(taille_l, taille_p, taille_m), 
               type = c("s", "s", "s"),
               name.group = c("Legis_2024", "Pres_2022", "Muni_2020"),
               graph = FALSE)

# Visualisation des transferts électoraux
fviz_mfa_var(res_afm, "quanti.var", palette = "jco", repel = TRUE) +
  labs(title = "AFM - Évolution et Transferts des votes à Paris",
       subtitle = "Saisons 2020 (Municipales), 2022 (Présidentielle), 2024 (Législatives)")

# Execution de l'Analyse en Composantes Canoniques (ACC) entre Municipales 2020 et Présidentielle 2022
df_cca <- inner_join(df_m_paris, df_p_paris, by = "id_bureau") |>
    column_to_rownames("id_bureau")

    X <- df_cca |> dplyr::select(ends_with("_M20"))
    Y <- df_cca |> dplyr::select(ends_with("_P22"))

    res_cca_mp <- cc(X, Y)

    #plt.cc(res_cca_mp, var.label = TRUE, type = "v")