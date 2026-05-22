library(dplyr)
library(FactoMineR)
library(factoextra)
library(tibble)
library(CCA)

# ==========================================
# 1. ANALYSE MULTIDIMENSIONNELLE
# ==========================================


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