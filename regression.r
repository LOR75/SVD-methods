library(dplyr)
library(FactoMineR)
library(ggplot2)
library(tibble)

df_response <- df_l_paris

df_explanatory <- df_p_paris |>
  inner_join(df_m_paris, by = "id_bureau")

df_model <- df_response |>
  inner_join(df_explanatory, by = "id_bureau") |>
  column_to_rownames("id_bureau")

# ==========================================
# 1. DATA PREPARATION
# ==========================================
# Merge the response variables (Legis 2024) and explanatory variables (2017 - Euro 2024)
df_model <- df_response |>
  inner_join(df_explanatory, by = "id_bureau") |>
  column_to_rownames("id_bureau")

# Define our response variable 
# Define our explanatory variables 
response_var <- df_model$Centre_L24
df_expl <- df_model |> dplyr::select(-ends_with("_L24")) 

# ==========================================
# 2. STANDARD LINEAR REGRESSION (OLS)
# ==========================================
# Predicting the 2024 score based directly on past election scores
df_ols <- cbind(Target_2024 = response_var, df_expl)
model_ols <- lm(Target_2024 ~ ., data = df_ols)

print("--- Standard Linear Regression Summary ---")
summary(model_ols)

# ==========================================
# 3. SVD METHOD (PRINCIPAL COMPONENT REGRESSION)
# ==========================================
res_pca_expl <- PCA(df_expl, scale.unit = TRUE, ncp = 5, graph = FALSE)
svd_components <- as.data.frame(res_pca_expl$ind$coord)
df_pcr <- cbind(Target_2024 = response_var, svd_components)
model_pcr <- lm(Target_2024 ~ ., data = df_pcr)

print("--- SVD / Principal Component Regression Summary ---")
summary(model_pcr)
# ==========================================
# 4. VISUALISATIONS DES RÉSULTATS (PCR/SVD)
# ==========================================

# Graphique 1 : Valeurs Réelles vs Valeurs Prédites
# Ajout des prédictions générées par le modèle au tableau de données
df_pcr$Predictions <- predict(model_pcr)

plot_predictions <- ggplot(df_pcr, aes(x = Target_2024, y = Predictions)) +
  geom_point(alpha = 0.5, color = "#2E9FDF") +
  geom_abline(intercept = 0, slope = 1, color = "#FC4E07", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Performance du modèle SVD : Réalité vs Prédiction",
    subtitle = "Comparaison des scores observés et prédits pour 2024",
    x = "Scores Réels (Observés en 2024)",
    y = "Scores Prédits par le modèle SVD"
  ) +
  theme_minimal()

print(plot_predictions)


# Graphique 2 : Importance des Composantes SVD (Statistique t)
# Extraction des coefficients et de la statistique t (hors constante/Intercept)
coefs_pcr <- as.data.frame(summary(model_pcr)$coefficients[-1, ])
coefs_pcr$Composante <- rownames(coefs_pcr)

plot_importance <- ggplot(coefs_pcr, aes(x = reorder(Composante, abs(`t value`)), y = `t value`)) +
  geom_col(aes(fill = `t value` > 0), show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#00AFBB", "FALSE" = "#FC4E07")) +
  labs(
    title = "Importance et direction des Composantes SVD",
    subtitle = "Basé sur la statistique t (significativité du coefficient)",
    x = "Composantes SVD (Dimensions)",
    y = "Valeur t (Impact sur la prédiction)"
  ) +
  theme_minimal()