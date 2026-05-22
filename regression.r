library(dplyr)
library(FactoMineR)
library(ggplot2)
library(tibble)

df_response <- df_p_paris

df_explanatory <- df_m_paris |>
  filter(if_any(contains("Centre"), ~ . > 0))

df_model <- df_response |>
  inner_join(df_explanatory, by = "id_bureau") |>
  column_to_rownames("id_bureau")

target_col <- grep("^Centre", names(df_response), value = TRUE)[1]
response_var <- df_model[[target_col]]

expl_cols <- setdiff(names(df_explanatory), "id_bureau")
df_expl <- df_model |> dplyr::select(all_of(expl_cols))

df_ols <- cbind(Target_Pres = response_var, df_expl)
model_ols <- lm(Target_Pres ~ ., data = df_ols)

summary(model_ols)

res_pca_expl <- PCA(df_expl, scale.unit = TRUE, ncp = 5, graph = FALSE)
svd_components <- as.data.frame(res_pca_expl$ind$coord)
df_pcr <- cbind(Target_Pres = response_var, svd_components)
model_pcr <- lm(Target_Pres ~ ., data = df_pcr)

summary(model_pcr)

df_pcr$Predictions <- predict(model_pcr)

plot_predictions <- ggplot(df_pcr, aes(x = Target_Pres, y = Predictions)) +
  geom_point(alpha = 0.5, color = "#2E9FDF") +
  geom_abline(intercept = 0, slope = 1, color = "#FC4E07", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Performance du modèle SVD : Réalité vs Prédiction",
    subtitle = "Comparaison des scores observés et prédits",
    x = "Scores municipales",
    y = "Scores Présidentiels"
  ) +
  theme_minimal()

print(plot_predictions)

coefs_pcr <- as.data.frame(summary(model_pcr)$coefficients[-1, ])
coefs_pcr$Composante <- rownames(coefs_pcr)

plot_importance <- ggplot(coefs_pcr, aes(x = reorder(Composante, abs(`t value`)), y = `t value`)) +
  geom_col(aes(fill = `t value` > 0), show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#00AFBB", "FALSE" = "#FC4E07")) +
  labs(
    title = "Importance et direction des Composantes SVD",
    subtitle = "Basé sur la statistique t",
    x = "Composantes SVD",
    y = "Valeur t"
  ) +
  theme_minimal()