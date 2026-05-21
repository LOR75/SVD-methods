library(FactoMineR)
library(factoextra)
library(dplyr)
library(cluster)
library(ggplot2)

# Extraction des coordonnées issues de l'AFM
df_cluster <- res_afm$ind$coord
dist_eucl  <- dist(df_cluster, method = "euclidean")

# ==========================================
# NOMMAGE AUTOMATIQUE DES CLUSTERS
# ==========================================
# Utilise df_p_paris (id_bureau alignés, noms sans accents) produit par analyses_temps.r
blocs_pres <- df_p_paris |>
  rename_with(~sub("_P22$", "", .), ends_with("_P22"))

cols_blocs <- intersect(
  c("Extreme_Gauche", "Gauche", "Centre", "Droite", "Extreme_Droite"),
  colnames(blocs_pres)
)

# Pour un vecteur nommé id_bureau -> numéro de cluster,
# renvoie un vecteur nommé "1","2",... -> label politique
make_cluster_labels <- function(cluster_vec) {
  df_tmp <- data.frame(
    id_bureau = names(cluster_vec),
    cluster   = cluster_vec
  ) |>
    inner_join(blocs_pres, by = "id_bureau") |>
    group_by(cluster) |>
    summarise(across(all_of(cols_blocs), mean, na.rm = TRUE), .groups = "drop")

  df_tmp$label <- apply(df_tmp[, cols_blocs, drop = FALSE], 1, function(row) {
    names(which.max(row))
  })

  # Dédoublonnage si deux clusters partagent le même bloc dominant
  counts <- table(df_tmp$label)
  for (lbl in names(counts[counts > 1])) {
    idx <- which(df_tmp$label == lbl)
    df_tmp$label[idx] <- paste0(lbl, " (", seq_along(idx), ")")
  }

  setNames(df_tmp$label, as.character(df_tmp$cluster))
}

# Remplace les numéros dans la légende d'un plot fviz_cluster
# en renommant les breaks de scale_color et scale_fill
apply_labels_to_plot <- function(p, labels) {
  p +
    scale_color_discrete(labels = labels) +
    scale_fill_discrete(labels = labels)
}

# ==========================================
# 1. K-MEANS
# ==========================================
set.seed(42)
km_nstart_50 <- kmeans(df_cluster, centers = 4, nstart = 50)

labels_km <- make_cluster_labels(
  setNames(km_nstart_50$cluster, rownames(df_cluster))
)

# On passe par list() pour que fviz_cluster ne recalcule pas les clusters
res_km <- list(data = df_cluster, cluster = km_nstart_50$cluster)

plot_kmeans <- fviz_cluster(res_km,
                            geom = "point",
                            ellipse.type = "convex",
                            palette = "jco",
                            main = "1. Clustering K-Means (4 clusters)",
                            ggtheme = theme_minimal()) +
  scale_color_brewer(palette = "Set1", labels = labels_km) +
  scale_fill_brewer(palette  = "Set1", labels = labels_km)

# ==========================================
# 2. CAH — MÉTHODE WARD
# ==========================================
cah_ward    <- hclust(dist_eucl, method = "ward.D2")
groupes_ward <- cutree(cah_ward, k = 4)

labels_ward <- make_cluster_labels(
  setNames(groupes_ward, rownames(df_cluster))
)

res_ward <- list(data = df_cluster, cluster = groupes_ward)

plot_ward <- fviz_cluster(res_ward,
                          geom = "point",
                          ellipse.type = "convex",
                          palette = "jco",
                          main = "2. CAH - Méthode de Ward (4 clusters)",
                          ggtheme = theme_minimal()) +
  scale_color_brewer(palette = "Set1", labels = labels_ward) +
  scale_fill_brewer(palette  = "Set1", labels = labels_ward)

# ==========================================
# 3. CAH — LIENS COMPLETS
# ==========================================
cah_complete    <- hclust(dist_eucl, method = "complete")
groupes_complete <- cutree(cah_complete, k = 4)

labels_complete <- make_cluster_labels(
  setNames(groupes_complete, rownames(df_cluster))
)

res_complete <- list(data = df_cluster, cluster = groupes_complete)

plot_complete <- fviz_cluster(res_complete,
                              geom = "point",
                              ellipse.type = "convex",
                              palette = "jco",
                              main = "3. CAH - Liens Complets (4 clusters)",
                              ggtheme = theme_minimal()) +
  scale_color_brewer(palette = "Set1", labels = labels_complete) +
  scale_fill_brewer(palette  = "Set1", labels = labels_complete)

# ==========================================
# SCORES DE SILHOUETTE
# ==========================================
sil_km       <- silhouette(km_nstart_50$cluster, dist_eucl)
sil_ward     <- silhouette(groupes_ward,          dist_eucl)
sil_complete <- silhouette(groupes_complete,      dist_eucl)

cat("\n--- Scores de Silhouette ---\n")
cat("K-means       :", mean(sil_km[, 3]),       "\n")
cat("CAH Ward      :", mean(sil_ward[, 3]),     "\n")
cat("CAH Complete  :", mean(sil_complete[, 3]), "\n")

print(plot_complete)