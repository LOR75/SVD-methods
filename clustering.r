library(FactoMineR)
library(factoextra)
library(dplyr)
library(cluster)

# Extraction des coordonnées issues de l'AFM
df_cluster <- res_afm$ind$coord
dist_eucl <- dist(df_cluster, method = "euclidean")

# ==========================================
# 1. NUAGE K-MEANS
# ==========================================
set.seed(42) 
km_nstart_50 <- kmeans(df_cluster, centers = 4, nstart = 50)

plot_kmeans <- fviz_cluster(km_nstart_50, data = df_cluster,
                            geom = "point", 
                            ellipse.type = "convex",
                            palette = "jco",
                            main = "1. Clustering K-Means (4 clusters)",
                            ggtheme = theme_minimal())


# ==========================================
# 2. NUAGE CAH (MÉTHODE WARD)
# ==========================================
cah_ward <- hclust(dist_eucl, method = "ward.D2")
groupes_ward <- cutree(cah_ward, k = 4)

# Astuce : fviz_cluster a besoin d'une liste combinant données et clusters
res_ward <- list(data = df_cluster, cluster = groupes_ward)

plot_ward <- fviz_cluster(res_ward,
                          geom = "point", 
                          ellipse.type = "convex",
                          palette = "jco",
                          main = "2. CAH - Méthode de Ward (4 clusters)",
                          ggtheme = theme_minimal())


# ==========================================
# 3. NUAGE CAH (LIENS COMPLETS)
# ==========================================
cah_complete <- hclust(dist_eucl, method = "complete")
groupes_complete <- cutree(cah_complete, k = 4)

res_complete <- list(data = df_cluster, cluster = groupes_complete)

plot_complete <- fviz_cluster(res_complete,
                              geom = "point", 
                              ellipse.type = "convex",
                              palette = "jco",
                              main = "3. CAH - Liens Complets (4 clusters)",
                              ggtheme = theme_minimal())

# ==========================================
# AFFICHAGE POUR LE RAPPORT (.qmd)
# ==========================================



# Calcul des scores de silhouette pour l'analyse
sil_km <- silhouette(km_nstart_50$cluster, dist_eucl)
sil_ward <- silhouette(groupes_ward, dist_eucl)
sil_complete <- silhouette(groupes_complete, dist_eucl)

cat("\n--- Scores de Silhouette ---\n")
cat("K-means       :", mean(sil_km[, 3]), "\n")
cat("CAH Ward      :", mean(sil_ward[, 3]), "\n")
cat("CAH Complete  :", mean(sil_complete[, 3]), "\n")

print(plot_ward)