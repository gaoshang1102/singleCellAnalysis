df <- data.frame(Embeddings(object = seuset_data, reduction = "umap"))
df$cluster <- unlist(seuset_data$seurat_clusters)
centers <- aggregate(formula = .~cluster, data = df, FUN = mean)
ggplot(df, aes(UMAP_1, UMAP_2, color = cluster)) + geom_point() +
  annotate("text", x = centers$UMAP_1, y = centers$UMAP_2, label = centers$cluster)
