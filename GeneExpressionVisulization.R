df <- data.frame(Embeddings(object = seuset_data, reduction = "umap"))
df$cluster <- unlist(seuset_data$seurat_clusters)

ggplot(df, aes(UMAP1, UMAP2, color = df[, i])) + 
          geom_point(size = 0.2, alpha = 0.7) + theme_bw() + 
          scale_colour_gradient(name = paste0("sox17", "expression"),
                                 low = "#001AFF", high = "#FF0000",
                                 na.value = "red") + 
          theme(aspect.ratio=1,
                plot.title = element_text(color="black", size=16, face="bold.italic", hjust = 0.5),
                axis.title=element_text(size=14,face="bold"),
                axis.text=element_text(size=11, face="bold"),
                legend.text=element_text(size=9, face="bold"),
                panel.border = element_rect(colour = "black", fill=NA, size=2))
