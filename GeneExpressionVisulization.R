df <- data.frame(Embeddings(object = seuset_data, reduction = "umap"))
df$cluster <- unlist(seuset_data$seurat_clusters)

  tmp <- df[, i]
  tmp <- pmax(tmp, quantile(tmp, 0.05))
  medtmp <- median(tmp)
  if(medtmp == 0){
    tmp[tmp > medtmp] <- (tmp[tmp > medtmp] - min(tmp[tmp > medtmp])) / (max(tmp[tmp > medtmp]) - min(tmp[tmp > medtmp])) / 2 + 0.5
  }else{
    tmp[tmp <= medtmp] <- (tmp[tmp <= medtmp] - min(tmp[tmp <= medtmp])) / (max(tmp[tmp <= medtmp]) - min(tmp[tmp <= medtmp])) /2
    tmp[tmp >= medtmp] <- (tmp[tmp >= medtmp] - min(tmp[tmp >= medtmp])) / (max(tmp[tmp >= medtmp]) - min(tmp[tmp >= medtmp])) /2 + 0.5
  }
  tiff(paste0("/home/sgao30/Dropbox/elife_EC/hlca/UMAPs/", i , "_expression.tiff"),
       height = 4, width = 4, units = "in", res = 200)
print(ggplot(df, aes(UMAP_1, UMAP_2, color = tmp)) + 
  geom_point(size = 0.2, alpha = 0.7) + theme_bw() + 
  scale_colour_gradient2(name = paste0(i, "\nexpression"),
                        midpoint = 0.5,
                        low = "#001AFF", high = "#FF0000", mid = "white",
                        na.value = "red") + 
  theme(aspect.ratio=1,
        plot.title = element_text(color="black", size=16, face="bold.italic", hjust = 0.5),
        axis.title=element_text(size=14,face="bold"),
        axis.text=element_text(size=11, face="bold"),
        legend.text=element_text(size=9, face="bold"),
        panel.border = element_rect(colour = "black", fill=NA, size=2)))
  dev.off()
