library(clusterProfiler)
library(org.Mm.eg.db)

GOErichment <- function(geneList, figurePlotName){
ego2 <- enrichGO(gene         =  geneList,
                 OrgDb         = org.Hs.eg.db,
                 keyType       = 'SYMBOL',
                 ont           = "BP",
                 pAdjustMethod = "BH",
                 pvalueCutoff  = 0.01,
                 qvalueCutoff  = 0.05)
ego2 <- data.frame(Term = ego2@result$Description,
                   Gene_ratio = ego2@result$GeneRatio, 
                   Pvalue = ego2@result$p.adjust,
                   stringsAsFactors = F)
ego2 <- data.frame(Term = ego2$Term,
                   Gene_ratio = as.integer(unlist(lapply(strsplit(ego2$Gene_ratio, "/"), `[[`, 1))) / as.integer(unlist(lapply(strsplit(ego2$Gene_ratio, "/"), `[[`, 2))), 
                   Pvalue = ego2$Pvalue,
                   stringsAsFactors = F)
ego2_tmp <- ego2[1:20, ]
png(figurePlotName, height = 1000, width = 1000)
print(ggplot(ego2_tmp, aes(y = Gene_ratio, x = factor(ego2_tmp$Term, levels = rev(ego2_tmp$Term[order(ego2_tmp$Pvalue)])))) +
  geom_col(aes(fill = Pvalue)) + theme_bw() + coord_flip() +
  theme(legend.title = element_text(size=14,face="bold"),
        plot.title = element_text(color="black", size=16, face="bold.ariel", hjust = 0.5),
        axis.title=element_text(size=14,face="bold"),
        axis.text=element_text(size=14, face="bold", color = "black",  family="Arial"),
        legend.text=element_text(size=12, face="bold"),
        panel.border = element_rect(colour = "black", fill=NA, size=2)) + ylab("") +
  scale_fill_gradient(low = "#FF3333", high = "#FF9999") +
  guides(colour = guide_colourbar(order = 1),
         size = guide_legend(order = 2)) + xlab(""))
dev.off()
}
