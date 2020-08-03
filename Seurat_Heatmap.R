DoHeatmap(seuset_data,features = genes, cells = names(Idents(seuset_data)[Idents(seuset_data) %in% c(0, 1)]))
