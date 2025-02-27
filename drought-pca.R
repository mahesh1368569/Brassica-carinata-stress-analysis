library("FactoMineR")
library("factoextra")


pca_drought = readxl::read_xlsx("PCA-drought.xlsx")

pca_drought <- as.data.frame(pca_drought)

rownames(pca_drought) <- pca_drought[, 1]

pca_drought = pca_drought[ ,-1]

cor.pca.drought <- round(cor(pca_drought),2)


res.pca.drought <- PCA(pca_drought, graph = FALSE)

print(res.pca.drought)

# getting eigen values

eigenvalues_drought <- get_eigenvalue(res.pca.drought)

eigenvalues_drought

# Generating scree plot
scree_plot_drought = fviz_screeplot(res.pca.drought, addlabels = TRUE, ylim = c(0, 50))

scree_plot_drought

ggsave("scree_plot_drought.pdf", scree_plot_drought, width = 4, height = 4)

# Extracting variables
var_drought = get_pca_var(res.pca.drought)

var_drought

# Quality of represents

head(var_drought$cos2, 4)

## Correlation plots between variables and dimensions

cor_drought = corrplot(var_drought$cos2, is.corr=FALSE)

# saving plot
pdf("correlations_drought.pdf", width = 5, height = 8)

corrplot(var_drought$cos2, 
         is.corr = FALSE,                   # Not a correlation matrix
         tl.col = "black",                  # Text label color
         cl.lim = c(0, max(var_drought$cos2, na.rm = TRUE)), # Color legend limits
         cl.cex = 1.2,                      # Legend text size
         cl.ratio = 0.3,                    # Legend bar width
         tl.srt = 45                        # Rotate text labels
)
dev.off()

barplot_cos2_drought = fviz_cos2(res.pca.drought, choice = "var", axes = 1:2)

barplot_cos2_drought


# Color by cos2 values: quality on the factor map
varbiles_factor_drought = fviz_pca_var(res.pca.drought, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE) +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))# Avoid text overlapping


ggsave("varbiles_factor_drought.pdf", varbiles_factor_drought, width = 6, height = 6, dpi = 1000)

## contributions of each variable - visualization

Drought_contributions <- fviz_pca_var(res.pca.drought, col.var = "contrib",
                                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                   repel = TRUE) +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
        text = element_text(size = 12))  # Adjust text size

ggsave("Drought_contributions.pdf", Drought_contributions, width = 6, height = 6, dpi = 1000)

## dimension description
res.desc.drought = dimdesc(res.pca.drought, axes = c(1,2), proba = 0.05)

res.desc.drought$Dim.1

## graph of individuals

genoype_drought = fviz_pca_ind(res.pca.drought, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
        text = element_text(size = 12))# Avoid text overlapping (slow if many points

ggsave("genotype_drought.pdf", genoype_drought, width = 7, height = 5, dpi = 1000)
