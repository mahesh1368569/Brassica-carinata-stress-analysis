library("FactoMineR")
library("factoextra")


pca_salt = readxl::read_xlsx("PCA-salt.xlsx")

pca_salt <- as.data.frame(pca_salt)

rownames(pca_salt) <- pca_salt[, 1]

pca_salt = pca_salt[ ,-1]

cor.pca.salt <- round(cor(pca_salt),2)

corrplot(cor.pca.salt, type="upper", order="hclust", 
         tl.col="black", tl.srt=45)
pdf("correlations_traits.pdf", width = 7, height = 7)
cor_salt = corrplot(var_salt$cos2, is.corr=FALSE)

dev.off()

res.pca.salt <- PCA(pca_salt, graph = FALSE)

print(res.pca.salt)

# getting eigen values

eigenvalues <- get_eigenvalue(res.pca.salt)

eigenvalues

# Generating scree plot
scree_plot = fviz_screeplot(res.pca.salt, addlabels = TRUE, ylim = c(0, 50))

scree_plot

ggsave("scree_plot_salt.pdf", scree_plot, width = 4, height = 4)

# Extracting variables
var_salt = get_pca_var(res.pca.salt)

var_salt

# Quality of represents

head(var_salt$cos2, 4)

## Correlation plots between variables and dimensions

cor_salt = corrplot(var_salt$cos2, is.corr=FALSE)

# saving plot
pdf("correlations_salt.pdf", width = 5, height = 8)

corrplot(var_salt$cos2, 
         is.corr = FALSE,                   # Not a correlation matrix
         tl.col = "black",                  # Text label color
         cl.lim = c(0, max(var_salt$cos2, na.rm = TRUE)), # Color legend limits
         cl.cex = 1.2,                      # Legend text size
         cl.ratio = 0.3,                    # Legend bar width
         tl.srt = 45                        # Rotate text labels
)
dev.off()

barplot_cos2_salt = fviz_cos2(res.pca.salt, choice = "var", axes = 1:2)

barplot_cos2_salt


# Color by cos2 values: quality on the factor map
varbiles_factor_salt = fviz_pca_var(res.pca.salt, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE) +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))# Avoid text overlapping
)

ggsave("varbiles_factor_salt.pdf", varbiles_factor_salt, width = 6, height = 6, dpi = 1000)

## contributions of each variable - visualization

salt_contributions <- fviz_pca_var(res.pca.salt, col.var = "contrib",
                                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                   repel = TRUE) +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
        text = element_text(size = 12))  # Adjust text size

ggsave("salt_contributions.pdf", salt_contributions, width = 6, height = 6, dpi = 1000)

## dimension description
res.desc.salt = dimdesc(res.pca.salt, axes = c(1,2), proba = 0.05)

res.desc.salt$Dim.1

## graph of individuals

genoype_salt = fviz_pca_ind(res.pca.salt, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
        text = element_text(size = 12))# Avoid text overlapping (slow if many points

ggsave("genotype_salt.pdf", genoype_salt, width = 7, height = 5, dpi = 1000)
