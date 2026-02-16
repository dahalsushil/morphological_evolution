library(ape)
library(phytools)
library(geiger)
library(dplyr)
library(ggplot2)

#load environmental data
envData <- read.csv("subg_virgulus_merged_extractions.csv")
summary(envData)

#selecting taxa to include in PCA
subgenus_virgulus <- c("Symphyotrichum bimater", "Symphyotrichum concolor", "Symphyotrichum pratense", "Symphyotrichum oblongifolium", "Symphyotrichum yukonense", "Symphyotrichum hintonii", "Symphyotrichum moranense", "Symphyotrichum patens", "Symphyotrichum adnatum", "Symphyotrichum walteri", "Symphyotrichum novae-angliae", "Symphyotrichum ericoides", "Symphyotrichum falcatum", "Symphyotrichum turneri", "Symphyotrichum purpurescens", "Symphyotrichum chihuahuense", "Symphyotrichum sericeum", "Symphyotrichum fendleri", "Symphyotrichum fontinale", "Symphyotrichum grandiflorum", "Symphyotrichum phlogifolium", "Symphyotrichum georgianum", "Symphyotrichum amethystinum", "Symphyotrichum plumosum", "Symphyotrichum lucayanum", "Symphyotrichum campestre", "Symphyotrichum estesii", "Symphyotrichum trilineatum", "Symphyotrichum pygmaeum")

myTrait_virgulus <- envData[envData$species %in% subgenus_virgulus,]
myTrait_virgulus$species <- as.factor(as.character(myTrait_virgulus$species))

#selecting columns to include in PCA
myTrait <- myTrait_virgulus[, 2:38]

#str checks for numeric or character values
str(myTrait)

#standardizing the data because PCA is sensitive to the scale of the data 
myTrait_normalized <- scale(myTrait)
head(myTrait_normalized)

#Perform PCA
pca <- prcomp(myTrait_normalized, center = TRUE, scale. = TRUE)
summary(pca)

#Variance explained by each principal component
variance_explained <- pca$sdev^2 / sum(pca$sdev^2)
cumulative_variance_explained <- cumsum(variance_explained)
print(variance_explained)
print(cumulative_variance_explained)

# Principal component scores
pca$x

# Create a data frame with PCA results
pca_df <- as.data.frame(pca$x)
pca_df$Species <- myTrait_virgulus$species
#write.csv(pca_df, "pca.csv") #I saved pca (PC1 to PC13) points as csv file 

#Visualization
plot <- ggplot(pca_df) + geom_point(aes(PC1, PC2, colour = Species, shape = Species), size = 8) +
  scale_color_viridis_d(option = "turbo") +
  scale_shape_manual(values = 1:25)

# Save as PDF
ggsave("pca_plot.pdf", plot = plot, width = 16, height = 10)

#Scree Plot: It is used to visualize the importance of each principal component and can be used to determine the number of principal components to retain. The scree plot can be generated using the fviz_eig() function. 
library(factoextra)
scree_plot <- fviz_eig(pca, addlabels = TRUE)
ggsave("scree_plot.pdf", plot = scree_plot, width = 10, height = 6)

#Biplot of the attributes: With the biplot, it is possible to visualize the similarities and dissimilarities between the samples, and further shows the impact of each attribute on each of the principal components.
# Graph of the variables
fviz_pca_var(pca, labelsize = 3, repel = TRUE)+
  theme(text = element_text(size = 7.5),
        axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 7.5)
  )

#Contribution of each variable: The goal of the third visualization is to determine how much each variable is represented in a given component. Such a quality of representation is called the Cos2 and corresponds to the square cosine, and it is computed using the fviz_cos2 function.
#A low value means that the variable is not perfectly represented by that component. 
#A high value, on the other hand, means a good representation of the variable on that component.
cos2 <- fviz_cos2(pca, choice = "var", axes = 1:2)
ggsave("PCA_quality_of_representation_of_variables.pdf", plot = cos2, width = 10, height = 6)

#Biplot combined with cos2: The last two visualization approaches: biplot and attributes importance can be combined to create a single biplot, where attributes with similar cos2 scores will have similar colors.  This is achieved by fine-tuning the fviz_pca_var function as follows:  
biplot <- fviz_pca_var(pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)

ggsave("PCA_biplot_each_morphometric_data_contribution.pdf", plot = biplot, width = 10, height = 6)

