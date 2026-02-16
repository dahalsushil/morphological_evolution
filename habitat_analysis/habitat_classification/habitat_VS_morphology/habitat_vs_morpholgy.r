library(dplyr)

#load and combine data 
morpho <- read.csv("Symphyotrichum_morphometrics_updated.csv", header = TRUE)
morpho$Barcode <- NULL
morpho$Collector <- NULL
morpho$Collector.number <- NULL
morpho[morpho == "n/a"] <- NA
morpho <- na.omit(morpho)

# Set measurement columns as numeric -- "n/a" can result in importing as character, causing MANOVA problems
cols.num <- c("phyllary_length", "phyllary_width", "primary_axis_leaf_length", "primary_axis_leaf_width", "secondary_axis_leaf_length", "secondary_axis_leaf_width", "bract_length", "bract_width", "auricle_length", "primary_axis_hair_length", "below_capitulum_hair_length", "ray_length", "ray_width")
morpho[cols.num] <- sapply(morpho[cols.num],as.numeric)
head(morpho)

# Define species as a model factor
morpho$species <- as.factor(morpho$species)

# Normalize data matrix
morpho.nonnormalized <- morpho
morpho <- rapply(morpho,scale,c("numeric","integer"),how="replace")

# Check for duplicate rows
duplicate_rows <- duplicated(morpho$species)

# If there are duplicate rows, average them
if (any(duplicate_rows)) {
  morpho <- morpho %>%
    group_by(species) %>%
    summarize_all(mean, na.rm = TRUE) %>%
    ungroup()
}

morpho

classification=read.table("final_classification_k_6.csv", header = FALSE, sep = ",")
rownames(classification) <- classification[,1]
classification$V1<- NULL

combined <- merge(morpho, classification, by="row.names", all=TRUE)
combined <- na.omit(combined)

#################################
## subgenus analysis-- Virgulus
#################################
subgenus_virgulus <- c("Symphyotrichum bimater", "Symphyotrichum concolor", "Symphyotrichum pratense", "Symphyotrichum oblongifolium", "Symphyotrichum yukonense", "Symphyotrichum hintonii", "Symphyotrichum moranense", "Symphyotrichum patens", "Symphyotrichum adnatum", "Symphyotrichum walteri", "Symphyotrichum novae-angliae", "Symphyotrichum ericoides", "Symphyotrichum falcatum", "Symphyotrichum turneri", "Symphyotrichum purpurescens", "Symphyotrichum chihuahuense", "Symphyotrichum sericeum", "Symphyotrichum fendleri", "Symphyotrichum fontinale", "Symphyotrichum grandiflorum", "Symphyotrichum phlogifolium", "Symphyotrichum georgianum", "Symphyotrichum amethystinum", "Symphyotrichum plumosum", "Symphyotrichum lucayanum", "Symphyotrichum campestre", "Symphyotrichum estesii", "Symphyotrichum trilineatum", "Symphyotrichum pygmaeum")

morpho_subset1 <- morpho[morpho$species %in% subgenus_virgulus,]
morpho_subset1$species <- as.factor(as.character(morpho_subset1$species))
str(morpho_subset1)
#write.csv(morpho_subset1, "virgulus_morpho.updated.csv")
#I manually added habitat information on the dowloaded morphological fata and reloaded as follows

myData <- read.csv("virgulus_habitat_morpho.updated.csv", header = TRUE)
myData$habitat <- as.factor(as.character(myData$habitat))
str(myData)

# Build the discriminant
library(MASS)
discriminant <- lda(habitat ~ phyllary_length + phyllary_width + primary_axis_leaf_length + primary_axis_leaf_width + secondary_axis_leaf_length + secondary_axis_leaf_width + bract_length + bract_width + auricle_length + primary_axis_hair_length + below_capitulum_hair_length + ray_length + ray_width, data = myData, na.action="na.omit")
head(discriminant)
#write.csv(discriminant, "means.csv")

# Classification success
discriminant.jackknife <- lda(habitat ~ phyllary_length + phyllary_width + primary_axis_leaf_length + primary_axis_leaf_width + secondary_axis_leaf_length + secondary_axis_leaf_width + bract_length + bract_width + auricle_length + primary_axis_hair_length + below_capitulum_hair_length + ray_length + ray_width, data = myData, na.action="na.omit", CV = TRUE)
ct <- table(myData$habitat, discriminant.jackknife$class)
sum(diag(prop.table(ct)))

# Predict species by the discriminant function
discriminant.prediction <- predict(discriminant)

# Create dataframe for plotting
plotdata <- data.frame(type = myData$habitat, lda = discriminant.prediction$x)

##If you want to save lda values for testing phylogenetic signal (lda1 is probably the best to use)
##write.csv(plotdata, "plotdata.csv")

library(ggplot2)
ggplot(plotdata) + geom_boxplot(aes(lda.LD1, lda.LD2, colour = type))
plot <- ggplot(plotdata) + geom_point(aes(lda.LD1, lda.LD2, colour = type, shape = type), size = 10) +
  scale_color_viridis_d(option = "turbo") +
  scale_shape_manual(values = 1:21)

# Save as PDF
#ggsave("habitat_morpho_plot.updated.pdf", plot = plot, width = 8, height = 6)

# Create dataframe for plotting for species vs habitat
plotdata1 <- data.frame(type = myData$species, lda = discriminant.prediction$x)

##If you want to save lda values for testing phylogenetic signal (lda1 is probably the best to use)
##write.csv(plotdata1, "plotdata1.csv")

plot1 <- ggplot(plotdata1) + geom_point(aes(lda.LD1, lda.LD2, colour = type, shape = type), size = 10) +
  scale_color_viridis_d(option = "turbo") +
  scale_shape_manual(values = 1:21)

# Save as PDF
#ggsave("species_morpho_plot.updated.pdf", plot = plot1, width = 12, height = 8)


# Multivariate MANOVA
res.man <- manova(cbind(phyllary_length, phyllary_width, primary_axis_leaf_length, primary_axis_leaf_width, secondary_axis_leaf_length, secondary_axis_leaf_width, bract_length, bract_width, auricle_length, primary_axis_hair_length, below_capitulum_hair_length, ray_length, ray_width) ~ habitat, data = myData)
summary(res.man)

# Break down variable importance
summary.aov(res.man)


# Assess SPECIES pairwise significance 
# You must drop perfectly correlated values or you will get a rank deficiency error
#library(devtools)
#install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
pairwiseAdonis <- pairwise.adonis(myData[,c("phyllary_length", "phyllary_width", "primary_axis_leaf_length", "primary_axis_leaf_width", "secondary_axis_leaf_length", " secondary_axis_leaf_width", "bract_length", "bract_width", " auricle_length", " primary_axis_hair_length", "below_capitulum_hair_length", "ray_length", "ray_width")], myData$habitat, sim.method = "euclidean", p.adjust.m = "hochberg", perm = 10000)

write.csv(pairwiseAdonis, file = "habitat_pairwiseAdonis.csv")

# Break down variable importance
summary.aov(res.man)



