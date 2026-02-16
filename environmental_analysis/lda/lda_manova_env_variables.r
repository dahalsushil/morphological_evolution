library(dplyr)
library(ape)
library(picante)
library(phytools)
library(geiger)
library(ggplot2) #to plot
library(MASS) #to build discriminant
library(pairwiseAdonis)

#load and combine data 
combined <- read.csv("subg_virgulus_merged_extractions.csv", header = TRUE)

# Set measurement columns as numeric -- "n/a" can result in importing as character, causing MANOVA problems
cols.num <- c("aridity_index_UNEP", "BIOCLIM_1", "BIOCLIM_10", "BIOCLIM_11", "BIOCLIM_12", "BIOCLIM_13", "BIOCLIM_14", "BIOCLIM_15", "BIOCLIM_16", "BIOCLIM_17", "BIOCLIM_18", "BIOCLIM_19", "BIOCLIM_2", "BIOCLIM_3", "BIOCLIM_4", "BIOCLIM_5", "BIOCLIM_6", "BIOCLIM_7", "BIOCLIM_8", "BIOCLIM_9", "GTOPO30_ASPECT_reduced", "GTOPO30_ELEVATION", "GTOPO30_SLOPE_reduced", "ISRICSOILGRIDS_new_average_bulkdensity_reduced", "ISRICSOILGRIDS_new_average_claypercent_reduced", "ISRICSOILGRIDS_new_average_coarsefragmentpercent_reduced", "ISRICSOILGRIDS_new_average_nitrogen_reduced", "ISRICSOILGRIDS_new_average_phx10percent_reduced", "ISRICSOILGRIDS_new_average_sandpercent_reduced", "ISRICSOILGRIDS_new_average_siltpercent_reduced", "ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced", "LandCover_1_Needleleaf", "LandCover_2_Evergreeenbroadleaf", "LandCover_3_Deciduousbroadleaf", "LandCover_4_Mixedtrees", "LandCover_5_Shrubs", "LandCover_6_Herbaceous")
combined[cols.num] <- sapply(combined[cols.num],as.numeric)
head(combined)

# Define species as a model factor
combined$species <- as.factor(combined$species)

# Normalize data matrix
combined.nonnormalized <- combined
combined <- rapply(combined,scale,c("numeric","integer"),how="replace")
str(combined)

#################################
## subgenus analysis-- Virgulus
#################################
subgenus_virgulus <- c("Symphyotrichum bimater", "Symphyotrichum concolor", "Symphyotrichum pratense", "Symphyotrichum oblongifolium", "Symphyotrichum yukonense", "Symphyotrichum hintonii", "Symphyotrichum moranense", "Symphyotrichum patens", "Symphyotrichum adnatum", "Symphyotrichum walteri", "Symphyotrichum novae-angliae", "Symphyotrichum ericoides", "Symphyotrichum falcatum", "Symphyotrichum turneri", "Symphyotrichum purpurescens", "Symphyotrichum chihuahuense", "Symphyotrichum sericeum", "Symphyotrichum fendleri", "Symphyotrichum fontinale", "Symphyotrichum grandiflorum", "Symphyotrichum phlogifolium", "Symphyotrichum georgianum", "Symphyotrichum amethystinum", "Symphyotrichum plumosum", "Symphyotrichum lucayanum", "Symphyotrichum campestre", "Symphyotrichum estesii", "Symphyotrichum trilineatum", "Symphyotrichum pygmaeum")

combined_subset <- combined[combined$species %in% subgenus_virgulus,]
combined_subset$species <- as.factor(as.character(combined_subset$species))

#combined_subset1$species = factor(combined_subset1$species, labels = gsub("Symphyotrichum ", "S.", levels(combined_subset1$species)))
str(combined_subset)

#Build the discriminant
discriminant <- lda(species ~ aridity_index_UNEP + BIOCLIM_1 + BIOCLIM_10 + BIOCLIM_11 +	BIOCLIM_12 + BIOCLIM_13 + BIOCLIM_14 + BIOCLIM_15 + BIOCLIM_16 + BIOCLIM_17 + BIOCLIM_18 + BIOCLIM_19 + BIOCLIM_2 + BIOCLIM_3 + BIOCLIM_4 + BIOCLIM_5 + BIOCLIM_6 + BIOCLIM_7 + BIOCLIM_8 + BIOCLIM_9 + GTOPO30_ASPECT_reduced + GTOPO30_ELEVATION + GTOPO30_SLOPE_reduced + ISRICSOILGRIDS_new_average_bulkdensity_reduced + ISRICSOILGRIDS_new_average_claypercent_reduced + ISRICSOILGRIDS_new_average_coarsefragmentpercent_reduced + ISRICSOILGRIDS_new_average_nitrogen_reduced + ISRICSOILGRIDS_new_average_phx10percent_reduced + ISRICSOILGRIDS_new_average_sandpercent_reduced + ISRICSOILGRIDS_new_average_siltpercent_reduced + ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced + LandCover_1_Needleleaf + LandCover_2_Evergreeenbroadleaf + LandCover_3_Deciduousbroadleaf + LandCover_4_Mixedtrees + LandCover_5_Shrubs + LandCover_6_Herbaceous, data = combined_subset, na.action="na.omit")

#Classification success
discriminant.jackknife <- lda(species ~ aridity_index_UNEP + BIOCLIM_1 + BIOCLIM_10 + BIOCLIM_11 +	BIOCLIM_12 + BIOCLIM_13 + BIOCLIM_14 + BIOCLIM_15 + BIOCLIM_16 + BIOCLIM_17 + BIOCLIM_18 + BIOCLIM_19 + BIOCLIM_2 + BIOCLIM_3 + BIOCLIM_4 + BIOCLIM_5 + BIOCLIM_6 + BIOCLIM_7 + BIOCLIM_8 + BIOCLIM_9 + GTOPO30_ASPECT_reduced + GTOPO30_ELEVATION + GTOPO30_SLOPE_reduced + ISRICSOILGRIDS_new_average_bulkdensity_reduced + ISRICSOILGRIDS_new_average_claypercent_reduced + ISRICSOILGRIDS_new_average_coarsefragmentpercent_reduced + ISRICSOILGRIDS_new_average_nitrogen_reduced + ISRICSOILGRIDS_new_average_phx10percent_reduced + ISRICSOILGRIDS_new_average_sandpercent_reduced + ISRICSOILGRIDS_new_average_siltpercent_reduced + ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced + LandCover_1_Needleleaf + LandCover_2_Evergreeenbroadleaf + LandCover_3_Deciduousbroadleaf + LandCover_4_Mixedtrees + LandCover_5_Shrubs + LandCover_6_Herbaceous, data = combined_subset, na.action="na.omit", CV = TRUE)
ct <- table(combined_subset$species, discriminant.jackknife$class)
sum(diag(prop.table(ct)))

#Predict species by the discriminant function
discriminant.prediction <- predict(discriminant)

# Create dataframe for plotting
plotdata <- data.frame(type = combined_subset$species, lda = discriminant.prediction$x)

##If you want to save lda values for testing phylogenetic signal (lda1 is probably the best to use)
##write.csv(plotdata1, "plotdata1.csv")
ggplot(plotdata) + geom_boxplot(aes(lda.LD1, lda.LD2, colour = type)) + scale_color_viridis_d(option = "turbo")
plot <- ggplot(plotdata) + geom_point(aes(lda.LD1, lda.LD2, colour = type, shape = type), size = 6) +
  scale_color_viridis_d(option = "turbo") +
  scale_shape_manual(values = 1:25)
#ggsave("subgneus_virgulus_plot.pdf", plot = plot, width = 16, height = 10)


# Multivariate MANOVA
res.man <- manova(cbind(aridity_index_UNEP, BIOCLIM_1, BIOCLIM_10, BIOCLIM_11,	BIOCLIM_12, BIOCLIM_13, BIOCLIM_14, BIOCLIM_15, BIOCLIM_16, BIOCLIM_17, BIOCLIM_18, BIOCLIM_19, BIOCLIM_2, BIOCLIM_3, BIOCLIM_4, BIOCLIM_5, BIOCLIM_6, BIOCLIM_7, BIOCLIM_8, BIOCLIM_9, GTOPO30_ASPECT_reduced, GTOPO30_ELEVATION, GTOPO30_SLOPE_reduced, ISRICSOILGRIDS_new_average_bulkdensity_reduced, ISRICSOILGRIDS_new_average_claypercent_reduced, ISRICSOILGRIDS_new_average_coarsefragmentpercent_reduced, ISRICSOILGRIDS_new_average_nitrogen_reduced, ISRICSOILGRIDS_new_average_phx10percent_reduced, ISRICSOILGRIDS_new_average_sandpercent_reduced, ISRICSOILGRIDS_new_average_siltpercent_reduced, ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced, LandCover_1_Needleleaf, LandCover_2_Evergreeenbroadleaf, LandCover_3_Deciduousbroadleaf, LandCover_4_Mixedtrees, LandCover_5_Shrubs, LandCover_6_Herbaceous) ~ species, data = combined_subset)
summary(res.man)

# Break down variable importance
summary.aov(res.man)

#################################
## section analysis-- concolores
#################################
section_concolores <- c("Symphyotrichum concolor", "Symphyotrichum lucayanum", "Symphyotrichum plumosum", "Symphyotrichum pratense", "Symphyotrichum sericeum")

combined_subset1 <- combined_subset[combined_subset$species %in% section_concolores,]
combined_subset1$species <- as.factor(as.character(combined_subset1$species))
combined_subset1$species = factor(combined_subset1$species, labels = gsub("Symphyotrichum", "S. ", levels(combined_subset1$species)))

# Build the discriminant
discriminant1 <- lda(species ~ aridity_index_UNEP + BIOCLIM_1 + BIOCLIM_10 + BIOCLIM_11 +	BIOCLIM_12 + BIOCLIM_13 + BIOCLIM_14 + BIOCLIM_15 + BIOCLIM_16 + BIOCLIM_17 + BIOCLIM_18 + BIOCLIM_19 + BIOCLIM_2 + BIOCLIM_3 + BIOCLIM_4 + BIOCLIM_5 + BIOCLIM_6 + BIOCLIM_7 + BIOCLIM_8 + BIOCLIM_9 + GTOPO30_ASPECT_reduced + GTOPO30_ELEVATION + GTOPO30_SLOPE_reduced + ISRICSOILGRIDS_new_average_bulkdensity_reduced + ISRICSOILGRIDS_new_average_claypercent_reduced + ISRICSOILGRIDS_new_average_coarsefragmentpercent_reduced + ISRICSOILGRIDS_new_average_nitrogen_reduced + ISRICSOILGRIDS_new_average_phx10percent_reduced + ISRICSOILGRIDS_new_average_sandpercent_reduced + ISRICSOILGRIDS_new_average_siltpercent_reduced + ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced + LandCover_1_Needleleaf + LandCover_2_Evergreeenbroadleaf + LandCover_3_Deciduousbroadleaf + LandCover_4_Mixedtrees + LandCover_5_Shrubs + LandCover_6_Herbaceous, data = combined_subset1, na.action="na.omit")

# Classification success
discriminant.jackknife1 <- lda(species ~ aridity_index_UNEP + BIOCLIM_1 + BIOCLIM_10 + BIOCLIM_11 +	BIOCLIM_12 + BIOCLIM_13 + BIOCLIM_14 + BIOCLIM_15 + BIOCLIM_16 + BIOCLIM_17 + BIOCLIM_18 + BIOCLIM_19 + BIOCLIM_2 + BIOCLIM_3 + BIOCLIM_4 + BIOCLIM_5 + BIOCLIM_6 + BIOCLIM_7 + BIOCLIM_8 + BIOCLIM_9 + GTOPO30_ASPECT_reduced + GTOPO30_ELEVATION + GTOPO30_SLOPE_reduced + ISRICSOILGRIDS_new_average_bulkdensity_reduced + ISRICSOILGRIDS_new_average_claypercent_reduced + ISRICSOILGRIDS_new_average_coarsefragmentpercent_reduced + ISRICSOILGRIDS_new_average_nitrogen_reduced + ISRICSOILGRIDS_new_average_phx10percent_reduced + ISRICSOILGRIDS_new_average_sandpercent_reduced + ISRICSOILGRIDS_new_average_siltpercent_reduced + ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced + LandCover_1_Needleleaf + LandCover_2_Evergreeenbroadleaf + LandCover_3_Deciduousbroadleaf + LandCover_4_Mixedtrees + LandCover_5_Shrubs + LandCover_6_Herbaceous, data = combined_subset1, na.action="na.omit", CV = TRUE)
ct1 <- table(combined_subset1$species, discriminant.jackknife1$class)
sum(diag(prop.table(ct1)))

# Predict species by the discriminant function
discriminant.prediction1 <- predict(discriminant1)

# Create dataframe for plotting
plotdata1 <- data.frame(type = combined_subset1$species, lda = discriminant.prediction1$x)

ggplot(plotdata1) + geom_boxplot(aes(lda.LD1, lda.LD2, colour = type)) + scale_color_viridis_d(option = "turbo")
plot1 <- ggplot(plotdata1) + geom_point(aes(lda.LD1, lda.LD2, colour = type, shape = type), size = 6) +
  scale_color_viridis_d(option = "turbo") +
  scale_shape_manual(values = 1:21)
#ggsave("section_concolores_plot.pdf", plot = plot1, width = 10, height = 6)

# Multivariate MANOVA
res.man <- manova(cbind(aridity_index_UNEP, BIOCLIM_1, BIOCLIM_10, BIOCLIM_11,	BIOCLIM_12, BIOCLIM_13, BIOCLIM_14, BIOCLIM_15, BIOCLIM_16, BIOCLIM_17, BIOCLIM_18, BIOCLIM_19, BIOCLIM_2, BIOCLIM_3, BIOCLIM_4, BIOCLIM_5, BIOCLIM_6, BIOCLIM_7, BIOCLIM_8, BIOCLIM_9, GTOPO30_ASPECT_reduced, GTOPO30_ELEVATION, GTOPO30_SLOPE_reduced, ISRICSOILGRIDS_new_average_bulkdensity_reduced, ISRICSOILGRIDS_new_average_claypercent_reduced, ISRICSOILGRIDS_new_average_coarsefragmentpercent_reduced, ISRICSOILGRIDS_new_average_nitrogen_reduced, ISRICSOILGRIDS_new_average_phx10percent_reduced, ISRICSOILGRIDS_new_average_sandpercent_reduced, ISRICSOILGRIDS_new_average_siltpercent_reduced, ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced, LandCover_1_Needleleaf, LandCover_2_Evergreeenbroadleaf, LandCover_3_Deciduousbroadleaf, LandCover_4_Mixedtrees, LandCover_5_Shrubs, LandCover_6_Herbaceous) ~ species, data = combined_subset1)
summary(res.man)

# Break down variable importance
summary.aov(res.man)

# Assess SPECIES pairwise significance 
pairwise.adonis(combined_subset1[,c("aridity_index_UNEP", "BIOCLIM_1", "BIOCLIM_10", "BIOCLIM_11",	"BIOCLIM_12", "BIOCLIM_13", "BIOCLIM_14", "BIOCLIM_15", "BIOCLIM_16", "BIOCLIM_17", "BIOCLIM_18", "BIOCLIM_19", "BIOCLIM_2", "BIOCLIM_3", "BIOCLIM_4", "BIOCLIM_5", "BIOCLIM_6", "BIOCLIM_7", "BIOCLIM_8", "BIOCLIM_9", "GTOPO30_ASPECT_reduced", "GTOPO30_ELEVATION", "GTOPO30_SLOPE_reduced", "ISRICSOILGRIDS_new_average_bulkdensity_reduced", "ISRICSOILGRIDS_new_average_claypercent_reduced", "ISRICSOILGRIDS_new_average_coarsefragmentpercent_reduced", "ISRICSOILGRIDS_new_average_nitrogen_reduced", "ISRICSOILGRIDS_new_average_phx10percent_reduced", "ISRICSOILGRIDS_new_average_sandpercent_reduced", "ISRICSOILGRIDS_new_average_siltpercent_reduced", "ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced", "LandCover_1_Needleleaf", "LandCover_2_Evergreeenbroadleaf", "LandCover_3_Deciduousbroadleaf", "LandCover_4_Mixedtrees", "LandCover_5_Shrubs", "LandCover_6_Herbaceous")], combined_subset1$species, sim.method = "euclidean", p.adjust.m = "hochberg", perm = 10000)

#################################
## section analysis-- grandiflori
#################################
section_grandiflori<- c("Symphyotrichum campestre", "Symphyotrichum fendleri", "Symphyotrichum fontinale", "Symphyotrichum estesii", "Symphyotrichum grandiflorum", "Symphyotrichum oblongifolium", "Symphyotrichum pygmaeum", "Symphyotrichum yukonense", "Symphyotrichum gypsophilum", "Symphyotrichum hintonii", "Symphyotrichum moranense", "Symphyotrichum trilineatum", "Symphyotrichum purpurescens", "Symphyotrichum turneri", "Symphyotrichum bimater")

combined_subset2 <- combined_subset[combined_subset$species %in% section_grandiflori,]
combined_subset2$species <- as.factor(as.character(combined_subset2$species))
combined_subset2$species = factor(combined_subset2$species, labels = gsub("Symphyotrichum", "S.", levels(combined_subset2$species)))

# Build the discriminant
discriminant2 <- lda(species ~ aridity_index_UNEP + BIOCLIM_1 + BIOCLIM_10 + BIOCLIM_11 +	BIOCLIM_12 + BIOCLIM_13 + BIOCLIM_14 + BIOCLIM_15 + BIOCLIM_16 + BIOCLIM_17 + BIOCLIM_18 + BIOCLIM_19 + BIOCLIM_2 + BIOCLIM_3 + BIOCLIM_4 + BIOCLIM_5 + BIOCLIM_6 + BIOCLIM_7 + BIOCLIM_8 + BIOCLIM_9 + GTOPO30_ASPECT_reduced + GTOPO30_ELEVATION + GTOPO30_SLOPE_reduced + ISRICSOILGRIDS_new_average_bulkdensity_reduced + ISRICSOILGRIDS_new_average_claypercent_reduced + ISRICSOILGRIDS_new_average_coarsefragmentpercent_reduced + ISRICSOILGRIDS_new_average_nitrogen_reduced + ISRICSOILGRIDS_new_average_phx10percent_reduced + ISRICSOILGRIDS_new_average_sandpercent_reduced + ISRICSOILGRIDS_new_average_siltpercent_reduced + ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced + LandCover_1_Needleleaf + LandCover_2_Evergreeenbroadleaf + LandCover_3_Deciduousbroadleaf + LandCover_4_Mixedtrees + LandCover_5_Shrubs + LandCover_6_Herbaceous, data = combined_subset2, na.action="na.omit")

# Classification success
discriminant.jackknife2 <- lda(species ~ aridity_index_UNEP + BIOCLIM_1 + BIOCLIM_10 + BIOCLIM_11 +	BIOCLIM_12 + BIOCLIM_13 + BIOCLIM_14 + BIOCLIM_15 + BIOCLIM_16 + BIOCLIM_17 + BIOCLIM_18 + BIOCLIM_19 + BIOCLIM_2 + BIOCLIM_3 + BIOCLIM_4 + BIOCLIM_5 + BIOCLIM_6 + BIOCLIM_7 + BIOCLIM_8 + BIOCLIM_9 + GTOPO30_ASPECT_reduced + GTOPO30_ELEVATION + GTOPO30_SLOPE_reduced + ISRICSOILGRIDS_new_average_bulkdensity_reduced + ISRICSOILGRIDS_new_average_claypercent_reduced + ISRICSOILGRIDS_new_average_coarsefragmentpercent_reduced + ISRICSOILGRIDS_new_average_nitrogen_reduced + ISRICSOILGRIDS_new_average_phx10percent_reduced + ISRICSOILGRIDS_new_average_sandpercent_reduced + ISRICSOILGRIDS_new_average_siltpercent_reduced + ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced + LandCover_1_Needleleaf + LandCover_2_Evergreeenbroadleaf + LandCover_3_Deciduousbroadleaf + LandCover_4_Mixedtrees + LandCover_5_Shrubs + LandCover_6_Herbaceous, data = combined_subset2, na.action="na.omit", CV = TRUE)
ct2 <- table(combined_subset2$species, discriminant.jackknife2$class)
sum(diag(prop.table(ct2)))

# Predict species by the discriminant function
discriminant.prediction2 <- predict(discriminant2)

# Create dataframe for plotting
plotdata2 <- data.frame(type = combined_subset2$species, lda = discriminant.prediction2$x)

ggplot(plotdata2) + geom_boxplot(aes(lda.LD1, lda.LD2, colour = type)) + scale_color_viridis_d(option = "turbo")
plot2 <- ggplot(plotdata2) + geom_point(aes(lda.LD1, lda.LD2, colour = type, shape = type), size = 6) +
  scale_color_viridis_d(option = "turbo") +
  scale_shape_manual(values = 1:21)
#ggsave("section_grandiflori_plot.pdf", plot = plot2, width = 10, height = 6)

# Multivariate MANOVA
res.man <- manova(cbind(aridity_index_UNEP, BIOCLIM_1, BIOCLIM_10, BIOCLIM_11,	BIOCLIM_12, BIOCLIM_13, BIOCLIM_14, BIOCLIM_15, BIOCLIM_16, BIOCLIM_17, BIOCLIM_18, BIOCLIM_19, BIOCLIM_2, BIOCLIM_3, BIOCLIM_4, BIOCLIM_5, BIOCLIM_6, BIOCLIM_7, BIOCLIM_8, BIOCLIM_9, GTOPO30_ASPECT_reduced, GTOPO30_ELEVATION, GTOPO30_SLOPE_reduced, ISRICSOILGRIDS_new_average_bulkdensity_reduced, ISRICSOILGRIDS_new_average_claypercent_reduced, ISRICSOILGRIDS_new_average_coarsefragmentpercent_reduced, ISRICSOILGRIDS_new_average_nitrogen_reduced, ISRICSOILGRIDS_new_average_phx10percent_reduced, ISRICSOILGRIDS_new_average_sandpercent_reduced, ISRICSOILGRIDS_new_average_siltpercent_reduced, ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced, LandCover_1_Needleleaf, LandCover_2_Evergreeenbroadleaf, LandCover_3_Deciduousbroadleaf, LandCover_4_Mixedtrees, LandCover_5_Shrubs, LandCover_6_Herbaceous) ~ species, data = combined_subset2)
summary(res.man)

# Break down variable importance
summary.aov(res.man)

# Assess SPECIES pairwise significance 
# You must drop perfectly correlated values or you will get a rank deficiency error
#library(devtools)
#install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
pairwise.adonis(combined_subset2[,c("aridity_index_UNEP", "BIOCLIM_1", "BIOCLIM_10", "BIOCLIM_11",	"BIOCLIM_12", "BIOCLIM_13", "BIOCLIM_14", "BIOCLIM_15", "BIOCLIM_16", "BIOCLIM_17", "BIOCLIM_18", "BIOCLIM_19", "BIOCLIM_2", "BIOCLIM_3", "BIOCLIM_4", "BIOCLIM_5", "BIOCLIM_6", "BIOCLIM_7", "BIOCLIM_8", "BIOCLIM_9", "GTOPO30_ASPECT_reduced", "GTOPO30_ELEVATION", "GTOPO30_SLOPE_reduced", "ISRICSOILGRIDS_new_average_bulkdensity_reduced", "ISRICSOILGRIDS_new_average_claypercent_reduced", "ISRICSOILGRIDS_new_average_coarsefragmentpercent_reduced", "ISRICSOILGRIDS_new_average_nitrogen_reduced", "ISRICSOILGRIDS_new_average_phx10percent_reduced", "ISRICSOILGRIDS_new_average_sandpercent_reduced", "ISRICSOILGRIDS_new_average_siltpercent_reduced", "ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced", "LandCover_1_Needleleaf", "LandCover_2_Evergreeenbroadleaf", "LandCover_3_Deciduousbroadleaf", "LandCover_4_Mixedtrees", "LandCover_5_Shrubs", "LandCover_6_Herbaceous")], combined_subset2$species, sim.method = "euclidean", p.adjust.m = "hochberg", perm = 10000)

#################################
## section analysis-- patentes
#################################
section_patentes <- c("Symphyotrichum georgianum", "Symphyotrichum patens", "Symphyotrichum phlogifolium", "Symphyotrichum adnatum", "Symphyotrichum walteri")

combined_subset3 <- combined_subset[combined_subset$species %in% section_patentes,]
combined_subset3$species <- as.factor(as.character(combined_subset3$species))
combined_subset3$species = factor(combined_subset3$species, labels = gsub("Symphyotrichum", "S.", levels(combined_subset3$species)))

# Build the discriminant
discriminant3 <- lda(species ~ aridity_index_UNEP + BIOCLIM_1 + BIOCLIM_10 + BIOCLIM_11 +	BIOCLIM_12 + BIOCLIM_13 + BIOCLIM_14 + BIOCLIM_15 + BIOCLIM_16 + BIOCLIM_17 + BIOCLIM_18 + BIOCLIM_19 + BIOCLIM_2 + BIOCLIM_3 + BIOCLIM_4 + BIOCLIM_5 + BIOCLIM_6 + BIOCLIM_7 + BIOCLIM_8 + BIOCLIM_9 + GTOPO30_ASPECT_reduced + GTOPO30_ELEVATION + GTOPO30_SLOPE_reduced + ISRICSOILGRIDS_new_average_bulkdensity_reduced + ISRICSOILGRIDS_new_average_claypercent_reduced + ISRICSOILGRIDS_new_average_coarsefragmentpercent_reduced + ISRICSOILGRIDS_new_average_nitrogen_reduced + ISRICSOILGRIDS_new_average_phx10percent_reduced + ISRICSOILGRIDS_new_average_sandpercent_reduced + ISRICSOILGRIDS_new_average_siltpercent_reduced + ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced + LandCover_1_Needleleaf + LandCover_2_Evergreeenbroadleaf + LandCover_3_Deciduousbroadleaf + LandCover_4_Mixedtrees + LandCover_5_Shrubs + LandCover_6_Herbaceous, data = combined_subset3, na.action="na.omit")

# Classification success
discriminant.jackknife3 <- lda(species ~ aridity_index_UNEP + BIOCLIM_1 + BIOCLIM_10 + BIOCLIM_11 +	BIOCLIM_12 + BIOCLIM_13 + BIOCLIM_14 + BIOCLIM_15 + BIOCLIM_16 + BIOCLIM_17 + BIOCLIM_18 + BIOCLIM_19 + BIOCLIM_2 + BIOCLIM_3 + BIOCLIM_4 + BIOCLIM_5 + BIOCLIM_6 + BIOCLIM_7 + BIOCLIM_8 + BIOCLIM_9 + GTOPO30_ASPECT_reduced + GTOPO30_ELEVATION + GTOPO30_SLOPE_reduced + ISRICSOILGRIDS_new_average_bulkdensity_reduced + ISRICSOILGRIDS_new_average_claypercent_reduced + ISRICSOILGRIDS_new_average_coarsefragmentpercent_reduced + ISRICSOILGRIDS_new_average_nitrogen_reduced + ISRICSOILGRIDS_new_average_phx10percent_reduced + ISRICSOILGRIDS_new_average_sandpercent_reduced + ISRICSOILGRIDS_new_average_siltpercent_reduced + ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced + LandCover_1_Needleleaf + LandCover_2_Evergreeenbroadleaf + LandCover_3_Deciduousbroadleaf + LandCover_4_Mixedtrees + LandCover_5_Shrubs + LandCover_6_Herbaceous, data = combined_subset3, na.action="na.omit", CV = TRUE)
ct3 <- table(combined_subset3$species, discriminant.jackknife3$class)
sum(diag(prop.table(ct3)))

# Predict species by the discriminant function
discriminant.prediction3 <- predict(discriminant3)

# Create dataframe for plotting
plotdata3 <- data.frame(type = combined_subset3$species, lda = discriminant.prediction3$x)

ggplot(plotdata3) + geom_boxplot(aes(lda.LD1, lda.LD2, colour = type)) + scale_color_viridis_d(option = "turbo")
plot3 <- ggplot(plotdata3) + geom_point(aes(lda.LD1, lda.LD2, colour = type, shape = type), size = 6) +
  scale_color_viridis_d(option = "turbo") +
  scale_shape_manual(values = 1:21)
#ggsave("section_patentes_plot.pdf", plot = plot3, width = 10, height = 6)

# Multivariate MANOVA
res.man <- manova(cbind(aridity_index_UNEP, BIOCLIM_1, BIOCLIM_10, BIOCLIM_11,	BIOCLIM_12, BIOCLIM_13, BIOCLIM_14, BIOCLIM_15, BIOCLIM_16, BIOCLIM_17, BIOCLIM_18, BIOCLIM_19, BIOCLIM_2, BIOCLIM_3, BIOCLIM_4, BIOCLIM_5, BIOCLIM_6, BIOCLIM_7, BIOCLIM_8, BIOCLIM_9, GTOPO30_ASPECT_reduced, GTOPO30_ELEVATION, GTOPO30_SLOPE_reduced, ISRICSOILGRIDS_new_average_bulkdensity_reduced, ISRICSOILGRIDS_new_average_claypercent_reduced, ISRICSOILGRIDS_new_average_coarsefragmentpercent_reduced, ISRICSOILGRIDS_new_average_nitrogen_reduced, ISRICSOILGRIDS_new_average_phx10percent_reduced, ISRICSOILGRIDS_new_average_sandpercent_reduced, ISRICSOILGRIDS_new_average_siltpercent_reduced, ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced, LandCover_1_Needleleaf, LandCover_2_Evergreeenbroadleaf, LandCover_3_Deciduousbroadleaf, LandCover_4_Mixedtrees, LandCover_5_Shrubs, LandCover_6_Herbaceous) ~ species, data = combined_subset3)
summary(res.man)

# Break down variable importance
summary.aov(res.man)

# Assess SPECIES pairwise significance 
pairwise.adonis(combined_subset3[,c("aridity_index_UNEP", "BIOCLIM_1", "BIOCLIM_10", "BIOCLIM_11",	"BIOCLIM_12", "BIOCLIM_13", "BIOCLIM_14", "BIOCLIM_15", "BIOCLIM_16", "BIOCLIM_17", "BIOCLIM_18", "BIOCLIM_19", "BIOCLIM_2", "BIOCLIM_3", "BIOCLIM_4", "BIOCLIM_5", "BIOCLIM_6", "BIOCLIM_7", "BIOCLIM_8", "BIOCLIM_9", "GTOPO30_ASPECT_reduced", "GTOPO30_ELEVATION", "GTOPO30_SLOPE_reduced", "ISRICSOILGRIDS_new_average_bulkdensity_reduced", "ISRICSOILGRIDS_new_average_claypercent_reduced", "ISRICSOILGRIDS_new_average_coarsefragmentpercent_reduced", "ISRICSOILGRIDS_new_average_nitrogen_reduced", "ISRICSOILGRIDS_new_average_phx10percent_reduced", "ISRICSOILGRIDS_new_average_sandpercent_reduced", "ISRICSOILGRIDS_new_average_siltpercent_reduced", "ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced", "LandCover_1_Needleleaf", "LandCover_2_Evergreeenbroadleaf", "LandCover_3_Deciduousbroadleaf", "LandCover_4_Mixedtrees", "LandCover_5_Shrubs", "LandCover_6_Herbaceous")], combined_subset3$species, sim.method = "euclidean", p.adjust.m = "hochberg", perm = 10000)

#################################
## subset analysis-- polyliguli & ericoidei
#################################
section_polyliguli_ericoidei <- c("Symphyotrichum novae-angliae", "Symphyotrichum ericoides", "Symphyotrichum amethystinum", "Symphyotrichum falcatum")

combined_subset4 <- combined_subset[combined_subset$species %in% section_polyliguli_ericoidei,]
combined_subset4$species <- as.factor(as.character(combined_subset4$species))
combined_subset4$species = factor(combined_subset4$species, labels = gsub("Symphyotrichum", "S.", levels(combined_subset4$species)))

# Build the discriminant
discriminant4 <- lda(species ~ aridity_index_UNEP + BIOCLIM_1 + BIOCLIM_10 + BIOCLIM_11 +	BIOCLIM_12 + BIOCLIM_13 + BIOCLIM_14 + BIOCLIM_15 + BIOCLIM_16 + BIOCLIM_17 + BIOCLIM_18 + BIOCLIM_19 + BIOCLIM_2 + BIOCLIM_3 + BIOCLIM_4 + BIOCLIM_5 + BIOCLIM_6 + BIOCLIM_7 + BIOCLIM_8 + BIOCLIM_9 + GTOPO30_ASPECT_reduced + GTOPO30_ELEVATION + GTOPO30_SLOPE_reduced + ISRICSOILGRIDS_new_average_bulkdensity_reduced + ISRICSOILGRIDS_new_average_claypercent_reduced + ISRICSOILGRIDS_new_average_coarsefragmentpercent_reduced + ISRICSOILGRIDS_new_average_nitrogen_reduced + ISRICSOILGRIDS_new_average_phx10percent_reduced + ISRICSOILGRIDS_new_average_sandpercent_reduced + ISRICSOILGRIDS_new_average_siltpercent_reduced + ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced + LandCover_1_Needleleaf + LandCover_2_Evergreeenbroadleaf + LandCover_3_Deciduousbroadleaf + LandCover_4_Mixedtrees + LandCover_5_Shrubs + LandCover_6_Herbaceous, data = combined_subset4, na.action="na.omit")

# Classification success
discriminant.jackknife4 <- lda(species ~ aridity_index_UNEP + BIOCLIM_1 + BIOCLIM_10 + BIOCLIM_11 +	BIOCLIM_12 + BIOCLIM_13 + BIOCLIM_14 + BIOCLIM_15 + BIOCLIM_16 + BIOCLIM_17 + BIOCLIM_18 + BIOCLIM_19 + BIOCLIM_2 + BIOCLIM_3 + BIOCLIM_4 + BIOCLIM_5 + BIOCLIM_6 + BIOCLIM_7 + BIOCLIM_8 + BIOCLIM_9 + GTOPO30_ASPECT_reduced + GTOPO30_ELEVATION + GTOPO30_SLOPE_reduced + ISRICSOILGRIDS_new_average_bulkdensity_reduced + ISRICSOILGRIDS_new_average_claypercent_reduced + ISRICSOILGRIDS_new_average_coarsefragmentpercent_reduced + ISRICSOILGRIDS_new_average_nitrogen_reduced + ISRICSOILGRIDS_new_average_phx10percent_reduced + ISRICSOILGRIDS_new_average_sandpercent_reduced + ISRICSOILGRIDS_new_average_siltpercent_reduced + ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced + LandCover_1_Needleleaf + LandCover_2_Evergreeenbroadleaf + LandCover_3_Deciduousbroadleaf + LandCover_4_Mixedtrees + LandCover_5_Shrubs + LandCover_6_Herbaceous, data = combined_subset4, na.action="na.omit", CV = TRUE)
ct4 <- table(combined_subset4$species, discriminant.jackknife4$class)
sum(diag(prop.table(ct4)))

# Predict species by the discriminant function
discriminant.prediction4 <- predict(discriminant4)

# Create dataframe for plotting
plotdata4 <- data.frame(type = combined_subset4$species, lda = discriminant.prediction4$x)

ggplot(plotdata4) + geom_boxplot(aes(lda.LD1, lda.LD2, colour = type)) + scale_color_viridis_d(option = "turbo")
plot4 <- ggplot(plotdata4) + geom_point(aes(lda.LD1, lda.LD2, colour = type, shape = type), size = 6) +
  scale_color_viridis_d(option = "turbo") +
  scale_shape_manual(values = 1:21)
#ggsave("section_polyliguli & ericoidei_plot.pdf", plot = plot4, width = 10, height = 6)

# Multivariate MANOVA
res.man <- manova(cbind(aridity_index_UNEP, BIOCLIM_1, BIOCLIM_10, BIOCLIM_11,	BIOCLIM_12, BIOCLIM_13, BIOCLIM_14, BIOCLIM_15, BIOCLIM_16, BIOCLIM_17, BIOCLIM_18, BIOCLIM_19, BIOCLIM_2, BIOCLIM_3, BIOCLIM_4, BIOCLIM_5, BIOCLIM_6, BIOCLIM_7, BIOCLIM_8, BIOCLIM_9, GTOPO30_ASPECT_reduced, GTOPO30_ELEVATION, GTOPO30_SLOPE_reduced, ISRICSOILGRIDS_new_average_bulkdensity_reduced, ISRICSOILGRIDS_new_average_claypercent_reduced, ISRICSOILGRIDS_new_average_coarsefragmentpercent_reduced, ISRICSOILGRIDS_new_average_nitrogen_reduced, ISRICSOILGRIDS_new_average_phx10percent_reduced, ISRICSOILGRIDS_new_average_sandpercent_reduced, ISRICSOILGRIDS_new_average_siltpercent_reduced, ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced, LandCover_1_Needleleaf, LandCover_2_Evergreeenbroadleaf, LandCover_3_Deciduousbroadleaf, LandCover_4_Mixedtrees, LandCover_5_Shrubs, LandCover_6_Herbaceous) ~ species, data = combined_subset4)
summary(res.man)

# Break down variable importance
summary.aov(res.man)

#pairw
pairwise.adonis(combined_subset4[,c("aridity_index_UNEP", "BIOCLIM_1", "BIOCLIM_10", "BIOCLIM_11",	"BIOCLIM_12", "BIOCLIM_13", "BIOCLIM_14", "BIOCLIM_15", "BIOCLIM_16", "BIOCLIM_17", "BIOCLIM_18", "BIOCLIM_19", "BIOCLIM_2", "BIOCLIM_3", "BIOCLIM_4", "BIOCLIM_5", "BIOCLIM_6", "BIOCLIM_7", "BIOCLIM_8", "BIOCLIM_9", "GTOPO30_ASPECT_reduced", "GTOPO30_ELEVATION", "GTOPO30_SLOPE_reduced", "ISRICSOILGRIDS_new_average_bulkdensity_reduced", "ISRICSOILGRIDS_new_average_claypercent_reduced", "ISRICSOILGRIDS_new_average_coarsefragmentpercent_reduced", "ISRICSOILGRIDS_new_average_nitrogen_reduced", "ISRICSOILGRIDS_new_average_phx10percent_reduced", "ISRICSOILGRIDS_new_average_sandpercent_reduced", "ISRICSOILGRIDS_new_average_siltpercent_reduced", "ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced", "LandCover_1_Needleleaf", "LandCover_2_Evergreeenbroadleaf", "LandCover_3_Deciduousbroadleaf", "LandCover_4_Mixedtrees", "LandCover_5_Shrubs", "LandCover_6_Herbaceous")], combined_subset4$species, sim.method = "euclidean", p.adjust.m = "hochberg", perm = 10000)

