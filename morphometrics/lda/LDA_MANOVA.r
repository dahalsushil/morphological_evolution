library(dplyr)

#load and combine data 
combined <- read.csv("Symphyotrichum_morphometrics_final.csv", header = TRUE)
combined$Barcode <- NULL
combined$Collector <- NULL
combined$Collector.number <- NULL
combined[combined == "n/a"] <- NA
combined <- na.omit(combined)

# Set measurement columns as numeric -- "n/a" can result in importing as character, causing MANOVA problems
cols.num <- c("phyllary_length", "phyllary_width", "primary_axis_leaf_length", "primary_axis_leaf_width", "secondary_axis_leaf_length", "secondary_axis_leaf_width", "bract_length", "bract_width", "auricle_length", "primary_axis_hair_length", "below_capitulum_hair_length", "ray_length", "ray_width")
combined[cols.num] <- sapply(combined[cols.num],as.numeric)
head(combined)

# Define species as a model factor
combined$species <- as.factor(combined$species)

# Normalize data matrix
combined.nonnormalized <- combined
combined <- rapply(combined,scale,c("numeric","integer"),how="replace")

#################################
## subgenus analysis-- Virgulus
#################################
subgenus_virgulus <- c("Symphyotrichum bimater", "Symphyotrichum concolor", "Symphyotrichum pratense", "Symphyotrichum oblongifolium", "Symphyotrichum yukonense", "Symphyotrichum hintonii", "Symphyotrichum moranense", "Symphyotrichum patens", "Symphyotrichum adnatum", "Symphyotrichum walteri", "Symphyotrichum novae-angliae", "Symphyotrichum ericoides", "Symphyotrichum falcatum", "Symphyotrichum turneri", "Symphyotrichum purpurescens", "Symphyotrichum chihuahuense", "Symphyotrichum sericeum", "Symphyotrichum fendleri", "Symphyotrichum fontinale", "Symphyotrichum grandiflorum", "Symphyotrichum phlogifolium", "Symphyotrichum georgianum", "Symphyotrichum amethystinum", "Symphyotrichum plumosum", "Symphyotrichum lucayanum", "Symphyotrichum campestre", "Symphyotrichum estesii", "Symphyotrichum trilineatum", "Symphyotrichum pygmaeum")

combined_subset1 <- combined[combined$species %in% subgenus_virgulus,]
combined_subset1$species <- as.factor(as.character(combined_subset1$species))
combined_subset1$species = factor(combined_subset1$species, labels = gsub("Symphyotrichum ", "S.", levels(combined_subset1$species)))
str(combined_subset1)

# Build the discriminant
library(MASS)
discriminant <- lda(species ~ phyllary_length + phyllary_width + primary_axis_leaf_length + primary_axis_leaf_width + secondary_axis_leaf_length + secondary_axis_leaf_width + bract_length + bract_width + auricle_length + primary_axis_hair_length + below_capitulum_hair_length + ray_length + ray_width, data = combined_subset1, na.action="na.omit")
head(discriminant)
write.csv(discriminant, "means.csv")

# Classification success
discriminant.jackknife <- lda(species ~ phyllary_length + phyllary_width + primary_axis_leaf_length + primary_axis_leaf_width + secondary_axis_leaf_length + secondary_axis_leaf_width + bract_length + bract_width + auricle_length + primary_axis_hair_length + below_capitulum_hair_length + ray_length + ray_width, data = combined_subset1, na.action="na.omit", CV = TRUE)
ct1 <- table(combined_subset1$species, discriminant.jackknife$class)
sum(diag(prop.table(ct1)))

# Predict species by the discriminant function
discriminant.prediction <- predict(discriminant)

# Create dataframe for plotting
plotdata1 <- data.frame(type = combined_subset1$species, lda = discriminant.prediction$x)

##If you want to save lda values for testing phylogenetic signal (lda1 is probably the best to use)
##write.csv(plotdata1, "plotdata1.csv")

library(ggplot2)
ggplot(plotdata1) + geom_boxplot(aes(lda.LD1, lda.LD2, colour = type))
ggplot(plotdata1) + geom_point(aes(lda.LD1, lda.LD2, colour = type), size = 2)


# Multivariate MANOVA
res.man <- manova(cbind(phyllary_length, phyllary_width, primary_axis_leaf_length, primary_axis_leaf_width, secondary_axis_leaf_length, secondary_axis_leaf_width, bract_length, bract_width, auricle_length, primary_axis_hair_length, below_capitulum_hair_length, ray_length, ray_width) ~ species, data = combined_subset1)
summary(res.man)

# Break down variable importance
summary.aov(res.man)

#################################
## section analysis-- concolores
#################################
section_concolores <- c("Symphyotrichum concolor", "Symphyotrichum lucayanum", "Symphyotrichum plumosum", "Symphyotrichum pratense", "Symphyotrichum sericeum")

combined_subset2 <- combined[combined$species %in% section_concolores,]
combined_subset2$species <- as.factor(as.character(combined_subset2$species))
combined_subset2$species = factor(combined_subset2$species, labels = gsub("Symphyotrichum ", "S.", levels(combined_subset2$species)))

# Build the discriminant
library(MASS)
discriminant2 <- lda(species ~ phyllary_length + phyllary_width + primary_axis_leaf_length + primary_axis_leaf_width + secondary_axis_leaf_length + secondary_axis_leaf_width + bract_length + bract_width + primary_axis_hair_length + below_capitulum_hair_length + ray_length + ray_width, data = combined_subset2, na.action="na.omit")

# Classification success
discriminant.jackknife2 <- lda(species ~ phyllary_length + phyllary_width + primary_axis_leaf_length + primary_axis_leaf_width + secondary_axis_leaf_length + secondary_axis_leaf_width + bract_length + bract_width + primary_axis_hair_length + below_capitulum_hair_length + ray_length + ray_width, data = combined_subset2, na.action="na.omit", CV = TRUE)
ct2 <- table(combined_subset2$species, discriminant.jackknife2$class)
sum(diag(prop.table(ct2)))

# Predict species by the discriminant function
discriminant.prediction2 <- predict(discriminant2)

# Create dataframe for plotting
plotdata2 <- data.frame(type = combined_subset2$species, lda = discriminant.prediction2$x)

library(ggplot2)
ggplot(plotdata2) + geom_boxplot(aes(lda.LD1, lda.LD2, colour = type))
ggplot(plotdata2) + geom_point(aes(lda.LD1, lda.LD2, colour = type), size = 2.5)

# Multivariate MANOVA
res.man <- manova(cbind(phyllary_length, phyllary_width, primary_axis_leaf_length, primary_axis_leaf_width, secondary_axis_leaf_length, secondary_axis_leaf_width, bract_length, bract_width, primary_axis_hair_length, below_capitulum_hair_length, ray_length, ray_width) ~ species, data = combined_subset2)
summary(res.man)

# Break down variable importance
summary.aov(res.man)

# Assess SPECIES pairwise significance 
# You must drop perfectly correlated values or you will get a rank deficiency error
#library(devtools)
#install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
library(pairwiseAdonis)
pairwise.adonis(combined_subset2[,c("phyllary_length", "phyllary_width", "primary_axis_leaf_length", "primary_axis_leaf_width", "secondary_axis_leaf_length", "secondary_axis_leaf_width", "bract_length", "bract_width", "auricle_length", "primary_axis_hair_length", "below_capitulum_hair_length",  "ray_length", "ray_width")], combined_subset2$species, sim.method = "euclidean", p.adjust.m = "hochberg", perm = 10000)


# Univariate boxplots
p1 <- ggplot(combined_subset2, aes(y=phyllary_length, x=species)) + geom_boxplot() + xlab("") + ylab("phyllary_length (mm)") + theme(axis.text.x = element_text(angle = 45))
p2 <- ggplot(combined_subset2, aes(y=phyllary_width, x=species)) + geom_boxplot() + xlab("") + ylab("phyllary_width (mm)") + theme(axis.text.x = element_text(angle = 45))
p3 <- ggplot(combined_subset2, aes(y=primary_axis_leaf_length, x=species)) + geom_boxplot() + xlab("") + ylab("primary_axis_leaf_length (mm)") + theme(axis.text.x = element_text(angle = 45))
p4 <- ggplot(combined_subset2, aes(y=primary_axis_leaf_width * 10, x=species)) + geom_boxplot() + xlab("") + ylab("primary_axis_leaf_width (mm)") + theme(axis.text.x = element_text(angle = 45))
p5 <- ggplot(combined_subset2, aes(y=secondary_axis_leaf_length * 10, x=species)) + geom_boxplot() + xlab("") + ylab("secondary_axis_leaf_length (mm)") + theme(axis.text.x = element_text(angle = 45))
p6 <- ggplot(combined_subset2, aes(y=secondary_axis_leaf_width, x=species)) + geom_boxplot() + xlab("") + ylab("secondary_axis_leaf_width (mm)") + theme(axis.text.x = element_text(angle = 45))
p7 <- ggplot(combined_subset2, aes(y=bract_length * 10, x=species)) + geom_boxplot() + xlab("") + ylab("bract_length (mm)") + theme(axis.text.x = element_text(angle = 45)) # Check the units
p8 <- ggplot(combined_subset2, aes(y=bract_width * 10, x=species)) + geom_boxplot() + xlab("") + ylab("bract_width (mm)") + theme(axis.text.x = element_text(angle = 45)) # Check the units
p9 <- ggplot(combined_subset2, aes(y=auricle_length * 10, x=species)) + geom_boxplot() + xlab("") + ylab("auricle_length (mm)") + theme(axis.text.x = element_text(angle = 45)) # Check the units
p10 <- ggplot(combined_subset2, aes(y=petiole_length * 10, x=species)) + geom_boxplot() + xlab("") + ylab("petiole_length (mm)") + theme(axis.text.x = element_text(angle = 45)) # Check the units
p11 <- ggplot(combined_subset2, aes(y=primary_axis_hair_length * 10, x=species)) + geom_boxplot() + xlab("") + ylab("primary_axis_hair_length (mm)") + theme(axis.text.x = element_text(angle = 45)) # Check the units
p12 <- ggplot(combined_subset2, aes(y=below_capitulum_hair_length * 10, x=species)) + geom_boxplot() + xlab("") + ylab("below_capitulum_hair_length (mm)") + theme(axis.text.x = element_text(angle = 45)) # Check the units
p13 <- ggplot(combined_subset2, aes(y=ray_length * 10, x=species)) + geom_boxplot() + xlab("") + ylab("ray_length (mm)") + theme(axis.text.x = element_text(angle = 45)) # Check the units
p14 <- ggplot(combined_subset2, aes(y=ray_width * 10, x=species)) + geom_boxplot() + xlab("") + ylab("ray_width (mm)") + theme(axis.text.x = element_text(angle = 45)) # Check the units

library(gridExtra)
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, nrow = 2)

#p9 and p10 have null values so exclude those

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p11, p12, p13, p14, nrow = 2)


#################################
## section analysis-- grandiflori
#################################
section_grandiflori<- c("Symphyotrichum campestre", "Symphyotrichum fendleri", "Symphyotrichum fontinale", "Symphyotrichum estesii", "Symphyotrichum grandiflorum", "Symphyotrichum oblongifolium", "Symphyotrichum pygmaeum", "Symphyotrichum yukonense", "Symphyotrichum gypsophilum", "Symphyotrichum hintonii", "Symphyotrichum moranense", "Symphyotrichum trilineatum", "Symphyotrichum purpurescens", "Symphyotrichum turneri", "Symphyotrichum bimater")

combined_subset3 <- combined[combined$species %in% section_grandiflori,]
combined_subset3$species <- as.factor(as.character(combined_subset3$species))
combined_subset3$species = factor(combined_subset3$species, labels = gsub("Symphyotrichum ", "S.", levels(combined_subset3$species)))

# Build the discriminant
library(MASS)
discriminant3 <- lda(species ~ phyllary_length + phyllary_width + primary_axis_leaf_length + primary_axis_leaf_width + secondary_axis_leaf_length + secondary_axis_leaf_width + bract_length + bract_width + auricle_length + primary_axis_hair_length + below_capitulum_hair_length + ray_length + ray_width, data = combined_subset3, na.action="na.omit")

# Classification success
discriminant.jackknife3 <- lda(species ~ phyllary_length + phyllary_width + primary_axis_leaf_length + primary_axis_leaf_width + secondary_axis_leaf_length + secondary_axis_leaf_width + bract_length + bract_width + auricle_length + primary_axis_hair_length + below_capitulum_hair_length + ray_length + ray_width, data = combined_subset3, na.action="na.omit", CV = TRUE)
ct3 <- table(combined_subset3$species, discriminant.jackknife3$class)
sum(diag(prop.table(ct3)))

# Predict species by the discriminant function
discriminant.prediction3 <- predict(discriminant3)

# Create dataframe for plotting
plotdata3 <- data.frame(type = combined_subset3$species, lda = discriminant.prediction3$x)

library(ggplot2)
ggplot(plotdata3) + geom_boxplot(aes(lda.LD1, lda.LD2, colour = type))
ggplot(plotdata3) + geom_point(aes(lda.LD1, lda.LD2, colour = type), size = 2.5)

# Multivariate MANOVA
res.man <- manova(cbind(phyllary_length, phyllary_width, primary_axis_leaf_length, primary_axis_leaf_width, secondary_axis_leaf_length, secondary_axis_leaf_width, bract_length, bract_width, auricle_length, primary_axis_hair_length, below_capitulum_hair_length, ray_length, ray_width) ~ species, data = combined_subset3)
summary(res.man)

# Break down variable importance
summary.aov(res.man)

# Assess SPECIES pairwise significance 
# You must drop perfectly correlated values or you will get a rank deficiency error
#library(devtools)
#install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
library(pairwiseAdonis)
pairwise.adonis(combined_subset3[,c("phyllary_length", "phyllary_width", "primary_axis_leaf_length", "primary_axis_leaf_width", "secondary_axis_leaf_length", "secondary_axis_leaf_width", "bract_length", "bract_width", "auricle_length", "primary_axis_hair_length", "below_capitulum_hair_length",  "ray_length", "ray_width")], combined_subset3$species, sim.method = "euclidean", p.adjust.m = "hochberg", perm = 10000)

# Univariate boxplots
p1 <- ggplot(combined_subset3, aes(y=phyllary_length, x=species)) + geom_boxplot() + xlab("") + ylab("phyllary_length (mm)") + theme(axis.text.x = element_text(angle = 45))
p2 <- ggplot(combined_subset3, aes(y=phyllary_width, x=species)) + geom_boxplot() + xlab("") + ylab("phyllary_width (mm)") + theme(axis.text.x = element_text(angle = 45))
p3 <- ggplot(combined_subset3, aes(y=primary_axis_leaf_length, x=species)) + geom_boxplot() + xlab("") + ylab("primary_axis_leaf_length (mm)") + theme(axis.text.x = element_text(angle = 45))
p4 <- ggplot(combined_subset3, aes(y=primary_axis_leaf_width * 10, x=species)) + geom_boxplot() + xlab("") + ylab("primary_axis_leaf_width (mm)") + theme(axis.text.x = element_text(angle = 45))
p5 <- ggplot(combined_subset3, aes(y=secondary_axis_leaf_length * 10, x=species)) + geom_boxplot() + xlab("") + ylab("secondary_axis_leaf_length (mm)") + theme(axis.text.x = element_text(angle = 45))
p6 <- ggplot(combined_subset3, aes(y=secondary_axis_leaf_width, x=species)) + geom_boxplot() + xlab("") + ylab("secondary_axis_leaf_width (mm)") + theme(axis.text.x = element_text(angle = 45))
p7 <- ggplot(combined_subset3, aes(y=bract_length * 10, x=species)) + geom_boxplot() + xlab("") + ylab("bract_length (mm)") + theme(axis.text.x = element_text(angle = 45)) # Check the units
p8 <- ggplot(combined_subset3, aes(y=bract_width * 10, x=species)) + geom_boxplot() + xlab("") + ylab("bract_width (mm)") + theme(axis.text.x = element_text(angle = 45)) # Check the units
p9 <- ggplot(combined_subset3, aes(y=auricle_length * 10, x=species)) + geom_boxplot() + xlab("") + ylab("auricle_length (mm)") + theme(axis.text.x = element_text(angle = 45)) # Check the units
p10 <- ggplot(combined_subset3, aes(y=petiole_length * 10, x=species)) + geom_boxplot() + xlab("") + ylab("petiole_length (mm)") + theme(axis.text.x = element_text(angle = 45)) # Check the units
p11 <- ggplot(combined_subset3, aes(y=primary_axis_hair_length * 10, x=species)) + geom_boxplot() + xlab("") + ylab("primary_axis_hair_length (mm)") + theme(axis.text.x = element_text(angle = 45)) # Check the units
p12 <- ggplot(combined_subset3, aes(y=below_capitulum_hair_length * 10, x=species)) + geom_boxplot() + xlab("") + ylab("below_capitulum_hair_length (mm)") + theme(axis.text.x = element_text(angle = 45)) # Check the units
p13 <- ggplot(combined_subset3, aes(y=ray_length * 10, x=species)) + geom_boxplot() + xlab("") + ylab("ray_length (mm)") + theme(axis.text.x = element_text(angle = 45)) # Check the units
p14 <- ggplot(combined_subset3, aes(y=ray_width * 10, x=species)) + geom_boxplot() + xlab("") + ylab("ray_width (mm)") + theme(axis.text.x = element_text(angle = 45)) # Check the units

library(gridExtra)
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, nrow = 2)

#p9 and p10 have null values so exclude those

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p11, p12, p13, p14, nrow = 2)


#################################
## section analysis-- patentes
#################################
section_patentes <- c("Symphyotrichum georgianum", "Symphyotrichum patens", "Symphyotrichum phlogifolium", "Symphyotrichum adnatum", "Symphyotrichum walteri")

combined_subset4 <- combined[combined$species %in% section_patentes,]
combined_subset4$species <- as.factor(as.character(combined_subset4$species))
combined_subset4$species = factor(combined_subset4$species, labels = gsub("Symphyotrichum ", "S.", levels(combined_subset4$species)))

# Build the discriminant
library(MASS)
discriminant4 <- lda(species ~ phyllary_length + phyllary_width + primary_axis_leaf_length + primary_axis_leaf_width + secondary_axis_leaf_length + secondary_axis_leaf_width + bract_length + bract_width + primary_axis_hair_length + below_capitulum_hair_length + ray_length + ray_width, data = combined_subset4, na.action="na.omit")

# Classification success
discriminant.jackknife4 <- lda(species ~ phyllary_length + phyllary_width + primary_axis_leaf_length + primary_axis_leaf_width + secondary_axis_leaf_length + secondary_axis_leaf_width + bract_length + bract_width + primary_axis_hair_length + below_capitulum_hair_length + ray_length + ray_width, data = combined_subset4, na.action="na.omit", CV = TRUE)
ct4 <- table(combined_subset4$species, discriminant.jackknife4$class)
sum(diag(prop.table(ct4)))

# Predict species by the discriminant function
discriminant.prediction4 <- predict(discriminant4)

# Create dataframe for plotting
plotdata4 <- data.frame(type = combined_subset4$species, lda = discriminant.prediction4$x)

library(ggplot2)
ggplot(plotdata4) + geom_boxplot(aes(lda.LD1, lda.LD2, colour = type))
ggplot(plotdata4) + geom_point(aes(lda.LD1, lda.LD2, colour = type), size = 2.5)

# Multivariate MANOVA
res.man <- manova(cbind(phyllary_length, phyllary_width, primary_axis_leaf_length, primary_axis_leaf_width, secondary_axis_leaf_length, secondary_axis_leaf_width, bract_length, bract_width, primary_axis_hair_length, below_capitulum_hair_length, ray_length, ray_width) ~ species, data = combined_subset4)
summary(res.man)

# Break down variable importance
summary.aov(res.man)

# Assess SPECIES pairwise significance 
# You must drop perfectly correlated values or you will get a rank deficiency error
#library(devtools)
#install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
library(pairwiseAdonis)
pairwise.adonis(combined_subset4[,c("phyllary_length", "phyllary_width", "primary_axis_leaf_length", "primary_axis_leaf_width", "secondary_axis_leaf_length", "secondary_axis_leaf_width", "bract_length", "bract_width", "auricle_length", "primary_axis_hair_length", "below_capitulum_hair_length",  "ray_length", "ray_width")], combined_subset4$species, sim.method = "euclidean", p.adjust.m = "hochberg", perm = 10000)

# Univariate boxplots
p1 <- ggplot(combined_subset4, aes(y=phyllary_length, x=species)) + geom_boxplot() + xlab("") + ylab("phyllary_length (mm)") + theme(axis.text.x = element_text(angle = 45))
p2 <- ggplot(combined_subset4, aes(y=phyllary_width, x=species)) + geom_boxplot() + xlab("") + ylab("phyllary_width (mm)") + theme(axis.text.x = element_text(angle = 45))
p3 <- ggplot(combined_subset4, aes(y=primary_axis_leaf_length, x=species)) + geom_boxplot() + xlab("") + ylab("primary_axis_leaf_length (mm)") + theme(axis.text.x = element_text(angle = 45))
p4 <- ggplot(combined_subset4, aes(y=primary_axis_leaf_width * 10, x=species)) + geom_boxplot() + xlab("") + ylab("primary_axis_leaf_width (mm)") + theme(axis.text.x = element_text(angle = 45))
p5 <- ggplot(combined_subset4, aes(y=secondary_axis_leaf_length * 10, x=species)) + geom_boxplot() + xlab("") + ylab("secondary_axis_leaf_length (mm)") + theme(axis.text.x = element_text(angle = 45))
p6 <- ggplot(combined_subset4, aes(y=secondary_axis_leaf_width, x=species)) + geom_boxplot() + xlab("") + ylab("secondary_axis_leaf_width (mm)") + theme(axis.text.x = element_text(angle = 45))
p7 <- ggplot(combined_subset4, aes(y=bract_length * 10, x=species)) + geom_boxplot() + xlab("") + ylab("bract_length (mm)") + theme(axis.text.x = element_text(angle = 45)) # Check the units
p8 <- ggplot(combined_subset4, aes(y=bract_width * 10, x=species)) + geom_boxplot() + xlab("") + ylab("bract_width (mm)") + theme(axis.text.x = element_text(angle = 45)) # Check the units
p9 <- ggplot(combined_subset4, aes(y=auricle_length * 10, x=species)) + geom_boxplot() + xlab("") + ylab("auricle_length (mm)") + theme(axis.text.x = element_text(angle = 45)) # Check the units
p10 <- ggplot(combined_subset4, aes(y=petiole_length * 10, x=species)) + geom_boxplot() + xlab("") + ylab("petiole_length (mm)") + theme(axis.text.x = element_text(angle = 45)) # Check the units
p11 <- ggplot(combined_subset4, aes(y=primary_axis_hair_length * 10, x=species)) + geom_boxplot() + xlab("") + ylab("primary_axis_hair_length (mm)") + theme(axis.text.x = element_text(angle = 45)) # Check the units
p12 <- ggplot(combined_subset4, aes(y=below_capitulum_hair_length * 10, x=species)) + geom_boxplot() + xlab("") + ylab("below_capitulum_hair_length (mm)") + theme(axis.text.x = element_text(angle = 45)) # Check the units
p13 <- ggplot(combined_subset4, aes(y=ray_length * 10, x=species)) + geom_boxplot() + xlab("") + ylab("ray_length (mm)") + theme(axis.text.x = element_text(angle = 45)) # Check the units
p14 <- ggplot(combined_subset4, aes(y=ray_width * 10, x=species)) + geom_boxplot() + xlab("") + ylab("ray_width (mm)") + theme(axis.text.x = element_text(angle = 45)) # Check the units

library(gridExtra)
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, nrow = 2)

#p9 and p10 have null values so exclude those

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p11, p12, p13, p14, nrow = 2)

#################################
## subset analysis-- polyliguli & ericoidei
#################################
section_polyliguli_ericoidei <- c("Symphyotrichum novae-angliae", "Symphyotrichum ericoides", "Symphyotrichum amethystinum", "Symphyotrichum falcatum")

combined_subset5 <- combined[combined$species %in% section_polyliguli_ericoidei,]
combined_subset5$species <- as.factor(as.character(combined_subset5$species))
combined_subset5$species = factor(combined_subset5$species, labels = gsub("Symphyotrichum ", "S.", levels(combined_subset5$species)))

# Build the discriminant
library(MASS)
discriminant <- lda(species ~ phyllary_length + phyllary_width + primary_axis_leaf_length + primary_axis_leaf_width + secondary_axis_leaf_length + secondary_axis_leaf_width + bract_length + bract_width + primary_axis_hair_length + below_capitulum_hair_length + ray_length + ray_width, data = combined_subset5, na.action="na.omit")

# Classification success
discriminant.jackknife <- lda(species ~ phyllary_length + phyllary_width + primary_axis_leaf_length + primary_axis_leaf_width + secondary_axis_leaf_length + secondary_axis_leaf_width + bract_length + bract_width + primary_axis_hair_length + below_capitulum_hair_length + ray_length + ray_width, data = combined_subset5, na.action="na.omit", CV = TRUE)
ct5 <- table(combined_subset5$species, discriminant.jackknife$class)
sum(diag(prop.table(ct5)))

# Predict species by the discriminant function
discriminant.prediction <- predict(discriminant)

# Create dataframe for plotting
plotdata5 <- data.frame(type = combined_subset5$species, lda = discriminant.prediction$x)

library(ggplot2)
ggplot(plotdata5) + geom_boxplot(aes(lda.LD1, lda.LD2, colour = type))
ggplot(plotdata5) + geom_point(aes(lda.LD1, lda.LD2, colour = type), size = 2.5)

# Multivariate MANOVA
res.man <- manova(cbind(phyllary_length, phyllary_width, primary_axis_leaf_length, primary_axis_leaf_width, secondary_axis_leaf_length, secondary_axis_leaf_width, bract_length, bract_width, primary_axis_hair_length, below_capitulum_hair_length, ray_length, ray_width) ~ species, data = combined_subset5)
summary(res.man)

# Break down variable importance
summary.aov(res.man)

# Assess SPECIES pairwise significance 
# You must drop perfectly correlated values or you will get a rank deficiency error
#library(devtools)
#install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
library(pairwiseAdonis)
pairwise.adonis(combined_subset5[,c("phyllary_length", "phyllary_width", "primary_axis_leaf_length", "primary_axis_leaf_width", "secondary_axis_leaf_length", "secondary_axis_leaf_width", "bract_length", "bract_width", "auricle_length", "primary_axis_hair_length", "below_capitulum_hair_length",  "ray_length", "ray_width")], combined_subset5$species, sim.method = "euclidean", p.adjust.m = "hochberg", perm = 10000)

# Univariate boxplots
p1 <- ggplot(combined_subset5, aes(y=phyllary_length, x=species)) + geom_boxplot() + xlab("") + ylab("phyllary_length (mm)") + theme(axis.text.x = element_text(angle = 45))
p2 <- ggplot(combined_subset5, aes(y=phyllary_width, x=species)) + geom_boxplot() + xlab("") + ylab("phyllary_width (mm)") + theme(axis.text.x = element_text(angle = 45))
p3 <- ggplot(combined_subset5, aes(y=primary_axis_leaf_length, x=species)) + geom_boxplot() + xlab("") + ylab("primary_axis_leaf_length (mm)") + theme(axis.text.x = element_text(angle = 45))
p4 <- ggplot(combined_subset5, aes(y=primary_axis_leaf_width * 10, x=species)) + geom_boxplot() + xlab("") + ylab("primary_axis_leaf_width (mm)") + theme(axis.text.x = element_text(angle = 45))
p5 <- ggplot(combined_subset5, aes(y=secondary_axis_leaf_length * 10, x=species)) + geom_boxplot() + xlab("") + ylab("secondary_axis_leaf_length (mm)") + theme(axis.text.x = element_text(angle = 45))
p6 <- ggplot(combined_subset5, aes(y=secondary_axis_leaf_width, x=species)) + geom_boxplot() + xlab("") + ylab("secondary_axis_leaf_width (mm)") + theme(axis.text.x = element_text(angle = 45))
p7 <- ggplot(combined_subset5, aes(y=bract_length * 10, x=species)) + geom_boxplot() + xlab("") + ylab("bract_length (mm)") + theme(axis.text.x = element_text(angle = 45)) # Check the units
p8 <- ggplot(combined_subset5, aes(y=bract_width * 10, x=species)) + geom_boxplot() + xlab("") + ylab("bract_width (mm)") + theme(axis.text.x = element_text(angle = 45)) # Check the units
p9 <- ggplot(combined_subset5, aes(y=auricle_length * 10, x=species)) + geom_boxplot() + xlab("") + ylab("auricle_length (mm)") + theme(axis.text.x = element_text(angle = 45)) # Check the units
p10 <- ggplot(combined_subset5, aes(y=petiole_length * 10, x=species)) + geom_boxplot() + xlab("") + ylab("petiole_length (mm)") + theme(axis.text.x = element_text(angle = 45)) # Check the units
p11 <- ggplot(combined_subset5, aes(y=primary_axis_hair_length * 10, x=species)) + geom_boxplot() + xlab("") + ylab("primary_axis_hair_length (mm)") + theme(axis.text.x = element_text(angle = 45)) # Check the units
p12 <- ggplot(combined_subset5, aes(y=below_capitulum_hair_length * 10, x=species)) + geom_boxplot() + xlab("") + ylab("below_capitulum_hair_length (mm)") + theme(axis.text.x = element_text(angle = 45)) # Check the units
p13 <- ggplot(combined_subset5, aes(y=ray_length * 10, x=species)) + geom_boxplot() + xlab("") + ylab("ray_length (mm)") + theme(axis.text.x = element_text(angle = 45)) # Check the units
p14 <- ggplot(combined_subset5, aes(y=ray_width * 10, x=species)) + geom_boxplot() + xlab("") + ylab("ray_width (mm)") + theme(axis.text.x = element_text(angle = 45)) # Check the units

library(gridExtra)
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, nrow = 2)

#p9, p10, p11, p13 have null values so exclude those

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p11, p12, p13, p14, nrow = 2)

