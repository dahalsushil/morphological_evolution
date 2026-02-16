library(phytools)
library(geiger)
library(ggplot2)

classification=read.table("final_classification_k_6.csv", header = FALSE, sep = ",")
rownames(classification) <- classification[,1]
classification$V1<- NULL

environment=read.table("medians_data.csv", header = FALSE, sep = ",")
rownames(environment) <- environment[,1]
environment$V1<- NULL

combined <- merge(classification, environment, by="row.names", all=TRUE)
combined <- na.omit(combined)

combined[,2] <- as.factor(as.numeric(combined[,2]))
names(combined) <- c("species", "habitat", "aridity", "BIO1", "BIO10", "BIO11", "BIO12", "BIO13", "BIO14", "BIO15", "BIO16", "BIO17", "BIO18", "BIO19", "BIO2", "BIO3", "BIO4", "BIO5", "BIO6", "BIO7", "BIO8", "BIO9", "aspect", "elevation", "slope", "bulkdensity", "claypercent", "coarsefragmentpercent", "nitrogen", "phx10percent", "sandpercent", "siltpercent", "carboncontent", "needleleaf", "evergreeenbroadleaf", "deciduousbroadleaf", "mixedtrees", "shrubs", "herbaceous")
rownames(combined) <- combined[,1]

ag <- aggregate(. ~ habitat, combined, function(x) c(mean = mean(x), sd = sd(x)))
#write.csv(ag, file = "mean_stdev.csv")

#################################
## subgenus analysis-- Virgulus
#################################
subgenus_virgulus <- c("Symphyotrichum_grandiflorum", "Symphyotrichum_bimater", "Symphyotrichum_concolor", "Symphyotrichum_pratense", "Symphyotrichum_oblongifolium", "Symphyotrichum_phlogifolium", "Symphyotrichum_georgianum", "Symphyotrichum_yukonense", "Symphyotrichum_hintonii", "Symphyotrichum_moranense", "Symphyotrichum_patens", "Symphyotrichum_adnatum", "Symphyotrichum_walteri", "Symphyotrichum_novae-angliae", "Symphyotrichum_ericoides", "Symphyotrichum_falcatum", "Symphyotrichum_turneri", "Symphyotrichum_sericeum", "Symphyotrichum_fendleri", "Symphyotrichum_amethystinum", "Symphyotrichum_chihuahuense", "Symphyotrichum_purpurascens")
combined_subset <- combined[combined$species %in% subgenus_virgulus,]
combined_subset$species <- as.factor(as.character(combined_subset$species))
#write.csv(combined_subset, file = "virgulus_habitat_env.csv")
#save .csv file that has habitat and environmental variables information
#write.csv(combined, file = "habitat_env_variable.csv")
#load saved data with habitat in place of species
combined <- read.table("virgulus_habitat_env.csv", header = TRUE, sep = ",")
combined_subset$habitat <- as.factor(as.character(combined_subset$habitat))
str(combined)

# Build the discriminant
library(MASS)
discriminant <- lda(habitat ~ aridity + BIO1 + BIO10 + BIO11 + BIO12 + BIO13 + BIO14 + BIO15 + BIO16 + BIO17 + BIO18 + BIO19 + BIO2 + BIO3 + BIO4 + BIO5 + BIO6 + BIO7 + BIO8 + BIO9 + aspect + elevation + slope + bulkdensity + claypercent + coarsefragmentpercent + nitrogen + phx10percent + sandpercent + siltpercent + carboncontent + needleleaf + evergreeenbroadleaf + deciduousbroadleaf + mixedtrees + shrubs + herbaceous, data = combined, na.action="na.omit")
head(discriminant)

# Classification success
discriminant.jackknife <- lda(habitat ~ aridity + BIO1 + BIO10 + BIO11 + BIO12 + BIO13 + BIO14 + BIO15 + BIO16 + BIO17 + BIO18 + BIO19 + BIO2 + BIO3 + BIO4 + BIO5 + BIO6 + BIO7 + BIO8 + BIO9 + aspect + elevation + slope + bulkdensity + claypercent + coarsefragmentpercent + nitrogen + phx10percent + sandpercent + siltpercent + carboncontent + needleleaf + evergreeenbroadleaf + deciduousbroadleaf + mixedtrees + shrubs + herbaceous, data = combined, na.action="na.omit", CV = TRUE)
ct1 <- table(combined$habitat, discriminant.jackknife$class)
sum(diag(prop.table(ct1)))

# Predict species by the discriminant function
discriminant.prediction <- predict(discriminant)

# Create dataframe for plotting
plotdata1 <- data.frame(type = combined$habitat, lda = discriminant.prediction$x)

##If you want to save lda values for testing phylogenetic signal (lda1 is probably the best to use)
##write.csv(plotdata1, "plotdata1.csv")

library(ggplot2)
ggplot(plotdata1) + geom_boxplot(aes(lda.LD1, lda.LD2, colour = type))
plot<- ggplot(plotdata1) + geom_point(aes(lda.LD1, lda.LD2, colour = type), size = 12)

ggsave("habitat_cluster_plot.pdf", plot = plot, width = 12, height = 8)


# Multivariate MANOVA
res.man <- manova(cbind(aridity, BIO1, BIO10, BIO11, BIO12, BIO13, BIO14, BIO15, BIO16, BIO17, BIO18, BIO19, BIO2, BIO3, BIO4, BIO5, BIO6, BIO7, BIO8, BIO9, aspect, elevation, slope, bulkdensity, claypercent, coarsefragmentpercent, nitrogen, phx10percent, sandpercent, siltpercent, carboncontent, needleleaf, evergreeenbroadleaf, deciduousbroadleaf, mixedtrees, shrubs, herbaceous) ~ habitat, data = combined)
summary(res.man)

# Assess SPECIES pairwise significance 
# You must drop perfectly correlated values or you will get a rank deficiency error
#library(devtools)
#install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
pairwiseAdonis <- pairwise.adonis(combined[,c("aridity", "BIO1", "BIO10", "BIO11", "BIO12", "BIO13", "BIO14", "BIO15", "BIO16", "BIO17", "BIO18", "BIO19", "BIO2", "BIO3", "BIO4", "BIO5", "BIO6", "BIO7", "BIO8", "BIO9", "aspect", "elevation", "slope", "bulkdensity", "claypercent", "coarsefragmentpercent", "nitrogen", "phx10percent", "sandpercent", "siltpercent", "carboncontent", "needleleaf", "evergreeenbroadleaf", "deciduousbroadleaf", "mixedtrees", "shrubs", "herbaceous")], combined$habitat, sim.method = "euclidean", p.adjust.m = "hochberg", perm = 10000)

write.csv(pairwiseAdonis, file = "habitat_pairwiseAdonis.csv")

# Break down variable importance
summary.aov(res.man)

