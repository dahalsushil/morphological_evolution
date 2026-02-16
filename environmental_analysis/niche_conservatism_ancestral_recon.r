library(ape)
library(phytools)
library(geiger)
library(dplyr)
library(ggplot2)


###########
# Combined data file
bio1 <- read.csv("./../environmental_data/BIOCLIM_1.average.csv", header = FALSE, stringsAsFactors = FALSE, row.names = NULL, sep = "\t")
colnames(bio1) <- c("species", "bio1")
bio1 <- distinct(bio1, species, .keep_all= TRUE)

bio2 <- read.table("./../environmental_data/BIOCLIM_3.average.csv", header = FALSE, stringsAsFactors = FALSE, row.names = NULL, sep = "\t")
colnames(bio2) <- c("species", "bio2")
bio2 <- distinct(bio2, species, .keep_all= TRUE)

bio3 <- read.table("./../environmental_data/BIOCLIM_3.average.csv", header = FALSE, stringsAsFactors = FALSE, row.names = NULL, sep = "\t")
colnames(bio3) <- c("species", "bio3")
bio3 <- distinct(bio3, species, .keep_all= TRUE)

bio4 <- read.table("./../environmental_data/BIOCLIM_4.average.csv", header = FALSE, stringsAsFactors = FALSE, row.names = NULL, sep = "\t")
colnames(bio4) <- c("species", "bio4")
bio4 <- distinct(bio4, species, .keep_all= TRUE)

bio5 <- read.table("./../environmental_data/BIOCLIM_5.average.csv", header = FALSE, stringsAsFactors = FALSE, row.names = NULL, sep = "\t")
colnames(bio5) <- c("species", "bio5")
bio5 <- distinct(bio5, species, .keep_all= TRUE)

bio6 <- read.table("./../environmental_data/BIOCLIM_6.average.csv", header = FALSE, stringsAsFactors = FALSE, row.names = NULL, sep = "\t")
colnames(bio6) <- c("species", "bio6")
bio6 <- distinct(bio6, species, .keep_all= TRUE)

bio7 <- read.table("./../environmental_data/BIOCLIM_7.average.csv", header = FALSE, stringsAsFactors = FALSE, row.names = NULL, sep = "\t")
colnames(bio7) <- c("species", "bio7")
bio7 <- distinct(bio7, species, .keep_all= TRUE)

bio8 <- read.table("./../environmental_data/BIOCLIM_8.average.csv", header = FALSE, stringsAsFactors = FALSE, row.names = NULL, sep = "\t")
colnames(bio8) <- c("species", "bio8")
bio8 <- distinct(bio8, species, .keep_all= TRUE)

bio9 <- read.table("./../environmental_data/BIOCLIM_9.average.csv", header = FALSE, stringsAsFactors = FALSE, row.names = NULL, sep = "\t")
colnames(bio9) <- c("species", "bio9")
bio9 <- distinct(bio9, species, .keep_all= TRUE)

bio10 <- read.table("./../environmental_data/BIOCLIM_10.average.csv", header = FALSE, stringsAsFactors = FALSE, row.names = NULL, sep = "\t")
colnames(bio10) <- c("species", "bio10")
bio10 <- distinct(bio10, species, .keep_all= TRUE)

bio11 <- read.table("./../environmental_data/BIOCLIM_11.average.csv", header = FALSE, stringsAsFactors = FALSE, row.names = NULL, sep = "\t")
colnames(bio11) <- c("species", "bio11")
bio11 <- distinct(bio11, species, .keep_all= TRUE)

bio12 <- read.table("./../environmental_data/BIOCLIM_12.average.csv", header = FALSE, stringsAsFactors = FALSE, row.names = NULL, sep = "\t")
colnames(bio12) <- c("species", "bio12")
bio12 <- distinct(bio12, species, .keep_all= TRUE)

bio13 <- read.table("./../environmental_data/BIOCLIM_13.average.csv", header = FALSE, stringsAsFactors = FALSE, row.names = NULL, sep = "\t")
colnames(bio13) <- c("species", "bio13")
bio13 <- distinct(bio13, species, .keep_all= TRUE)

bio14 <- read.table("./../environmental_data/BIOCLIM_14.average.csv", header = FALSE, stringsAsFactors = FALSE, row.names = NULL, sep = "\t")
colnames(bio14) <- c("species", "bio14")
bio14 <- distinct(bio14, species, .keep_all= TRUE)

bio15 <- read.table("./../environmental_data/BIOCLIM_15.average.csv", header = FALSE, stringsAsFactors = FALSE, row.names = NULL, sep = "\t")
colnames(bio15) <- c("species", "bio15")
bio15 <- distinct(bio15, species, .keep_all= TRUE)

bio16 <- read.table("./../environmental_data/BIOCLIM_16.average.csv", header = FALSE, stringsAsFactors = FALSE, row.names = NULL, sep = "\t")
colnames(bio16) <- c("species", "bio16")
bio16 <- distinct(bio16, species, .keep_all= TRUE)

bio17 <- read.table("./../environmental_data/BIOCLIM_17.average.csv", header = FALSE, stringsAsFactors = FALSE, row.names = NULL, sep = "\t")
colnames(bio17) <- c("species", "bio17")
bio17 <- distinct(bio17, species, .keep_all= TRUE)

bio18 <- read.table("./../environmental_data/BIOCLIM_18.average.csv", header = FALSE, stringsAsFactors = FALSE, row.names = NULL, sep = "\t")
colnames(bio18) <- c("species", "bio18")
bio18 <- distinct(bio18, species, .keep_all= TRUE)

bio19 <- read.table("./../environmental_data/BIOCLIM_19.average.csv", header = FALSE, stringsAsFactors = FALSE, row.names = NULL, sep = "\t")
colnames(bio19) <- c("species", "bio19")
bio19 <- distinct(bio19, species, .keep_all= TRUE)

elevation <- read.table("./../environmental_data/GTOPO30_ELEVATION.average.csv", header = FALSE, stringsAsFactors = FALSE, row.names = NULL, sep = "\t")
colnames(elevation) <- c("species", "elevation")
elevation <- distinct(elevation, species, .keep_all= TRUE)

aspect <- read.table("./../environmental_data/GTOPO30_ASPECT_reduced.average.csv", header = FALSE, stringsAsFactors = FALSE, row.names = NULL, sep = "\t")
colnames(aspect) <- c("species", "aspect")
aspect <- distinct(aspect, species, .keep_all= TRUE)

slope <- read.table("./../environmental_data/GTOPO30_SLOPE_reduced.average.csv", header = FALSE, stringsAsFactors = FALSE, row.names = NULL, sep = "\t")
colnames(slope) <- c("species", "slope")
slope <- distinct(slope, species, .keep_all= TRUE)

bulkdensity <- read.table("./../environmental_data/ISRICSOILGRIDS_new_average_bulkdensity_reduced.average.csv", header = FALSE, stringsAsFactors = FALSE, row.names = NULL, sep = "\t")
colnames(bulkdensity) <- c("species", "bulkdensity")
bulkdensity <- distinct(bulkdensity, species, .keep_all= TRUE)

clay <- read.table("./../environmental_data/ISRICSOILGRIDS_new_average_claypercent_reduced.average.csv", header = FALSE, stringsAsFactors = FALSE, row.names = NULL, sep = "\t")
colnames(clay) <- c("species", "clay")
clay <- distinct(clay, species, .keep_all= TRUE)

nitrogen <- read.table("./../environmental_data/ISRICSOILGRIDS_new_average_nitrogen_reduced.average.csv", header = FALSE, stringsAsFactors = FALSE, row.names = NULL, sep = "\t")
colnames(nitrogen) <- c("species", "nitrogen")
nitrogen <- distinct(nitrogen, species, .keep_all= TRUE)

carbon <- read.table("./../environmental_data/ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced.average.csv", header = FALSE, stringsAsFactors = FALSE, row.names = NULL, sep = "\t")
colnames(carbon) <- c("species", "carbon")
carbon <- distinct(carbon, species, .keep_all= TRUE)

ph <- read.table("./../environmental_data/ISRICSOILGRIDS_new_average_phx10percent_reduced.average.csv", header = FALSE, stringsAsFactors = FALSE, row.names = NULL, sep = "\t")
colnames(ph) <- c("species", "ph")
ph <- distinct(ph, species, .keep_all= TRUE)

sand <- read.table("./../environmental_data/ISRICSOILGRIDS_new_average_sandpercent_reduced.average.csv", header = FALSE, stringsAsFactors = FALSE, row.names = NULL, sep = "\t")
colnames(sand) <- c("species", "sand")
sand <- distinct(sand, species, .keep_all= TRUE)

silt <- read.table("./../environmental_data/ISRICSOILGRIDS_new_average_siltpercent_reduced.average.csv", header = FALSE, stringsAsFactors = FALSE, row.names = NULL, sep = "\t")
colnames(silt) <- c("species", "silt")
silt <- distinct(silt, species, .keep_all= TRUE)

coarsefragment <- read.table("./../environmental_data/ISRICSOILGRIDS_new_average_coarsefragmentpercent_reduced.average.csv", header = FALSE, stringsAsFactors = FALSE, row.names = NULL, sep = "\t")
colnames(coarsefragment) <- c("species", "coarsefragment")
coarsefragment <- distinct(coarsefragment, species, .keep_all= TRUE)

needleleaf <- read.table("./../environmental_data/LandCover_1_Needleleaf.average.csv", header = FALSE, stringsAsFactors = FALSE, row.names = NULL, sep = "\t")
colnames(needleleaf) <- c("species", "needleleaf")
needleleaf <- distinct(needleleaf, species, .keep_all= TRUE)

evergreenbroadleaf <- read.table("./../environmental_data/LandCover_2_Evergreeenbroadleaf.average.csv", header = FALSE, stringsAsFactors = FALSE, row.names = NULL, sep = "\t")
colnames(evergreenbroadleaf) <- c("species", "evergreenbroadleaf")
evergreenbroadleaf <- distinct(evergreenbroadleaf, species, .keep_all= TRUE)

deciduousbroadleaf <- read.table("./../environmental_data/LandCover_3_Deciduousbroadleaf.average.csv", header = FALSE, stringsAsFactors = FALSE, row.names = NULL, sep = "\t")
colnames(deciduousbroadleaf) <- c("species", "deciduousbroadleaf")
deciduousbroadleaf <- distinct(deciduousbroadleaf, species, .keep_all= TRUE)

mixedtrees <- read.table("./../environmental_data/LandCover_4_Mixedtrees.average.csv", header = FALSE, stringsAsFactors = FALSE, row.names = NULL, sep = "\t")
colnames(mixedtrees) <- c("species", "mixedtrees")
mixedtrees <- distinct(mixedtrees, species, .keep_all= TRUE)

shrubs <- read.table("./../environmental_data/LandCover_5_Shrubs.average.csv", header = FALSE, stringsAsFactors = FALSE, row.names = NULL, sep = "\t")
colnames(shrubs) <- c("species", "shrubs")
shrubs <- distinct(shrubs, species, .keep_all= TRUE)

herbaceous <- read.table("./../environmental_data/LandCover_6_Herbaceous.average.csv", header = FALSE, stringsAsFactors = FALSE, row.names = NULL, sep = "\t")
colnames(herbaceous) <- c("species", "herbaceous")
herbaceous <- distinct(herbaceous, species, .keep_all= TRUE)

aridity <- read.table("./../environmental_data/aridity_index_UNEP.average.csv", header = FALSE, stringsAsFactors = FALSE, row.names = NULL, sep = "\t")
colnames(aridity) <- c("species", "aridity")
aridity <- distinct(aridity, species, .keep_all= TRUE)

biogeo <- read.table("./../biogeobears_subg_virgulus_fixed.tsv", header = FALSE)
colnames(biogeo) <- c("species", "biogeo")
biogeo <- distinct(biogeo, species, .keep_all= TRUE)

soil <- read.table("./../environment_data_soiltype_mode/soiltype_mostprobable.mode.renamed.csv", header = FALSE)
colnames(soil) <- c("species", "soil")
soil <- distinct(soil, species, .keep_all= TRUE)

combined = merge(bio1, bio2, by = "species")
combined = merge(combined, bio3, by = "species")
combined = merge(combined, bio4, by = "species")
combined = merge(combined, bio5, by = "species")
combined = merge(combined, bio6, by = "species")
combined = merge(combined, bio7, by = "species")
combined = merge(combined, bio8, by = "species")
combined = merge(combined, bio9, by = "species")
combined = merge(combined, bio10, by = "species")
combined = merge(combined, bio11, by = "species")
combined = merge(combined, bio12, by = "species")
combined = merge(combined, bio13, by = "species")
combined = merge(combined, bio14, by = "species")
combined = merge(combined, bio15, by = "species")
combined = merge(combined, bio16, by = "species")
combined = merge(combined, bio17, by = "species")
combined = merge(combined, bio18, by = "species")
combined = merge(combined, bio19, by = "species")
combined = merge(combined, elevation, by = "species")
combined = merge(combined, aspect, by = "species")
combined = merge(combined, slope, by = "species")
combined = merge(combined, bulkdensity, by = "species")
combined = merge(combined, clay, by = "species")
combined = merge(combined, nitrogen, by = "species")
combined = merge(combined, carbon, by = "species")
combined = merge(combined, ph, by = "species")
combined = merge(combined, sand, by = "species")
combined = merge(combined, silt, by = "species")
combined = merge(combined, coarsefragment, by = "species")
combined = merge(combined, needleleaf, by = "species")
combined = merge(combined, evergreenbroadleaf, by = "species")
combined = merge(combined, deciduousbroadleaf, by = "species")
combined = merge(combined, mixedtrees, by = "species")
combined = merge(combined, shrubs, by = "species")
combined = merge(combined, herbaceous, by = "species")
combined = merge(combined, aridity, by = "species")
combined = merge(combined, biogeo, by = "species")
combined = merge(combined, soil, by = "species")

#save combined file
#write.csv(combined, "./../combined.csv")


##################################
# Ancestral reconstruction
##################################
#load combined environmental data
combined = read.csv("combined.csv")
combined.reduced <- distinct(combined, species, .keep_all= TRUE)
summary(combined.reduced)

#load tree data
tree = read.tree("pruned_tree.tre")
is.ultrametric(tree)
tree = force.ultrametric(tree, method = "extend")
tree <- ladderize(tree)

#bio1
trait.vector = combined.reduced$bio1
names(trait.vector) <- combined.reduced$species
trait.reduced <- treedata(tree, trait.vector)$data
trait.reduced.vector <- trait.reduced[,1]
tree.reduced <- treedata(tree, trait.vector)$phy

# MODEL COMPARISON
fitBM = fitContinuous(tree.reduced, trait.reduced)
fitOU = fitContinuous(tree.reduced, trait.reduced, model = "OU") # Make sure it is truly ultrametric even considering small rounding errors -- even a small discrepancy will cause complaints and longer runtime due to VCF optimization.
fitEB = fitContinuous(tree.reduced, trait.reduced, model = "EB")

fitBM$opt$aicc
fitOU$opt$aicc
fitEB$opt$aicc

# BM favored for annual temp, precipitation, aridity, etc.

# Plot mean annual temperature
figure = contMap(tree.reduced, trait.reduced.vector, plot = FALSE, model = "BM")
plot(setMap(figure, invert = TRUE), legend=4*max(nodeHeights(tree)), fsize=0.6, ftype="i", lwd=c(3,1), lwd=c(3,1), outline = FALSE)

# Plot annual precipitation, elevation, pH, nitrogen, aridity for color inversion (for bio15 temp seasonality use above)
figure = contMap(tree.reduced, trait.reduced.vector, plot = FALSE, model = "OU")
plot(figure, legend=4*max(nodeHeights(tree)), fsize=0.6, ftype="i", lwd=c(3,1), lwd=c(3,1), outline = FALSE)

# Custom colors
# figure = contMap(tree.reduced, trait.reduced.vector, plot = FALSE, model = "OU")
# figure$cols[1:length(figure$cols)] <- colorRampPalette(c("blue", "red"), space="Lab")(length(figure$cols))
# dev.new(width=6, height=6)
# plot(figure, type = "fan", legend=0.7*max(nodeHeights(tree)), fsize=0.07, ftype="i", lwd=c(0.7,1), lwd=c(0.7,1), outline = FALSE, setMap(figure,invert=TRUE))

test1_lambda = phylosig(tree.reduced, trait.reduced.vector, method = "lambda", test = T)
test1_K = phylosig(tree.reduced, trait.reduced.vector, method = "K", test = T)
test1_lambda
test1_K

# Bio2; change the first line to get the rest of the variables
trait.vector = combined.reduced$bio2
names(trait.vector) <- combined.reduced$species
trait.reduced <- treedata(tree, trait.vector)$data
trait.reduced.vector <- trait.reduced[,1]
tree.reduced <- treedata(tree, trait.vector)$phy

# MODEL COMPARISON
fitBM = fitContinuous(tree.reduced, trait.reduced)
fitOU = fitContinuous(tree.reduced, trait.reduced, model = "OU") # Make sure it is truly ultrametric even considering small rounding errors -- even a small discrepancy will cause complaints and longer runtime due to VCF optimization.
fitEB = fitContinuous(tree.reduced, trait.reduced, model = "EB")

fitBM$opt$aicc
fitOU$opt$aicc
fitEB$opt$aicc

#plot
figure = contMap(tree.reduced, trait.reduced.vector, plot = FALSE, model = "BM")
plot(setMap(figure, invert = TRUE), legend=4*max(nodeHeights(tree)), fsize=0.6, ftype="i", lwd=c(3,1), lwd=c(3,1), outline = FALSE)

test2_lambda= phylosig(tree.reduced, trait.reduced.vector, method = "lambda", test = T)
test2_K = phylosig(tree.reduced, trait.reduced.vector, method = "K", test = T)
test2_lambda
test2_K

#Bio3
trait.vector = combined.reduced$bio3
names(trait.vector) <- combined.reduced$species
trait.reduced <- treedata(tree, trait.vector)$data
trait.reduced.vector <- trait.reduced[,1]
tree.reduced <- treedata(tree, trait.vector)$phy

# MODEL COMPARISON
fitBM = fitContinuous(tree.reduced, trait.reduced)
fitOU = fitContinuous(tree.reduced, trait.reduced, model = "OU") # Make sure it is truly ultrametric even considering small rounding errors -- even a small discrepancy will cause complaints and longer runtime due to VCF optimization.
fitEB = fitContinuous(tree.reduced, trait.reduced, model = "EB")

fitBM$opt$aicc
fitOU$opt$aicc
fitEB$opt$aicc

#plot
figure = contMap(tree.reduced, trait.reduced.vector, plot = FALSE, model = "BM")
plot(setMap(figure, invert = TRUE), legend=4*max(nodeHeights(tree)), fsize=0.6, ftype="i", lwd=c(3,1), lwd=c(3,1), outline = FALSE)

test3_lambda = phylosig(tree.reduced, trait.reduced.vector, method = "lambda", test = T)
test3_K = phylosig(tree.reduced, trait.reduced.vector, method = "K", test = T)
test3_lambda
test3_K

#Bio4
trait.vector = combined.reduced$bio4
names(trait.vector) <- combined.reduced$species
trait.reduced <- treedata(tree, trait.vector)$data
trait.reduced.vector <- trait.reduced[,1]
tree.reduced <- treedata(tree, trait.vector)$phy

# MODEL COMPARISON
fitBM = fitContinuous(tree.reduced, trait.reduced)
fitOU = fitContinuous(tree.reduced, trait.reduced, model = "OU") # Make sure it is truly ultrametric even considering small rounding errors -- even a small discrepancy will cause complaints and longer runtime due to VCF optimization.
fitEB = fitContinuous(tree.reduced, trait.reduced, model = "EB")

fitBM$opt$aicc
fitOU$opt$aicc
fitEB$opt$aicc

#plot
figure = contMap(tree.reduced, trait.reduced.vector, plot = FALSE, model = "BM")
plot(setMap(figure, invert = TRUE), legend=4*max(nodeHeights(tree)), fsize=0.6, ftype="i", lwd=c(3,1), lwd=c(3,1), outline = FALSE)

test4_lambda = phylosig(tree.reduced, trait.reduced.vector, method = "lambda", test = T)
test4_K = phylosig(tree.reduced, trait.reduced.vector, method = "K", test = T)
test4_lambda
test4_K

#Bio5
trait.vector = combined.reduced$bio5
names(trait.vector) <- combined.reduced$species
trait.reduced <- treedata(tree, trait.vector)$data
trait.reduced.vector <- trait.reduced[,1]
tree.reduced <- treedata(tree, trait.vector)$phy

# MODEL COMPARISON
fitBM = fitContinuous(tree.reduced, trait.reduced)
fitOU = fitContinuous(tree.reduced, trait.reduced, model = "OU") # Make sure it is truly ultrametric even considering small rounding errors -- even a small discrepancy will cause complaints and longer runtime due to VCF optimization.
fitEB = fitContinuous(tree.reduced, trait.reduced, model = "EB")

fitBM$opt$aicc
fitOU$opt$aicc
fitEB$opt$aicc

#plot
figure = contMap(tree.reduced, trait.reduced.vector, plot = FALSE, model = "BM")
plot(setMap(figure, invert = TRUE), legend=4*max(nodeHeights(tree)), fsize=0.6, ftype="i", lwd=c(3,1), lwd=c(3,1), outline = FALSE)

test5_lambda = phylosig(tree.reduced, trait.reduced.vector, method = "lambda", test = T)
test5_K = phylosig(tree.reduced, trait.reduced.vector, method = "K", test = T)
test5_lambda
test5_K

#Bio6
trait.vector = combined.reduced$bio6
names(trait.vector) <- combined.reduced$species
trait.reduced <- treedata(tree, trait.vector)$data
trait.reduced.vector <- trait.reduced[,1]
tree.reduced <- treedata(tree, trait.vector)$phy

# MODEL COMPARISON
fitBM = fitContinuous(tree.reduced, trait.reduced)
fitOU = fitContinuous(tree.reduced, trait.reduced, model = "OU") # Make sure it is truly ultrametric even considering small rounding errors -- even a small discrepancy will cause complaints and longer runtime due to VCF optimization.
fitEB = fitContinuous(tree.reduced, trait.reduced, model = "EB")

fitBM$opt$aicc
fitOU$opt$aicc
fitEB$opt$aicc

#plot
figure = contMap(tree.reduced, trait.reduced.vector, plot = FALSE, model = "BM")
plot(setMap(figure, invert = TRUE), legend=4*max(nodeHeights(tree)), fsize=0.6, ftype="i", lwd=c(3,1), lwd=c(3,1), outline = FALSE)

test6_lambda = phylosig(tree.reduced, trait.reduced.vector, method = "lambda", test = T)
test6_K = phylosig(tree.reduced, trait.reduced.vector, method = "K", test = T)
test6_lambda
test6_K

#Bio7
trait.vector = combined.reduced$bio7
names(trait.vector) <- combined.reduced$species
trait.reduced <- treedata(tree, trait.vector)$data
trait.reduced.vector <- trait.reduced[,1]
tree.reduced <- treedata(tree, trait.vector)$phy

# MODEL COMPARISON
fitBM = fitContinuous(tree.reduced, trait.reduced)
fitOU = fitContinuous(tree.reduced, trait.reduced, model = "OU") # Make sure it is truly ultrametric even considering small rounding errors -- even a small discrepancy will cause complaints and longer runtime due to VCF optimization.
fitEB = fitContinuous(tree.reduced, trait.reduced, model = "EB")

fitBM$opt$aicc
fitOU$opt$aicc
fitEB$opt$aicc

#plot
figure = contMap(tree.reduced, trait.reduced.vector, plot = FALSE, model = "BM")
plot(setMap(figure, invert = TRUE), legend=4*max(nodeHeights(tree)), fsize=0.6, ftype="i", lwd=c(3,1), lwd=c(3,1), outline = FALSE)

test7_lambda = phylosig(tree.reduced, trait.reduced.vector, method = "lambda", test = T)
test7_K = phylosig(tree.reduced, trait.reduced.vector, method = "K", test = T)
test7_lambda
test7_K

#Bio8
trait.vector = combined.reduced$bio8
names(trait.vector) <- combined.reduced$species
trait.reduced <- treedata(tree, trait.vector)$data
trait.reduced.vector <- trait.reduced[,1]
tree.reduced <- treedata(tree, trait.vector)$phy

# MODEL COMPARISON
fitBM = fitContinuous(tree.reduced, trait.reduced)
fitOU = fitContinuous(tree.reduced, trait.reduced, model = "OU") # Make sure it is truly ultrametric even considering small rounding errors -- even a small discrepancy will cause complaints and longer runtime due to VCF optimization.
fitEB = fitContinuous(tree.reduced, trait.reduced, model = "EB")

fitBM$opt$aicc
fitOU$opt$aicc
fitEB$opt$aicc

#plot
figure = contMap(tree.reduced, trait.reduced.vector, plot = FALSE, model = "BM")
plot(setMap(figure, invert = TRUE), legend=4*max(nodeHeights(tree)), fsize=0.6, ftype="i", lwd=c(3,1), lwd=c(3,1), outline = FALSE)

test8_lambda = phylosig(tree.reduced, trait.reduced.vector, method = "lambda", test = T)
test8_K = phylosig(tree.reduced, trait.reduced.vector, method = "K", test = T)
test8_lambda
test8_K

#Bio9
trait.vector = combined.reduced$bio9
names(trait.vector) <- combined.reduced$species
trait.reduced <- treedata(tree, trait.vector)$data
trait.reduced.vector <- trait.reduced[,1]
tree.reduced <- treedata(tree, trait.vector)$phy

# MODEL COMPARISON
fitBM = fitContinuous(tree.reduced, trait.reduced)
fitOU = fitContinuous(tree.reduced, trait.reduced, model = "OU") # Make sure it is truly ultrametric even considering small rounding errors -- even a small discrepancy will cause complaints and longer runtime due to VCF optimization.
fitEB = fitContinuous(tree.reduced, trait.reduced, model = "EB")

fitBM$opt$aicc
fitOU$opt$aicc
fitEB$opt$aicc

#plot
figure = contMap(tree.reduced, trait.reduced.vector, plot = FALSE, model = "BM")
plot(setMap(figure, invert = TRUE), legend=4*max(nodeHeights(tree)), fsize=0.6, ftype="i", lwd=c(3,1), lwd=c(3,1), outline = FALSE)

test9_lambda = phylosig(tree.reduced, trait.reduced.vector, method = "lambda", test = T)
test9_K = phylosig(tree.reduced, trait.reduced.vector, method = "K", test = T)
test9_lambda
test9_K

#Bio10
trait.vector = combined.reduced$bio10
names(trait.vector) <- combined.reduced$species
trait.reduced <- treedata(tree, trait.vector)$data
trait.reduced.vector <- trait.reduced[,1]
tree.reduced <- treedata(tree, trait.vector)$phy

# MODEL COMPARISON
fitBM = fitContinuous(tree.reduced, trait.reduced)
fitOU = fitContinuous(tree.reduced, trait.reduced, model = "OU") # Make sure it is truly ultrametric even considering small rounding errors -- even a small discrepancy will cause complaints and longer runtime due to VCF optimization.
fitEB = fitContinuous(tree.reduced, trait.reduced, model = "EB")

fitBM$opt$aicc
fitOU$opt$aicc
fitEB$opt$aicc

#plot
figure = contMap(tree.reduced, trait.reduced.vector, plot = FALSE, model = "BM")
plot(setMap(figure, invert = TRUE), legend=4*max(nodeHeights(tree)), fsize=0.6, ftype="i", lwd=c(3,1), lwd=c(3,1), outline = FALSE)

test10_lambda = phylosig(tree.reduced, trait.reduced.vector, method = "lambda", test = T)
test10_K = phylosig(tree.reduced, trait.reduced.vector, method = "K", test = T)
test10_lambda
test10_K

#Bio11
trait.vector = combined.reduced$bio11
names(trait.vector) <- combined.reduced$species
trait.reduced <- treedata(tree, trait.vector)$data
trait.reduced.vector <- trait.reduced[,1]
tree.reduced <- treedata(tree, trait.vector)$phy

# MODEL COMPARISON
fitBM = fitContinuous(tree.reduced, trait.reduced)
fitOU = fitContinuous(tree.reduced, trait.reduced, model = "OU") # Make sure it is truly ultrametric even considering small rounding errors -- even a small discrepancy will cause complaints and longer runtime due to VCF optimization.
fitEB = fitContinuous(tree.reduced, trait.reduced, model = "EB")

fitBM$opt$aicc
fitOU$opt$aicc
fitEB$opt$aicc

#plot
figure = contMap(tree.reduced, trait.reduced.vector, plot = FALSE, model = "BM")
plot(setMap(figure, invert = TRUE), legend=4*max(nodeHeights(tree)), fsize=0.6, ftype="i", lwd=c(3,1), lwd=c(3,1), outline = FALSE)

test11_lambda = phylosig(tree.reduced, trait.reduced.vector, method = "lambda", test = T)
test11_K = phylosig(tree.reduced, trait.reduced.vector, method = "K", test = T)
test11_lambda
test11_K

#Bio12
trait.vector = combined.reduced$bio12
names(trait.vector) <- combined.reduced$species
trait.reduced <- treedata(tree, trait.vector)$data
trait.reduced.vector <- trait.reduced[,1]
tree.reduced <- treedata(tree, trait.vector)$phy

# MODEL COMPARISON
fitBM = fitContinuous(tree.reduced, trait.reduced)
fitOU = fitContinuous(tree.reduced, trait.reduced, model = "OU") # Make sure it is truly ultrametric even considering small rounding errors -- even a small discrepancy will cause complaints and longer runtime due to VCF optimization.
fitEB = fitContinuous(tree.reduced, trait.reduced, model = "EB")

fitBM$opt$aicc
fitOU$opt$aicc
fitEB$opt$aicc

#plot
figure = contMap(tree.reduced, trait.reduced.vector, plot = FALSE, model = "BM")
plot(setMap(figure, invert = TRUE), legend=4*max(nodeHeights(tree)), fsize=0.6, ftype="i", lwd=c(3,1), lwd=c(3,1), outline = FALSE)

test12_lambda = phylosig(tree.reduced, trait.reduced.vector, method = "lambda", test = T)
test12_K = phylosig(tree.reduced, trait.reduced.vector, method = "K", test = T)
test12_lambda
test12_K

#Bio13
trait.vector = combined.reduced$bio13
names(trait.vector) <- combined.reduced$species
trait.reduced <- treedata(tree, trait.vector)$data
trait.reduced.vector <- trait.reduced[,1]
tree.reduced <- treedata(tree, trait.vector)$phy

# MODEL COMPARISON
fitBM = fitContinuous(tree.reduced, trait.reduced)
fitOU = fitContinuous(tree.reduced, trait.reduced, model = "OU") # Make sure it is truly ultrametric even considering small rounding errors -- even a small discrepancy will cause complaints and longer runtime due to VCF optimization.
fitEB = fitContinuous(tree.reduced, trait.reduced, model = "EB")

fitBM$opt$aicc
fitOU$opt$aicc
fitEB$opt$aicc

#plot
figure = contMap(tree.reduced, trait.reduced.vector, plot = FALSE, model = "BM")
plot(setMap(figure, invert = TRUE), legend=4*max(nodeHeights(tree)), fsize=0.6, ftype="i", lwd=c(3,1), lwd=c(3,1), outline = FALSE)

test13_lambda = phylosig(tree.reduced, trait.reduced.vector, method = "lambda", test = T)
test13_K = phylosig(tree.reduced, trait.reduced.vector, method = "K", test = T)
test13_lambda
test13_K

#Bio14
trait.vector = combined.reduced$bio14
names(trait.vector) <- combined.reduced$species
trait.reduced <- treedata(tree, trait.vector)$data
trait.reduced.vector <- trait.reduced[,1]
tree.reduced <- treedata(tree, trait.vector)$phy

# MODEL COMPARISON
fitBM = fitContinuous(tree.reduced, trait.reduced)
fitOU = fitContinuous(tree.reduced, trait.reduced, model = "OU") # Make sure it is truly ultrametric even considering small rounding errors -- even a small discrepancy will cause complaints and longer runtime due to VCF optimization.
fitEB = fitContinuous(tree.reduced, trait.reduced, model = "EB")

fitBM$opt$aicc
fitOU$opt$aicc
fitEB$opt$aicc

#plot
figure = contMap(tree.reduced, trait.reduced.vector, plot = FALSE, model = "BM")
plot(setMap(figure, invert = TRUE), legend=4*max(nodeHeights(tree)), fsize=0.6, ftype="i", lwd=c(3,1), lwd=c(3,1), outline = FALSE)

test14_lambda = phylosig(tree.reduced, trait.reduced.vector, method = "lambda", test = T)
test14_K = phylosig(tree.reduced, trait.reduced.vector, method = "K", test = T)
test14_lambda
test14_K

#Bio15
trait.vector = combined.reduced$bio15
names(trait.vector) <- combined.reduced$species
trait.reduced <- treedata(tree, trait.vector)$data
trait.reduced.vector <- trait.reduced[,1]
tree.reduced <- treedata(tree, trait.vector)$phy

# MODEL COMPARISON
fitBM = fitContinuous(tree.reduced, trait.reduced)
fitOU = fitContinuous(tree.reduced, trait.reduced, model = "OU") # Make sure it is truly ultrametric even considering small rounding errors -- even a small discrepancy will cause complaints and longer runtime due to VCF optimization.
fitEB = fitContinuous(tree.reduced, trait.reduced, model = "EB")

fitBM$opt$aicc
fitOU$opt$aicc
fitEB$opt$aicc

#plot
figure = contMap(tree.reduced, trait.reduced.vector, plot = FALSE, model = "BM")
plot(setMap(figure, invert = TRUE), legend=4*max(nodeHeights(tree)), fsize=0.6, ftype="i", lwd=c(3,1), lwd=c(3,1), outline = FALSE)

test15_lambda = phylosig(tree.reduced, trait.reduced.vector, method = "lambda", test = T)
test15_K = phylosig(tree.reduced, trait.reduced.vector, method = "K", test = T)
test15_lambda
test15_K

#Bio16
trait.vector = combined.reduced$bio16
names(trait.vector) <- combined.reduced$species
trait.reduced <- treedata(tree, trait.vector)$data
trait.reduced.vector <- trait.reduced[,1]
tree.reduced <- treedata(tree, trait.vector)$phy

# MODEL COMPARISON
fitBM = fitContinuous(tree.reduced, trait.reduced)
fitOU = fitContinuous(tree.reduced, trait.reduced, model = "OU") # Make sure it is truly ultrametric even considering small rounding errors -- even a small discrepancy will cause complaints and longer runtime due to VCF optimization.
fitEB = fitContinuous(tree.reduced, trait.reduced, model = "EB")

fitBM$opt$aicc
fitOU$opt$aicc
fitEB$opt$aicc

#plot
figure = contMap(tree.reduced, trait.reduced.vector, plot = FALSE, model = "BM")
plot(setMap(figure, invert = TRUE), legend=4*max(nodeHeights(tree)), fsize=0.6, ftype="i", lwd=c(3,1), lwd=c(3,1), outline = FALSE)

test16_lambda = phylosig(tree.reduced, trait.reduced.vector, method = "lambda", test = T)
test16_K = phylosig(tree.reduced, trait.reduced.vector, method = "K", test = T)
test16_lambda
test16_K

#Bio17
trait.vector = combined.reduced$bio17
names(trait.vector) <- combined.reduced$species
trait.reduced <- treedata(tree, trait.vector)$data
trait.reduced.vector <- trait.reduced[,1]
tree.reduced <- treedata(tree, trait.vector)$phy

# MODEL COMPARISON
fitBM = fitContinuous(tree.reduced, trait.reduced)
fitOU = fitContinuous(tree.reduced, trait.reduced, model = "OU") # Make sure it is truly ultrametric even considering small rounding errors -- even a small discrepancy will cause complaints and longer runtime due to VCF optimization.
fitEB = fitContinuous(tree.reduced, trait.reduced, model = "EB")

fitBM$opt$aicc
fitOU$opt$aicc
fitEB$opt$aicc

#plot
figure = contMap(tree.reduced, trait.reduced.vector, plot = FALSE, model = "BM")
plot(setMap(figure, invert = TRUE), legend=4*max(nodeHeights(tree)), fsize=0.6, ftype="i", lwd=c(3,1), lwd=c(3,1), outline = FALSE)

test17_lambda = phylosig(tree.reduced, trait.reduced.vector, method = "lambda", test = T)
test17_K = phylosig(tree.reduced, trait.reduced.vector, method = "K", test = T)
test17_lambda
test17_K

#Bio18
trait.vector = combined.reduced$bio18
names(trait.vector) <- combined.reduced$species
trait.reduced <- treedata(tree, trait.vector)$data
trait.reduced.vector <- trait.reduced[,1]
tree.reduced <- treedata(tree, trait.vector)$phy

# MODEL COMPARISON
fitBM = fitContinuous(tree.reduced, trait.reduced)
fitOU = fitContinuous(tree.reduced, trait.reduced, model = "OU") # Make sure it is truly ultrametric even considering small rounding errors -- even a small discrepancy will cause complaints and longer runtime due to VCF optimization.
fitEB = fitContinuous(tree.reduced, trait.reduced, model = "EB")

fitBM$opt$aicc
fitOU$opt$aicc
fitEB$opt$aicc

#plot
figure = contMap(tree.reduced, trait.reduced.vector, plot = FALSE, model = "BM")
plot(setMap(figure, invert = TRUE), legend=4*max(nodeHeights(tree)), fsize=0.6, ftype="i", lwd=c(3,1), lwd=c(3,1), outline = FALSE)

test18_lambda = phylosig(tree.reduced, trait.reduced.vector, method = "lambda", test = T)
test18_K = phylosig(tree.reduced, trait.reduced.vector, method = "K", test = T)
test18_lambda
test18_K

#Bio19
trait.vector = combined.reduced$bio19
names(trait.vector) <- combined.reduced$species
trait.reduced <- treedata(tree, trait.vector)$data
trait.reduced.vector <- trait.reduced[,1]
tree.reduced <- treedata(tree, trait.vector)$phy

# MODEL COMPARISON
fitBM = fitContinuous(tree.reduced, trait.reduced)
fitOU = fitContinuous(tree.reduced, trait.reduced, model = "OU") # Make sure it is truly ultrametric even considering small rounding errors -- even a small discrepancy will cause complaints and longer runtime due to VCF optimization.
fitEB = fitContinuous(tree.reduced, trait.reduced, model = "EB")

fitBM$opt$aicc
fitOU$opt$aicc
fitEB$opt$aicc

#plot
figure = contMap(tree.reduced, trait.reduced.vector, plot = FALSE, model = "BM")
plot(setMap(figure, invert = TRUE), legend=4*max(nodeHeights(tree)), fsize=0.6, ftype="i", lwd=c(3,1), lwd=c(3,1), outline = FALSE)

test19_lambda = phylosig(tree.reduced, trait.reduced.vector, method = "lambda", test = T)
test19_K = phylosig(tree.reduced, trait.reduced.vector, method = "K", test = T)
test19_lambda
test19_K

#aspect
trait.vector = combined.reduced$aspect
names(trait.vector) <- combined.reduced$species
trait.reduced <- treedata(tree, trait.vector)$data
trait.reduced.vector <- trait.reduced[,1]
tree.reduced <- treedata(tree, trait.vector)$phy

# MODEL COMPARISON
fitBM = fitContinuous(tree.reduced, trait.reduced)
fitOU = fitContinuous(tree.reduced, trait.reduced, model = "OU") # Make sure it is truly ultrametric even considering small rounding errors -- even a small discrepancy will cause complaints and longer runtime due to VCF optimization.
fitEB = fitContinuous(tree.reduced, trait.reduced, model = "EB")

fitBM$opt$aicc
fitOU$opt$aicc
fitEB$opt$aicc

#plot
figure = contMap(tree.reduced, trait.reduced.vector, plot = FALSE, model = "BM")
plot(setMap(figure, invert = TRUE), legend=4*max(nodeHeights(tree)), fsize=0.6, ftype="i", lwd=c(3,1), lwd=c(3,1), outline = FALSE)

test_aspect_lambda = phylosig(tree.reduced, trait.reduced.vector, method = "lambda", test = T)
test_aspect_K = phylosig(tree.reduced, trait.reduced.vector, method = "K", test = T)
test_aspect_lambda
test_aspect_K

#elevation
trait.vector = combined.reduced$elevation
names(trait.vector) <- combined.reduced$species
trait.reduced <- treedata(tree, trait.vector)$data
trait.reduced.vector <- trait.reduced[,1]
tree.reduced <- treedata(tree, trait.vector)$phy

# MODEL COMPARISON
fitBM = fitContinuous(tree.reduced, trait.reduced)
fitOU = fitContinuous(tree.reduced, trait.reduced, model = "OU") # Make sure it is truly ultrametric even considering small rounding errors -- even a small discrepancy will cause complaints and longer runtime due to VCF optimization.
fitEB = fitContinuous(tree.reduced, trait.reduced, model = "EB")

fitBM$opt$aicc
fitOU$opt$aicc
fitEB$opt$aicc

#plot
figure = contMap(tree.reduced, trait.reduced.vector, plot = FALSE, model = "BM")
plot(setMap(figure, invert = TRUE), legend=4*max(nodeHeights(tree)), fsize=0.6, ftype="i", lwd=c(3,1), lwd=c(3,1), outline = FALSE)

test_elevation_lambda = phylosig(tree.reduced, trait.reduced.vector, method = "lambda", test = T)
test_elevation_K = phylosig(tree.reduced, trait.reduced.vector, method = "K", test = T)
test_elevation_lambda
test_elevation_K

#slope
trait.vector = combined.reduced$slope
names(trait.vector) <- combined.reduced$species
trait.reduced <- treedata(tree, trait.vector)$data
trait.reduced.vector <- trait.reduced[,1]
tree.reduced <- treedata(tree, trait.vector)$phy

# MODEL COMPARISON
fitBM = fitContinuous(tree.reduced, trait.reduced)
fitOU = fitContinuous(tree.reduced, trait.reduced, model = "OU") # Make sure it is truly ultrametric even considering small rounding errors -- even a small discrepancy will cause complaints and longer runtime due to VCF optimization.
fitEB = fitContinuous(tree.reduced, trait.reduced, model = "EB")

fitBM$opt$aicc
fitOU$opt$aicc
fitEB$opt$aicc

#plot
figure = contMap(tree.reduced, trait.reduced.vector, plot = FALSE, model = "BM")
plot(setMap(figure, invert = TRUE), legend=4*max(nodeHeights(tree)), fsize=0.6, ftype="i", lwd=c(3,1), lwd=c(3,1), outline = FALSE)

test_slope_lambda = phylosig(tree.reduced, trait.reduced.vector, method = "lambda", test = T)
test_slope_K = phylosig(tree.reduced, trait.reduced.vector, method = "K", test = T)
test_slope_lambda
test_slope_K

#bulkdensity
trait.vector = combined.reduced$bulkdensity
names(trait.vector) <- combined.reduced$species
trait.reduced <- treedata(tree, trait.vector)$data
trait.reduced.vector <- trait.reduced[,1]
tree.reduced <- treedata(tree, trait.vector)$phy

# MODEL COMPARISON
fitBM = fitContinuous(tree.reduced, trait.reduced)
fitOU = fitContinuous(tree.reduced, trait.reduced, model = "OU") # Make sure it is truly ultrametric even considering small rounding errors -- even a small discrepancy will cause complaints and longer runtime due to VCF optimization.
fitEB = fitContinuous(tree.reduced, trait.reduced, model = "EB")

fitBM$opt$aicc
fitOU$opt$aicc
fitEB$opt$aicc

#plot
figure = contMap(tree.reduced, trait.reduced.vector, plot = FALSE, model = "BM")
plot(setMap(figure, invert = TRUE), legend=4*max(nodeHeights(tree)), fsize=0.6, ftype="i", lwd=c(3,1), lwd=c(3,1), outline = FALSE)

test_bulkdensity_lambda = phylosig(tree.reduced, trait.reduced.vector, method = "lambda", test = T)
test_bulkdensity_K = phylosig(tree.reduced, trait.reduced.vector, method = "K", test = T)
test_bulkdensity_lambda
test_bulkdensity_K

#clay
trait.vector = combined.reduced$clay
names(trait.vector) <- combined.reduced$species
trait.reduced <- treedata(tree, trait.vector)$data
trait.reduced.vector <- trait.reduced[,1]
tree.reduced <- treedata(tree, trait.vector)$phy

# MODEL COMPARISON
fitBM = fitContinuous(tree.reduced, trait.reduced)
fitOU = fitContinuous(tree.reduced, trait.reduced, model = "OU") # Make sure it is truly ultrametric even considering small rounding errors -- even a small discrepancy will cause complaints and longer runtime due to VCF optimization.
fitEB = fitContinuous(tree.reduced, trait.reduced, model = "EB")

fitBM$opt$aicc
fitOU$opt$aicc
fitEB$opt$aicc

#plot
figure = contMap(tree.reduced, trait.reduced.vector, plot = FALSE, model = "BM")
plot(setMap(figure, invert = TRUE), legend=4*max(nodeHeights(tree)), fsize=0.6, ftype="i", lwd=c(3,1), lwd=c(3,1), outline = FALSE)

test_clay_lambda = phylosig(tree.reduced, trait.reduced.vector, method = "lambda", test = T)
test_clay_K = phylosig(tree.reduced, trait.reduced.vector, method = "K", test = T)
test_clay_lambda
test_clay_K

#coarsefragment
trait.vector = combined.reduced$coarsefragment
names(trait.vector) <- combined.reduced$species
trait.reduced <- treedata(tree, trait.vector)$data
trait.reduced.vector <- trait.reduced[,1]
tree.reduced <- treedata(tree, trait.vector)$phy

# MODEL COMPARISON
fitBM = fitContinuous(tree.reduced, trait.reduced)
fitOU = fitContinuous(tree.reduced, trait.reduced, model = "OU") # Make sure it is truly ultrametric even considering small rounding errors -- even a small discrepancy will cause complaints and longer runtime due to VCF optimization.
fitEB = fitContinuous(tree.reduced, trait.reduced, model = "EB")

fitBM$opt$aicc
fitOU$opt$aicc
fitEB$opt$aicc

#plot
figure = contMap(tree.reduced, trait.reduced.vector, plot = FALSE, model = "BM")
plot(setMap(figure, invert = TRUE), legend=4*max(nodeHeights(tree)), fsize=0.6, ftype="i", lwd=c(3,1), lwd=c(3,1), outline = FALSE)

test_coarsefragment_lambda = phylosig(tree.reduced, trait.reduced.vector, method = "lambda", test = T)
test_coarsefragment_K = phylosig(tree.reduced, trait.reduced.vector, method = "K", test = T)
test_coarsefragment_lambda
test_coarsefragment_K

#nitrogen
trait.vector = combined.reduced$nitrogen
names(trait.vector) <- combined.reduced$species
trait.reduced <- treedata(tree, trait.vector)$data
trait.reduced.vector <- trait.reduced[,1]
tree.reduced <- treedata(tree, trait.vector)$phy

# MODEL COMPARISON
fitBM = fitContinuous(tree.reduced, trait.reduced)
fitOU = fitContinuous(tree.reduced, trait.reduced, model = "OU") # Make sure it is truly ultrametric even considering small rounding errors -- even a small discrepancy will cause complaints and longer runtime due to VCF optimization.
fitEB = fitContinuous(tree.reduced, trait.reduced, model = "EB")

fitBM$opt$aicc
fitOU$opt$aicc
fitEB$opt$aicc

#plot
figure = contMap(tree.reduced, trait.reduced.vector, plot = FALSE, model = "BM")
plot(setMap(figure, invert = TRUE), legend=4*max(nodeHeights(tree)), fsize=0.6, ftype="i", lwd=c(3,1), lwd=c(3,1), outline = FALSE)

test_nitrogen_lambda = phylosig(tree.reduced, trait.reduced.vector, method = "lambda", test = T)
test_nitrogen_K = phylosig(tree.reduced, trait.reduced.vector, method = "K", test = T)
test_nitrogen_lambda
test_nitrogen_K

#ph
trait.vector = combined.reduced$ph
names(trait.vector) <- combined.reduced$species
trait.reduced <- treedata(tree, trait.vector)$data
trait.reduced.vector <- trait.reduced[,1]
tree.reduced <- treedata(tree, trait.vector)$phy

# MODEL COMPARISON
fitBM = fitContinuous(tree.reduced, trait.reduced)
fitOU = fitContinuous(tree.reduced, trait.reduced, model = "OU") # Make sure it is truly ultrametric even considering small rounding errors -- even a small discrepancy will cause complaints and longer runtime due to VCF optimization.
fitEB = fitContinuous(tree.reduced, trait.reduced, model = "EB")

fitBM$opt$aicc
fitOU$opt$aicc
fitEB$opt$aicc

#plot
figure = contMap(tree.reduced, trait.reduced.vector, plot = FALSE, model = "BM")
plot(setMap(figure, invert = TRUE), legend=4*max(nodeHeights(tree)), fsize=0.6, ftype="i", lwd=c(3,1), lwd=c(3,1), outline = FALSE)

test_ph_lambda = phylosig(tree.reduced, trait.reduced.vector, method = "lambda", test = T)
test_ph_K = phylosig(tree.reduced, trait.reduced.vector, method = "K", test = T)
test_ph_lambda
test_ph_K

#sand
trait.vector = combined.reduced$sand
names(trait.vector) <- combined.reduced$species
trait.reduced <- treedata(tree, trait.vector)$data
trait.reduced.vector <- trait.reduced[,1]
tree.reduced <- treedata(tree, trait.vector)$phy

# MODEL COMPARISON
fitBM = fitContinuous(tree.reduced, trait.reduced)
fitOU = fitContinuous(tree.reduced, trait.reduced, model = "OU") # Make sure it is truly ultrametric even considering small rounding errors -- even a small discrepancy will cause complaints and longer runtime due to VCF optimization.
fitEB = fitContinuous(tree.reduced, trait.reduced, model = "EB")

fitBM$opt$aicc
fitOU$opt$aicc
fitEB$opt$aicc

#plot
figure = contMap(tree.reduced, trait.reduced.vector, plot = FALSE, model = "BM")
plot(setMap(figure, invert = TRUE), legend=4*max(nodeHeights(tree)), fsize=0.6, ftype="i", lwd=c(3,1), lwd=c(3,1), outline = FALSE)

test_sand_lambda = phylosig(tree.reduced, trait.reduced.vector, method = "lambda", test = T)
test_sand_K = phylosig(tree.reduced, trait.reduced.vector, method = "K", test = T)
test_sand_lambda
test_sand_K

#silt
trait.vector = combined.reduced$silt
names(trait.vector) <- combined.reduced$species
trait.reduced <- treedata(tree, trait.vector)$data
trait.reduced.vector <- trait.reduced[,1]
tree.reduced <- treedata(tree, trait.vector)$phy

# MODEL COMPARISON
fitBM = fitContinuous(tree.reduced, trait.reduced)
fitOU = fitContinuous(tree.reduced, trait.reduced, model = "OU") # Make sure it is truly ultrametric even considering small rounding errors -- even a small discrepancy will cause complaints and longer runtime due to VCF optimization.
fitEB = fitContinuous(tree.reduced, trait.reduced, model = "EB")

fitBM$opt$aicc
fitOU$opt$aicc
fitEB$opt$aicc

#plot
figure = contMap(tree.reduced, trait.reduced.vector, plot = FALSE, model = "BM")
plot(setMap(figure, invert = TRUE), legend=4*max(nodeHeights(tree)), fsize=0.6, ftype="i", lwd=c(3,1), lwd=c(3,1), outline = FALSE)

test_silt_lambda = phylosig(tree.reduced, trait.reduced.vector, method = "lambda", test = T)
test_silt_K = phylosig(tree.reduced, trait.reduced.vector, method = "K", test = T)
test_silt_lambda
test_silt_K

#carbon
trait.vector = combined.reduced$carbon
names(trait.vector) <- combined.reduced$species
trait.reduced <- treedata(tree, trait.vector)$data
trait.reduced.vector <- trait.reduced[,1]
tree.reduced <- treedata(tree, trait.vector)$phy

# MODEL COMPARISON
fitBM = fitContinuous(tree.reduced, trait.reduced)
fitOU = fitContinuous(tree.reduced, trait.reduced, model = "OU") # Make sure it is truly ultrametric even considering small rounding errors -- even a small discrepancy will cause complaints and longer runtime due to VCF optimization.
fitEB = fitContinuous(tree.reduced, trait.reduced, model = "EB")

fitBM$opt$aicc
fitOU$opt$aicc
fitEB$opt$aicc

#plot
figure = contMap(tree.reduced, trait.reduced.vector, plot = FALSE, model = "BM")
plot(setMap(figure, invert = TRUE), legend=4*max(nodeHeights(tree)), fsize=0.6, ftype="i", lwd=c(3,1), lwd=c(3,1), outline = FALSE)

test_carbon_lambda = phylosig(tree.reduced, trait.reduced.vector, method = "lambda", test = T)
test_carbon_K = phylosig(tree.reduced, trait.reduced.vector, method = "K", test = T)
test_carbon_lambda
test_carbon_K

#needleleaf
trait.vector = combined.reduced$needleleaf
names(trait.vector) <- combined.reduced$species
trait.reduced <- treedata(tree, trait.vector)$data
trait.reduced.vector <- trait.reduced[,1]
tree.reduced <- treedata(tree, trait.vector)$phy

# MODEL COMPARISON
fitBM = fitContinuous(tree.reduced, trait.reduced)
fitOU = fitContinuous(tree.reduced, trait.reduced, model = "OU") # Make sure it is truly ultrametric even considering small rounding errors -- even a small discrepancy will cause complaints and longer runtime due to VCF optimization.
fitEB = fitContinuous(tree.reduced, trait.reduced, model = "EB")

fitBM$opt$aicc
fitOU$opt$aicc
fitEB$opt$aicc

#plot
figure = contMap(tree.reduced, trait.reduced.vector, plot = FALSE, model = "BM")
plot(setMap(figure, invert = TRUE), legend=4*max(nodeHeights(tree)), fsize=0.6, ftype="i", lwd=c(3,1), lwd=c(3,1), outline = FALSE)

test_needleleaf_lambda = phylosig(tree.reduced, trait.reduced.vector, method = "lambda", test = T)
test_needleleaf_K = phylosig(tree.reduced, trait.reduced.vector, method = "K", test = T)
test_needleleaf_lambda
test_needleleaf_K

#evergreenbroadlead
trait.vector = combined.reduced$evergreenbroadleaf
names(trait.vector) <- combined.reduced$species
trait.reduced <- treedata(tree, trait.vector)$data
trait.reduced.vector <- trait.reduced[,1]
tree.reduced <- treedata(tree, trait.vector)$phy

# MODEL COMPARISON
fitBM = fitContinuous(tree.reduced, trait.reduced)
fitOU = fitContinuous(tree.reduced, trait.reduced, model = "OU") # Make sure it is truly ultrametric even considering small rounding errors -- even a small discrepancy will cause complaints and longer runtime due to VCF optimization.
fitEB = fitContinuous(tree.reduced, trait.reduced, model = "EB")

fitBM$opt$aicc
fitOU$opt$aicc
fitEB$opt$aicc

#plot
figure = contMap(tree.reduced, trait.reduced.vector, plot = FALSE, model = "BM")
plot(setMap(figure, invert = TRUE), legend=4*max(nodeHeights(tree)), fsize=0.6, ftype="i", lwd=c(3,1), lwd=c(3,1), outline = FALSE)

test_evergreenbroadleaf_lambda = phylosig(tree.reduced, trait.reduced.vector, method = "lambda", test = T)
test_evergreenbroadleaf_K = phylosig(tree.reduced, trait.reduced.vector, method = "K", test = T)
test_evergreenbroadleaf_lambda
test_evergreenbroadleaf_K

#deciduousbroadleaf
trait.vector = combined.reduced$deciduousbroadleaf
names(trait.vector) <- combined.reduced$species
trait.reduced <- treedata(tree, trait.vector)$data
trait.reduced.vector <- trait.reduced[,1]
tree.reduced <- treedata(tree, trait.vector)$phy

# MODEL COMPARISON
fitBM = fitContinuous(tree.reduced, trait.reduced)
fitOU = fitContinuous(tree.reduced, trait.reduced, model = "OU") # Make sure it is truly ultrametric even considering small rounding errors -- even a small discrepancy will cause complaints and longer runtime due to VCF optimization.
fitEB = fitContinuous(tree.reduced, trait.reduced, model = "EB")

fitBM$opt$aicc
fitOU$opt$aicc
fitEB$opt$aicc

#plot
figure = contMap(tree.reduced, trait.reduced.vector, plot = FALSE, model = "BM")
plot(setMap(figure, invert = TRUE), legend=4*max(nodeHeights(tree)), fsize=0.6, ftype="i", lwd=c(3,1), lwd=c(3,1), outline = FALSE)

test_deciduousbroadleaf_lambda = phylosig(tree.reduced, trait.reduced.vector, method = "lambda", test = T)
test_deciduousbroadleaf_K = phylosig(tree.reduced, trait.reduced.vector, method = "K", test = T)
test_deciduousbroadleaf_lambda
test_deciduousbroadleaf_K

#mixedtrees
trait.vector = combined.reduced$mixedtrees
names(trait.vector) <- combined.reduced$species
trait.reduced <- treedata(tree, trait.vector)$data
trait.reduced.vector <- trait.reduced[,1]
tree.reduced <- treedata(tree, trait.vector)$phy

# MODEL COMPARISON
fitBM = fitContinuous(tree.reduced, trait.reduced)
fitOU = fitContinuous(tree.reduced, trait.reduced, model = "OU") # Make sure it is truly ultrametric even considering small rounding errors -- even a small discrepancy will cause complaints and longer runtime due to VCF optimization.
fitEB = fitContinuous(tree.reduced, trait.reduced, model = "EB")

fitBM$opt$aicc
fitOU$opt$aicc
fitEB$opt$aicc

#plot
figure = contMap(tree.reduced, trait.reduced.vector, plot = FALSE, model = "BM")
plot(setMap(figure, invert = TRUE), legend=4*max(nodeHeights(tree)), fsize=0.6, ftype="i", lwd=c(3,1), lwd=c(3,1), outline = FALSE)

test_mixedtrees_lambda = phylosig(tree.reduced, trait.reduced.vector, method = "lambda", test = T)
test_mixedtrees_K = phylosig(tree.reduced, trait.reduced.vector, method = "K", test = T)
test_mixedtrees_lambda
test_mixedtrees_K

#shrubs
trait.vector = combined.reduced$shrubs
names(trait.vector) <- combined.reduced$species
trait.reduced <- treedata(tree, trait.vector)$data
trait.reduced.vector <- trait.reduced[,1]
tree.reduced <- treedata(tree, trait.vector)$phy

# MODEL COMPARISON
fitBM = fitContinuous(tree.reduced, trait.reduced)
fitOU = fitContinuous(tree.reduced, trait.reduced, model = "OU") # Make sure it is truly ultrametric even considering small rounding errors -- even a small discrepancy will cause complaints and longer runtime due to VCF optimization.
fitEB = fitContinuous(tree.reduced, trait.reduced, model = "EB")

fitBM$opt$aicc
fitOU$opt$aicc
fitEB$opt$aicc

#plot
figure = contMap(tree.reduced, trait.reduced.vector, plot = FALSE, model = "BM")
plot(setMap(figure, invert = TRUE), legend=4*max(nodeHeights(tree)), fsize=0.6, ftype="i", lwd=c(3,1), lwd=c(3,1), outline = FALSE)

test_shrubs_lambda = phylosig(tree.reduced, trait.reduced.vector, method = "lambda", test = T)
test_shrubs_K = phylosig(tree.reduced, trait.reduced.vector, method = "K", test = T)
test_shrubs_lambda
test_shrubs_K

#herbaceous
trait.vector = combined.reduced$herbaceous
names(trait.vector) <- combined.reduced$species
trait.reduced <- treedata(tree, trait.vector)$data
trait.reduced.vector <- trait.reduced[,1]
tree.reduced <- treedata(tree, trait.vector)$phy

# MODEL COMPARISON
fitBM = fitContinuous(tree.reduced, trait.reduced)
fitOU = fitContinuous(tree.reduced, trait.reduced, model = "OU") # Make sure it is truly ultrametric even considering small rounding errors -- even a small discrepancy will cause complaints and longer runtime due to VCF optimization.
fitEB = fitContinuous(tree.reduced, trait.reduced, model = "EB")

fitBM$opt$aicc
fitOU$opt$aicc
fitEB$opt$aicc

#plot
figure = contMap(tree.reduced, trait.reduced.vector, plot = FALSE, model = "BM")
plot(setMap(figure, invert = TRUE), legend=4*max(nodeHeights(tree)), fsize=0.6, ftype="i", lwd=c(3,1), lwd=c(3,1), outline = FALSE)

test_herbaceous_lambda = phylosig(tree.reduced, trait.reduced.vector, method = "lambda", test = T)
test_herbaceous_K = phylosig(tree.reduced, trait.reduced.vector, method = "K", test = T)
test_herbaceous_lambda
test_herbaceous_K

# Biogeographic correlation -- ask whether clade and biogeography are independent in explaining environment

# Set measurement columns as numeric -- "n/a" can result in importing as character, causing MANOVA problems

combined.normalized <- rapply(combined.reduced, scale, c("numeric","integer"), how="replace")
combined.normalized$biogeo <- as.factor(combined.normalized$biogeo)
combined.normalized$species <- as.factor(combined.normalized$species)
res.man <- manova(cbind(bio1, bio2, bio3, bio4, bio5, bio6, bio7, bio8, bio9, bio10, bio11, bio12, bio13, bio14, bio15, bio16, bio17, bio18, bio19, elevation, aspect, slope, bulkdensity, nitrogen, clay, carbon, ph, sand, silt, coarsefragment, needleleaf, evergreenbroadleaf, deciduousbroadleaf, mixedtrees, shrubs, herbaceous, aridity) ~ species*biogeo, data = combined.normalized)
summary(res.man, tol = 0, test="Pillai")
summary.aov(res.man)

# res.lm <- lm(bio1 + bio2 + bio3 + bio4 + bio7 + bio12 + bio15 + bio17 + elevation + nitrogen + carbon + ph + sand + coarsefragment + needleleaf + deciduousbroadleaf + herbaceous + aridity ~ group*biogeo, data = combined.normalized)
# summary(res.lm)


