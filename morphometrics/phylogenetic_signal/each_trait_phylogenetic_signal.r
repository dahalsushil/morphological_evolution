library(dplyr)
library(ape)
library(picante)
library(phytools)
library(geiger)

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

# Check for duplicate rows
duplicate_rows <- duplicated(combined$species)

# If there are duplicate rows, average them
if (any(duplicate_rows)) {
  combined <- combined %>%
    group_by(species) %>%
    summarize_all(mean, na.rm = TRUE) %>%
    ungroup()
}

# Set species names as row names
rownames(combined) <- combined$species
combined

# Normalize data matrix
combined.nonnormalized <- combined
combined <- rapply(combined,scale,c("numeric","integer"),how="replace")

#################################
## subgenus analysis-- Virgulus
#################################
subgenus_virgulus <- c("Symphyotrichum_bimater", "Symphyotrichum_concolor", "Symphyotrichum_pratense", "Symphyotrichum_oblongifolium", "Symphyotrichum_yukonense", "Symphyotrichum_hintonii", "Symphyotrichum_moranense", "Symphyotrichum_patens", "Symphyotrichum_adnatum", "Symphyotrichum_walteri", "Symphyotrichum_novae-angliae", "Symphyotrichum_ericoides", "Symphyotrichum_falcatum", "Symphyotrichum_turneri", "Symphyotrichum_purpurescens", "Symphyotrichum_chihuahuense", "Symphyotrichum_sericeum", "Symphyotrichum_fendleri", "Symphyotrichum_fontinale", "Symphyotrichum_grandiflorum", "Symphyotrichum_phlogifolium", "Symphyotrichum_georgianum", "Symphyotrichum_amethystinum", "Symphyotrichum_plumosum", "Symphyotrichum_lucayanum", "Symphyotrichum_campestre", "Symphyotrichum_estesii", "Symphyotrichum_trilineatum", "Symphyotrichum_pygmaeum")

combined_subset <- combined[combined$species %in% subgenus_virgulus,]
combined_subset$species <- as.factor(as.character(combined_subset$species))
str(combined_subset)

############################
#Testing PS for each of the morphological trait
############################

##############################################################################
#prune tree to match species in morphological data. 
#This can be done once. You do not have to run these codes everytime you perform signal tests
##############################################################################
tree = read.nexus("./datedFigTree.tre")
is.ultrametric(tree)
tree = force.ultrametric(tree, method = "extend")
tree <- ladderize(tree)

# Check if species names in the tree and trait data match
if (!all(names(combined_subset) %in% tree$tip.label)) {
  stop("species names in trait data do not match the phylogenetic tree.")
}

# Check for duplicate rows
duplicate_rows <- duplicated(combined_subset$species)

# If there are duplicate rows, average them
if (any(duplicate_rows)) {
  combined_subset <- combined_subset %>%
    group_by(species) %>%
    summarize_all(mean, na.rm = TRUE) %>%
    ungroup()
}

# Set species names as row names
rownames(combined_subset) <- combined_subset$species
combined_subset

# Extract species names from the trait data
trait_species <- unique(combined.reduced$species)

# Extract species names from the tree
tree_species <- tree$tip.label

# Identify species to remove
species_to_remove <- setdiff(tree_species, trait_species)

# Prune the tree
pruned_tree <- drop.tip(tree, species_to_remove)

# Save the pruned tree
#write.tree(pruned_tree, file = "pruned_tree.tre") #use this to save your file
################################################################################

#load pruned tree
tree = read.tree("pruned_tree.tre")
is.ultrametric(tree)
tree = force.ultrametric(tree, method = "extend")
tree <- ladderize(tree)

# Match species names between tree and trait data
matched_species <- match(tree$tip.label, rownames(combined))

# Filter out NA values (species not found in trait data)
matched_species <- matched_species[!is.na(matched_species)]

# Prepare the trait data in the order of the tree
combined.reduced.fixed <- combined[tree$tip.label, ]


#Assuming your trait data is in a column named "phyllary_length"
trait.vector = combined.reduced.fixed$phyllary_length
names(trait.vector) <- combined.reduced.fixed$species
lambda_phyllary_length <- phylosig(tree, trait.vector, method="lambda", test=TRUE, nsim=999)
lambda_phyllary_length
K_phyllary_length <- phylosig(tree, trait.vector, method="K", test=TRUE, nsim=999)
K_phyllary_length

#phyllary_width
trait.vector = combined.reduced.fixed$phyllary_width
names(trait.vector) <- combined.reduced.fixed$species
lambda_phyllary_width <- phylosig(tree, trait.vector, method="lambda", test=TRUE, nsim=999)
lambda_phyllary_width
K_phyllary_width <- phylosig(tree, trait.vector, method="K", test=TRUE, nsim=999)
K_phyllary_width

#primary_axis_leaf_length
trait.vector = combined.reduced.fixed$primary_axis_leaf_length
names(trait.vector) <- combined.reduced.fixed$species
lambda_primary_axis_leaf_length <- phylosig(tree, trait.vector, method="lambda", test=TRUE, nsim=999)
lambda_primary_axis_leaf_length
K_primary_axis_leaf_length <- phylosig(tree, trait.vector, method="K", test=TRUE, nsim=999)
K_primary_axis_leaf_length

#primary_axis_leaf_width
trait.vector = combined.reduced.fixed$primary_axis_leaf_width
names(trait.vector) <- combined.reduced.fixed$species
lambda_primary_axis_leaf_width <- phylosig(tree, trait.vector, method="lambda", test=TRUE, nsim=999)
lambda_primary_axis_leaf_width
K_primary_axis_leaf_width <- phylosig(tree, trait.vector, method="K", test=TRUE, nsim=999)
K_primary_axis_leaf_width

#secondary_axis_leaf_length
trait.vector = combined.reduced.fixed$secondary_axis_leaf_length
names(trait.vector) <- combined.reduced.fixed$species
lambda_secondary_axis_leaf_length <- phylosig(tree, trait.vector, method="lambda", test=TRUE, nsim=999)
lambda_secondary_axis_leaf_length
K_secondary_axis_leaf_length <- phylosig(tree, trait.vector, method="K", test=TRUE, nsim=999)
K_secondary_axis_leaf_length

#secondary_axis_leaf_width
trait.vector = combined.reduced.fixed$secondary_axis_leaf_width
names(trait.vector) <- combined.reduced.fixed$species
lambda_secondary_axis_leaf_width <- phylosig(tree, trait.vector, method="lambda", test=TRUE, nsim=999)
lambda_secondary_axis_leaf_width
K_secondary_axis_leaf_width<- phylosig(tree, trait.vector, method="K", test=TRUE, nsim=999)
K_secondary_axis_leaf_width

#bract_length
trait.vector = combined.reduced.fixed$bract_length
names(trait.vector) <- combined.reduced.fixed$species
lambda_bract_length <- phylosig(tree, trait.vector, method="lambda", test=TRUE, nsim=999)
lambda_bract_length
K_bract_length <- phylosig(tree, trait.vector, method="K", test=TRUE, nsim=999)
K_bract_length

#bract_width
trait.vector = combined.reduced.fixed$bract_width
names(trait.vector) <- combined.reduced.fixed$species
lambda_bract_width <- phylosig(tree, trait.vector, method="lambda", test=TRUE, nsim=999)
lambda_bract_width
K_bract_width <- phylosig(tree, trait.vector, method="K", test=TRUE, nsim=999)
K_bract_width

#auricle_length
trait.vector = combined.reduced.fixed$auricle_length
names(trait.vector) <- combined.reduced.fixed$species
lambda_auricle_length <- phylosig(tree, trait.vector, method="lambda", test=TRUE, nsim=999)
lambda_auricle_length
K_auricle_length <- phylosig(tree, trait.vector, method="K", test=TRUE, nsim=999)
K_auricle_length

#primary_axis_hair_length
trait.vector = combined.reduced.fixed$primary_axis_hair_length
names(trait.vector) <- combined.reduced.fixed$species
lambda_primary_axis_hair_length <- phylosig(tree, trait.vector, method="lambda", test=TRUE, nsim=999)
lambda_primary_axis_hair_length
K_primary_axis_hair_length <- phylosig(tree, trait.vector, method="K", test=TRUE, nsim=999)
K_primary_axis_hair_length

#below_capitulum_hair_length
trait.vector = combined.reduced.fixed$below_capitulum_hair_length
names(trait.vector) <- combined.reduced.fixed$species
lambda_below_capitulum_hair_length <- phylosig(tree, trait.vector, method="lambda", test=TRUE, nsim=999)
lambda_below_capitulum_hair_length
K_below_capitulum_hair_length <- phylosig(tree, trait.vector, method="K", test=TRUE, nsim=999)
K_below_capitulum_hair_length

#ray_length
trait.vector = combined.reduced.fixed$ray_length
names(trait.vector) <- combined.reduced.fixed$species
lambda_ray_length <- phylosig(tree, trait.vector, method="lambda", test=TRUE, nsim=999)
lambda_ray_length
K_ray_length <- phylosig(tree, trait.vector, method="K", test=TRUE, nsim=999)
K_ray_length

#ray_width
trait.vector = combined.reduced.fixed$ray_width
names(trait.vector) <- combined.reduced.fixed$species
lambda_ray_width <- phylosig(tree, trait.vector, method="lambda", test=TRUE, nsim=999)
lambda_ray_width
K_ray_width <- phylosig(tree, trait.vector, method="K", test=TRUE, nsim=999)
K_ray_width


