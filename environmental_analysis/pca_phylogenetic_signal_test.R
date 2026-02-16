library(ape)
library(picante)
library(phytools)
library(dplyr)

# Load phylogenetic tree
myTree = read.nexus("datedFigTree.tre")

# Load trait data
myTrait = read.csv("pca.csv")

# Check if species names in the tree and trait data match
if (!all(names(myTrait) %in% myTree$tip.label)) {
  stop("species names in trait data do not match the phylogenetic tree.")
}
# Check for duplicate rows
duplicate_rows <- duplicated(myTrait$Species)

# If there are duplicate rows, average them
if (any(duplicate_rows)) {
  myTrait <- myTrait %>%
    group_by(Species) %>%
    summarize_all(mean, na.rm = TRUE) %>%
    ungroup()
}

# Set species names as row names
rownames(myTrait) <- myTrait$Species
myTrait

# Extract species names from the trait data
trait_species <- unique(myTrait$Species)

# Extract species names from the tree
tree_species <- myTree$tip.label

# Identify species to remove
species_to_remove <- setdiff(tree_species, trait_species)

# Prune the tree
pruned_tree <- drop.tip(myTree, species_to_remove)

# Save the pruned tree
#write.tree(pruned_tree, file = "pruned_tree_pca.tre") #use this to save your file
tree = read.tree("pruned_tree_pca.tre")

# Match species names between tree and trait data
matched_species <- match(tree$tip.label, rownames(myTrait))

# Filter out NA values (species not found in trait data)
matched_species <- matched_species[!is.na(matched_species)]

# Prepare the trait data in the order of the tree
myTrait_fixed <- myTrait[tree$tip.label, ]

# Assuming your trait data is in a column named "lda.LD1"
trait_vector <- myTrait_fixed$PC1

#Pagel's lambda
phylosig(tree, trait_vector, method="lambda", test=TRUE, nsim=999)

#Blomberg's K
phylosig(tree, trait_vector, method="K", test=TRUE, nsim=999)

