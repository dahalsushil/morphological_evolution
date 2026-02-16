library(vegan)
#install.packages("geosphere")
#library(geosphere)
library(ape)
library(ggplot2)

###################################################################
#Scripts to prune phylogenetic tree to match your morphological and environmental data
#do not need to run these codes after you already have pruned tree
###################################################################
#Load phylogenetic tree which should exactly match the species in morphological and environmental data so prune the tree to match that
tree = read.nexus("datedFigTree.tre")

#load in your environmental and morphologcial data
df = read.csv("env_morph_combined_data.csv", header= TRUE)
head(df)

# Check if species names in the tree and df data match
if (!all(names(df) %in% tree$tip.label)) {
  stop("species names in trait data do not match the phylogenetic tree.")
}

# Check for duplicate rows
duplicate_rows <- duplicated(df$species)

# If there are duplicate rows, average them
if (any(duplicate_rows)) {
  df <- df %>%
    group_by(type) %>%
    summarize_all(mean, na.rm = TRUE) %>%
    ungroup()
}

# Set species names as row names
rownames(df) <- df$species
df

# Extract species names from the trait data
trait_species <- unique(df$species)

# Extract species names from the tree
tree_species <- tree$tip.label

# Identify species to remove
species_to_remove <- setdiff(tree_species, trait_species)

# Prune the tree
pruned_tree <- drop.tip(tree, species_to_remove)

# Save the pruned tree
#write.tree(pruned_tree, file = "pruned_tree.tre") #use this to save your file
##################################################################################
#mantel tests
##################################
#load tree file
tree = read.tree("pruned_tree.tre")

#load environment data frame
env = df[,2:37]
#load morphological data frame
morpho = df[,38:ncol(df)]

#Now we have to convert these subsets into distance matrices.
#tree
dist.tree <- cophenetic(tree)

#environmental vector - Euclidean distance
dist.env = dist(env, method = "euclidean")

#morphological vector - Euclidean distance
dist.morpho = vegdist(morpho, method = "euclidean")

#Now we can run the mantel command:
#env vs morpho 
env_morpho = mantel(dist.env, dist.morpho, method = "spearman", permutations = 9999, na.rm = TRUE)
env_morpho

#tree vs env
tree_env = mantel(dist.tree, dist.env, method = "spearman", permutations = 9999, na.rm = TRUE)
tree_env

#tree vs morpho
tree_morpho = mantel(dist.tree, dist.morpho, method = "spearman", permutations = 9999, na.rm = TRUE)
tree_morpho

## Partial mantel test if needed
mantel.partial(dist.tree, dist.env, dist.morpho, permutations = 9999)

##Scatter plot
Tree = as.vector(dist.tree)
Env = as.vector(dist.env)
Morpho = as.vector(dist.morpho)
#new data frame with vectorized distance matrices
length(Env) <- length(Tree)
length(Morpho) <- length(Tree)
abc = data.frame(Tree,Env,Morpho)

#scatter plot environment distance vs cophenetic distance
scatter_plot1 = ggplot(abc, aes(x = Env, y = Tree)) + 
  geom_point(size = 4, alpha = 0.75, colour = "blue",shape = 21) + 
  geom_smooth(method = "lm", colour = "black", alpha = 0.2) + 
  labs(x = "Environmental differences", y = "Cophenetic distance") + 
  theme( axis.text.x = element_text(face = "bold",colour = "black", size = 12), 
         axis.text.y = element_text(face = "bold", size = 11, colour = "black"), 
         axis.title= element_text(face = "bold", size = 14, colour = "black"), 
         panel.background = element_blank(), 
         panel.border = element_rect(fill = NA, colour = "black"),
         legend.position = "top",
         legend.text = element_text(size = 10, face = "bold"),
         legend.title = element_text(size = 11, face = "bold")) +
  scale_fill_continuous(high = "navy", low = "skyblue")

scatter_plot1

#scatter plot morphological distance vs cophenetic distance
scatter_plot2 = ggplot(abc, aes(x = Morpho, y = Tree)) + 
  geom_point(size = 4, alpha = 0.75, colour = "blue",shape = 21) + 
  geom_smooth(method = "lm", colour = "black", alpha = 0.2) + 
  labs(x = "Morphological distance", y = "Cophenetic distance") + 
  theme( axis.text.x = element_text(face = "bold",colour = "black", size = 12), 
         axis.text.y = element_text(face = "bold", size = 11, colour = "black"), 
         axis.title= element_text(face = "bold", size = 14, colour = "black"), 
         panel.background = element_blank(), 
         panel.border = element_rect(fill = NA, colour = "black"),
         legend.position = "top",
         legend.text = element_text(size = 10, face = "bold"),
         legend.title = element_text(size = 11, face = "bold")) +
  scale_fill_continuous(high = "navy", low = "skyblue")

scatter_plot2

#scatter plot morphological distance vs environmenta; distance
scatter_plot3 = ggplot(abc, aes(x = Env, y = Morpho)) + 
  geom_point(size = 4, alpha = 0.75, colour = "blue",shape = 21) + 
  geom_smooth(method = "lm", colour = "black", alpha = 0.2) + 
  labs(x = "Environmental distance", y = "Morphological distance") + 
  theme( axis.text.x = element_text(face = "bold",colour = "black", size = 12), 
         axis.text.y = element_text(face = "bold", size = 11, colour = "black"), 
         axis.title= element_text(face = "bold", size = 14, colour = "black"), 
         panel.background = element_blank(), 
         panel.border = element_rect(fill = NA, colour = "black"),
         legend.position = "top",
         legend.text = element_text(size = 10, face = "bold"),
         legend.title = element_text(size = 11, face = "bold")) +
  scale_fill_continuous(high = "navy", low = "skyblue")

scatter_plot3





#tree vs Temperature, tested individual envrionmental variables but were insignificant
env_temp = df$bio1
dist.env_temp = dist(env_temp, method = "euclidean")
tree_env_temp = mantel(dist.tree, dist.env_temp, method = "spearman", permutations = 9999, na.rm = TRUE)
tree_env_temp

#tree vs below capitulum hair length, tested individual morphological characters but were insignificant
morph_character_11 = df$below_capitulum_hair_length
dist.morph_character_11 = dist(morph_character_11, method = "euclidean")
tree_morph_character_11 = mantel(dist.tree, dist.morph_character_11, method = "spearman", permutations = 9999, na.rm = TRUE)
tree_morph_character_11


## Partial mantel test phylogeny, annual temperature and below capitulum hair length
mantel.partial(dist.tree, dist.env_temp, dist.morph_character_11, permutations = 9999)



#######################
#Mantel tests for only significant traits 
########################

#load environment data frame
env = df[,c(9,11,19,27,29,33,34,37)]
#load morphological data frame
morpho = df[,c(40,42,43,48)]

#Now we have to convert these subsets into distance matrices.
#tree
dist.tree <- cophenetic(tree)

#environmental vector - Euclidean distance
dist.env = dist(env, method = "euclidean")

#morphological vector - Euclidean distance
dist.morpho = vegdist(morpho, method = "euclidean")

#Now we can run the mantel command:
#env vs morpho 
env_morpho = mantel(dist.env, dist.morpho, method = "spearman", permutations = 9999, na.rm = TRUE)
env_morpho

#tree vs env
tree_env = mantel(dist.tree, dist.env, method = "spearman", permutations = 9999, na.rm = TRUE)
tree_env

#tree vs morpho
tree_morpho = mantel(dist.tree, dist.morpho, method = "spearman", permutations = 9999, na.rm = TRUE)
tree_morpho

## Partial mantel test if needed
mantel.partial(dist.tree, dist.env, dist.morpho, permutations = 9999)

##Scatter plot
Tree = as.vector(dist.tree)
Env = as.vector(dist.env)
Morpho = as.vector(dist.morpho)
#new data frame with vectorized distance matrices
length(Env) <- length(Tree)
length(Morpho) <- length(Tree)
abc = data.frame(Tree,Env,Morpho)

#scatter plot environment distance vs cophenetic distance
scatter_plot1 = ggplot(abc, aes(x = Env, y = Tree)) + 
  geom_point(size = 4, alpha = 0.75, colour = "blue",shape = 21) + 
  geom_smooth(method = "lm", colour = "black", alpha = 0.2) + 
  labs(x = "Environmental differences", y = "Cophenetic distance") + 
  theme( axis.text.x = element_text(face = "bold",colour = "black", size = 12), 
         axis.text.y = element_text(face = "bold", size = 11, colour = "black"), 
         axis.title= element_text(face = "bold", size = 14, colour = "black"), 
         panel.background = element_blank(), 
         panel.border = element_rect(fill = NA, colour = "black"),
         legend.position = "top",
         legend.text = element_text(size = 10, face = "bold"),
         legend.title = element_text(size = 11, face = "bold")) +
  scale_fill_continuous(high = "navy", low = "skyblue")

scatter_plot1

#scatter plot morphological distance vs cophenetic distance
scatter_plot2 = ggplot(abc, aes(x = Morpho, y = Tree)) + 
  geom_point(size = 4, alpha = 0.75, colour = "blue",shape = 21) + 
  geom_smooth(method = "lm", colour = "black", alpha = 0.2) + 
  labs(x = "Morphological distance", y = "Cophenetic distance") + 
  theme( axis.text.x = element_text(face = "bold",colour = "black", size = 12), 
         axis.text.y = element_text(face = "bold", size = 11, colour = "black"), 
         axis.title= element_text(face = "bold", size = 14, colour = "black"), 
         panel.background = element_blank(), 
         panel.border = element_rect(fill = NA, colour = "black"),
         legend.position = "top",
         legend.text = element_text(size = 10, face = "bold"),
         legend.title = element_text(size = 11, face = "bold")) +
  scale_fill_continuous(high = "navy", low = "skyblue")

scatter_plot2

#scatter plot morphological distance vs environmenta; distance
scatter_plot3 = ggplot(abc, aes(x = Env, y = Morpho)) + 
  geom_point(size = 4, alpha = 0.75, colour = "blue",shape = 21) + 
  geom_smooth(method = "lm", colour = "black", alpha = 0.2) + 
  labs(x = "Environmental distance", y = "Morphological distance") + 
  theme( axis.text.x = element_text(face = "bold",colour = "black", size = 12), 
         axis.text.y = element_text(face = "bold", size = 11, colour = "black"), 
         axis.title= element_text(face = "bold", size = 14, colour = "black"), 
         panel.background = element_blank(), 
         panel.border = element_rect(fill = NA, colour = "black"),
         legend.position = "top",
         legend.text = element_text(size = 10, face = "bold"),
         legend.title = element_text(size = 11, face = "bold")) +
  scale_fill_continuous(high = "navy", low = "skyblue")

scatter_plot3


#tree vs Temperature, tested individual envrionmental variables but were insignificant
env_temp = df$bio1
dist.env_temp = dist(env_temp, method = "euclidean")
tree_env_temp = mantel(dist.tree, dist.env_temp, method = "spearman", permutations = 9999, na.rm = TRUE)
tree_env_temp

#tree vs below capitulum hair length, tested individual morphological characters but were insignificant
morph_character_11 = df$below_capitulum_hair_length
dist.morph_character_11 = dist(morph_character_11, method = "euclidean")
tree_morph_character_11 = mantel(dist.tree, dist.morph_character_11, method = "spearman", permutations = 9999, na.rm = TRUE)
tree_morph_character_11


## Partial mantel test phylogeny, annual temperature and below capitulum hair length
mantel.partial(dist.tree, dist.env_temp, dist.morph_character_11, permutations = 9999)



