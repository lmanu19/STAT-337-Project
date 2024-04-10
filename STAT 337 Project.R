# set working directory
setwd("C:/Users/laure/OneDrive/Documents/")

install.packages('RWeka')
install.packages('Seurat')

library(Seurat)
library(RWeka)

#bone_marrow <- read.arff("bone+marrow+transplant+children")
#read.csv("bone+marrow+transplant+children", header=FALSE, comment.char = "@")
bone_marrow_transplant_children <- read.arff("bone+marrow+transplant+children")


dim(bone_marrow_transplant_children)



bone_marrow_transplant_children <- CreateSeuratObject(counts = bmmc, min.cells = 5, min.features = 300)
bone_marrow_transplant_children


# normalization
bmmc <- NormalizeData(bone_marrow_transplant_children, normalization.method = "LogNormalize", scale.factor = 10000)
bmmc <- FindVariableFeatures(bone_marrow_transplant_children, selection.method = "dispersion", nfeatures = 2500)

top20 <- head(VariableFeatures(bone_marrow_transplant_children), 20)
top20


all.genes <- rownames(bone_marrow_transplant_children)
bmmc <- ScaleData(bone_marrow_transplant_children, features = all.genes)
bmmc <- RunPCA(bone_marrow_transplant_children, features = VariableFeatures(bmmc), npcs = 30)

