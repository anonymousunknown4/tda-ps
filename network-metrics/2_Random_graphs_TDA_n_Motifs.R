

rm(list = ls())


setwd("C:/Users/doforib/Desktop/Attack trials")

source("Resil_func.R")


###################
#=== LIBRARIES ===#
###################

library(igraph)
library(TDA)


###################
#=== CONSTANTS ===#
###################

frac = seq(0,0.7, by = 0.1)

nd = 100 # node
ed = 200 # edge


edge_weights = runif(ed)

#######################################################################################################

#####################
#=== GRAPH TYPES ===#
#####################

# 1. Erdos-Renyi #

ER_G           = sample_gnm(nd, ed, directed = FALSE, loops = FALSE)
E(ER_G)$weight = edge_weights


# 2. Small world model #

ne = 2
p  = 0.08

smallW_G0 = sample_smallworld(1, nd, ne, p)
SW_G      = simplify(smallW_G0, remove.multiple = TRUE, remove.loops = TRUE)
 
E(SW_G)$weight = edge_weights

#######################################################################################################

#=== GRAPH PLOTS ===#

op = par(mfrow = c(1,2))

igraph.options(vertex.size=7, edge.arrow.size=2, edge.color = "black",vertex.label=NA) 
plot(ER_G, main = "ER_G")

igraph.options(vertex.size=7, edge.arrow.size=2, edge.color = "black",vertex.label=NA) 
plot(SW_G, main = "SW_G")

par(op)

#######################################################################################################


################
#=== MOTIFS ===#
################


#=== 1. NODE ===#

# Degree #
NodeM_degree = Resilience_Attacks2(attack = "node", graph_type = "weighted", analysis = "Motifs", type = "degree", ER_G, frac)
ERNodeM_degreeMatrix = as.matrix(NodeM_degree)
colnames(ERNodeM_degreeMatrix) = c("fr", "Tot_M", "M1", "M2", "M3", "M4", "M5", "C_M1", "C_M2", "C_M3", "C_M4", "C_M5")


NodeM_degree = Resilience_Attacks2(attack = "node", graph_type = "weighted", analysis = "Motifs", type = "degree", SW_G, frac)
SWNodeM_degreeMatrix = as.matrix(NodeM_degree)
colnames(SWNodeM_degreeMatrix) = c("fr", "Tot_M", "M1", "M2", "M3", "M4", "M5", "C_M1", "C_M2", "C_M3", "C_M4", "C_M5")



#=== 2. EDGE ===#

# Edge Betweenness Centrality #
EdgeM_betw = Resilience_Attacks2(attack = "edge", graph_type = "unweighted", analysis = "Motifs", type = "E_betweeness", ER_G, frac)
EREdgeM_betwMatrix = as.matrix(EdgeM_betw)
colnames(EREdgeM_betwMatrix) = c("fr", "Tot_M", "M1", "M2", "M3", "M4", "M5", "C_M1", "C_M2", "C_M3", "C_M4", "C_M5")

EdgeM_betw = Resilience_Attacks2(attack = "edge", graph_type = "unweighted", analysis = "Motifs", type = "E_betweeness", SW_G, frac)
SWEdgeM_betwMatrix = as.matrix(EdgeM_betw)
colnames(SWEdgeM_betwMatrix) = c("fr", "Tot_M", "M1", "M2", "M3", "M4", "M5", "C_M1", "C_M2", "C_M3", "C_M4", "C_M5")


#############
#=== TDA ===#
#############


# Degree #
NodeTDA_degree = Resilience_Attacks2(attack = "node", graph_type = "weighted", analysis = "TDA", type = "degree", ER_G, frac)
ERNodeTDA_degreeMatrix = as.matrix(NodeTDA_degree)
colnames(ERNodeTDA_degreeMatrix) = c("fr", "Betti0", "Betti1","Wass01")

NodeTDA_degree = Resilience_Attacks2(attack = "node", graph_type = "weighted", analysis = "TDA", type = "degree", SW_G, frac)
SWNodeTDA_degreeMatrix = as.matrix(NodeTDA_degree)
colnames(SWNodeTDA_degreeMatrix) = c("fr", "Betti0", "Betti1","Wass01")



# E_Betw #
EdgeTDA_betw= Resilience_Attacks2(attack = "edge", graph_type = "weighted", analysis = "TDA", type = "E_betweeness", ER_G, frac)
EREdgeTDA_betwMatrix = as.matrix(EdgeTDA_betw)
colnames(EREdgeTDA_betwMatrix) = c("fr", "Betti0", "Betti1","Wass01")

EdgeTDA_betw= Resilience_Attacks2(attack = "edge", graph_type = "weighted", analysis = "TDA", type = "E_betweeness", SW_G, frac)
SWEdgeTDA_betwMatrix = as.matrix(EdgeTDA_betw)
colnames(SWEdgeTDA_betwMatrix) = c("fr", "Betti0", "Betti1","Wass01")






