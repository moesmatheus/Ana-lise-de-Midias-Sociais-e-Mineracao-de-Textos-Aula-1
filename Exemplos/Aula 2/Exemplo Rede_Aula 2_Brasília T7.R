## FGV-Management - 2S2020
## Análise de Mídias Sociais e Mineração de Texto - Aula 2
## Prof. Eduardo de Rezende Francisco
## Data: 25/Agosto/2020                    

# Extensões para Análise de Redes
#(devem ser previamente baixadas no CRAN do R)

#install.packages("network")
library(network)
#install.packages("sna")
library(sna)

# Carrega algumas redes

rede0 <- read.table("c:/temp/Exemplo0.csv",header=TRUE,sep = ";", dec=",")
rede0 <- as.data.frame(rede0)
grede0 <- rede0[,2:6]
rownames(grede0) <- rede0[,1]

rede1 <- read.table("c:/temp/Exemplo1.csv",header=TRUE,sep = ";", dec=",")
rede1 <- as.data.frame(rede1)
grede1 <- rede1[,2:14]
rownames(grede1) <- rede1[,1]

rede2 <- read.table("c:/temp/Exemplo2.csv",header=TRUE,sep = ";", dec=",")
rede2 <- as.data.frame(rede2)
grede2 <- rede2[,2:14]
rownames(grede2) <- rede2[,1]

rede3 <- read.table("c:/temp/Exemplo3.csv",header=TRUE,sep = ";", dec=",")
rede3 <- as.data.frame(rede3)
grede3 <- rede3[,2:14]
rownames(grede3) <- rede3[,1]

rede4 <- read.table("c:/temp/Exemplo4.csv",header=TRUE,sep = ";", dec=",")
rede4 <- as.data.frame(rede4)
grede4 <- rede4[,2:14]
rownames(grede4) <- rede4[,1]

rede5 <- read.table("c:/temp/Exemplo5.csv",header=TRUE,sep = ";", dec=",")
rede5 <- as.data.frame(rede5)
grede5 <- rede5[,2:14]
rownames(grede5) <- rede5[,1]

# Desenhando as redes
gplot(grede0,gmode="graph",displaylabels = TRUE,edge.col="gray",usearrows=FALSE)

par(mfrow=c(2, 2))
gplot(grede1,gmode="graph",displaylabels = TRUE,edge.col="gray",usearrows=FALSE)
gplot(grede2,gmode="graph",displaylabels = TRUE,edge.col="gray",usearrows=FALSE)
gplot(grede3,gmode="graph",displaylabels = TRUE,edge.col="gray",usearrows=FALSE)
gplot(grede4,gmode="graph",displaylabels = TRUE,edge.col="gray",usearrows=FALSE)
gplot(grede5,gmode="graph",displaylabels = TRUE,edge.col="gray",usearrows=FALSE)

gden(grede0, g=NULL, diag=FALSE, mode="graph", ignore.eval=FALSE)
gden(grede1, g=NULL, diag=FALSE, mode="graph", ignore.eval=FALSE)
gden(grede2, g=NULL, diag=FALSE, mode="graph", ignore.eval=FALSE)
gden(grede3, g=NULL, diag=FALSE, mode="graph", ignore.eval=FALSE)
gden(grede4, g=NULL, diag=FALSE, mode="graph", ignore.eval=FALSE)

network.density(network(grede0))
network.density(network(grede1))
network.density(network(grede2))
network.density(network(grede3))
network.density(network(grede4))
network.density(network(grede5))

get.neighborhood(network(grede3),4,type="combined")

efficiency(grede0, g=NULL, diag=FALSE)
efficiency(grede1, g=NULL, diag=FALSE)
efficiency(grede2, g=NULL, diag=FALSE)
efficiency(grede3, g=NULL, diag=FALSE)
efficiency(grede4, g=NULL, diag=FALSE)

efficiency(grede3, g=NULL, diag=FALSE)
efficiency(grede5, g=NULL, diag=FALSE)

connectedness(grede1)
connectedness(grede2)
connectedness(grede3)
connectedness(grede4)

mutuality(grede1)
mutuality(grede2)
mutuality(grede3)
mutuality(grede4)

## Explorar trocando os valores dos parâmetros

clique.census(grede3, mode = "graph", tabulate.by.vertex = FALSE,
    clique.comembership = "sum", enumerate = FALSE,
    na.omit = TRUE)

clique.census(grede3, mode = "graph", tabulate.by.vertex = TRUE,
    clique.comembership = "none", enumerate = FALSE,
    na.omit = TRUE)

clique.census(grede3, mode = "graph", tabulate.by.vertex = TRUE,
    clique.comembership = "bysize", enumerate = FALSE,
    na.omit = TRUE)

clique.census(grede3, mode = "graph", tabulate.by.vertex = FALSE,
    clique.comembership = "none", enumerate = TRUE,
    na.omit = TRUE)

clique.census(grede2, mode = "graph", tabulate.by.vertex = FALSE,
    clique.comembership = "sum", enumerate = FALSE,
    na.omit = TRUE)

clique.census(grede4, mode = "graph", tabulate.by.vertex = FALSE,
    clique.comembership = "sum", enumerate = FALSE,
    na.omit = TRUE)

# Distância Geodésica
dg3 <- geodist(grede3)
dg3
dg3$counts
dg3$gdist

mdg3 <- dg3$gdist
colnames(mdg3) <- colnames(grede3)
rownames(mdg3) <- rownames(grede3)
mdg3

dg2 <- geodist(grede2)
dg4 <- geodist(grede4)



# Explorando a rede - centralidades
degree(grede3,gmode="graph",cmode="indegree")
closeness(grede3,gmode="graph")
betweenness(grede3,gmode="graph")

