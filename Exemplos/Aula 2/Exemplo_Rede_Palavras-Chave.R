## FGV-Management - 2S2020
## Análise de Mídias Sociais e Mineração de Texto - Aula 2
## Prof. Eduardo de Rezende Francisco
## Data: 25/Agosto/2020                        

## Exemplo de Análise da Rede de Palavras-Chave do Acervo da RAE

setwd("E:/Aula 2")

## Carrega as extensões NETWORK e SNA (os arquivos com as extensões devem ter sido
## previamente baixados do site do R (CRAN))
library(network)
library(sna)
#library(rgl)

# lê a base de palavras-chave do acervo da RAE
base_RAE <- read.table("Tabela_RAE.txt",header=TRUE,sep="\t")
colnames(base_RAE)
colnames(base_RAE)[1] <- "id"
colnames(base_RAE)[3] <- "PC"
colnames(base_RAE)
PC <- base_RAE$PC
length(PC)

# obtém o máximo de palavras-chave por artigo
maximo <- 1
posmax <- 1
for (i in 1:length(PC))
{
  x <- as.character(PC[i])
  x <- gsub("[.]","",x)
  x <- gsub("\"","",x)                # aspas duplas
  x <- gsub("\'","",x)                # aspas simples
  y <- unlist(strsplit(x,";"))
  y <- unlist(strsplit(y,","))
  y <- tolower(sub("[[:space:]]*","",sub(" +$", "", y))) 
  if (length(y) > maximo) {
    maximo <- length(y)
    posmax <- i
  }  
}

# variável maximo traz a quantidade máxima de palavras-chave
# que um artigo teve no acervo
maximo

# constrói a matriz de palavras-chave por artigo
# em formato matriz e também data.frame (com id do artigo)
matriz_pc_artigo <- matrix(data=NA,nrow=length(PC),ncol=maximo)
tab_pc_artigo <- data.frame(id_artigo=base_pc$id,qpc=rep(0,length(PC)),
                            pc1=NA,pc2=NA,pc3=NA,pc4=NA,pc5=NA,pc6=NA,
                            pc7=NA,pc8=NA,pc9=NA,pc10=NA,pc11=NA)

for (i in 1:length(PC))
{
  x <- as.character(PC[i])
  x <- gsub("[.]","",x)
  x <- gsub("\"","",x)                # aspas duplas
  x <- gsub("\'","",x)                # aspas simples
  y <- unlist(strsplit(x,";"))
  y <- unlist(strsplit(y,","))
  y <- tolower(sub("[[:space:]]*","",sub(" +$", "", y)))
  # substitui "x" por NA
  y[y=="x"] <- NA
  matriz_pc_artigo[i,1:length(y)] <- y
  if (!is.na(y[1])) {
    tab_pc_artigo$qpc[i] <- length(y)
    tab_pc_artigo[i,3:(2+length(y))] <- y
  }
}

# salva tabela com as palavras-chave por artigo
write.table(tab_pc_artigo,file="tab_pc_artigo.csv",quote=FALSE,sep=";",eol="\n",
            na="",row.names=FALSE)


# constrói lista de palavras-chave

pc1 <- as.vector(tab_pc_artigo$pc1)
pc2 <- as.vector(tab_pc_artigo$pc2)
pc3 <- as.vector(tab_pc_artigo$pc3)
pc4 <- as.vector(tab_pc_artigo$pc4)
pc5 <- as.vector(tab_pc_artigo$pc5)
pc6 <- as.vector(tab_pc_artigo$pc6)
pc7 <- as.vector(tab_pc_artigo$pc7)
pc8 <- as.vector(tab_pc_artigo$pc8)
pc9 <- as.vector(tab_pc_artigo$pc9)
pc10 <- as.vector(tab_pc_artigo$pc10)
pc11 <- as.vector(tab_pc_artigo$pc11)

# lista com as palavras-chave obtidas da tabela KEYWORD.TXT
lista_pc <- c(pc1,pc2,pc3,pc4,pc5,pc6,pc7,pc8,pc9,pc10,pc11)
lista_pc <- lista_pc[!is.na(lista_pc)]
lista_pc <- lista_pc[nchar(lista_pc)>0]
lista_pc_original <- sort(lista_pc)
lista_pc <- sort(rownames(table(lista_pc)))
lista_pc <- sort(lista_pc)

# apresenta as 50 palavras-chave mais frequentes dos artigos
sort(table(lista_pc_original),TRUE)[1:50]

# total de palavras-chave
length(lista_pc_original)

# total de palavras-chave únicas
length(table(lista_pc_original))

# cria tabela com as palavras-chaves e suas respectivas frequências
tab_freq_pc <- data.frame(pc=names(table(lista_pc_original)),
                          freq=as.vector(table(lista_pc_original)))
write.table(tab_freq_pc,file="tab_freq_pc.csv",quote=FALSE,sep=";",eol="\n",
            na="",row.names=FALSE)

# lista com apenas as palavras-chave que apareceram duas ou mais vezes
lista_pc_1 <- rownames(table(lista_pc)[table(lista_pc_original)==1])
length(lista_pc_1)
lista_pc_2oumais <- rownames(table(lista_pc)[table(lista_pc_original)>1])
length(lista_pc_2oumais)
lista_pc_3oumais <- rownames(table(lista_pc)[table(lista_pc_original)>2])
length(lista_pc_3oumais)
lista_pc_4oumais <- rownames(table(lista_pc)[table(lista_pc_original)>3])
length(lista_pc_4oumais)

## Separa as palavras-chaves compostas
lista_pc_simples <- unlist(strsplit(lista_pc_original," "))
sort(table(lista_pc_simples),TRUE)[1:60]


# MONTA MATRIZ DE CO-OCORRÊNCIA DE PALAVRAS-CHAVE

MKey <- matrix(0,nrow=length(lista_pc),ncol=length(lista_pc),
               dimnames=list(lista_pc,lista_pc))

for (i in 1:length(matriz_pc_artigo[,1]))
{
   if (tab_pc_artigo$qpc[i] > 1)
   {
     pcs_artigo <- names(sort(table(matriz_pc_artigo[i,1:(tab_pc_artigo$qpc[i])])))
     MKey[pcs_artigo,pcs_artigo] <- MKey[pcs_artigo,pcs_artigo] + 1
     for (j in pcs_artigo)
       MKey[j,j] <- MKey[j,j] - 1
   }
}

x <- 0
y <- 0
for (i in 1:2383) {
   for (j in 1:2383) {
      if (MKey[i,j] == 7) {
        x <- i
        y <- j   
      }
   }
}


# componentes
table(component.dist(MKey)$membership)

# componentes por tamanho
table(as.vector(table(component.dist(MKey)$membership)))


# seleciona as palavras que ocorrem 
MKey2 <- MKey[lista_pc_2oumais,lista_pc_2oumais]
MKey3 <- MKey[lista_pc_3oumais,lista_pc_3oumais]
MKey4 <- MKey[lista_pc_4oumais,lista_pc_4oumais]

# Desenha apenas a rede com as palavras-chaves que ocorrem 4 ou mais vezes
rede <- network(MKey4,directed=FALSE)
labels_rede <- network.vertex.names(rede)
plot(rede)

plot(rede,mode="fruchtermanreingold") ## padrão. Vou usar este.
plot(rede, mode= "circle")
plot(rede, mode= "kamadakawai")

# Desenha toda a rede (exceto componentes com 1 palavra-chave)
rede <- network(MKey,directed=FALSE)
labels_rede <- network.vertex.names(rede)
plot(rede)

# Analisa a estrutura da rede
network.edgecount(network(MKey,directed=FALSE))
network.density(network(MKey,directed=FALSE))

# Analisa a quantidade de componentes da rede
table(component.dist(MKey)$membership)
length(table(component.dist(MKey)$membership)) #quantidade de componentes
freqcomp2oumais <- table(component.dist(MKey)$membership)[table(component.dist(MKey)$membership)>=2]
freqcomp2oumais
comp2oumais <- as.integer(names(freqcomp2oumais))
length(comp2oumais) # quantidade de componentes com 2 ou mais nós 
comp2oumais

# explorando o desenho da rede
f.desenhacomp <- function(componente)
{
  if (length(componente)==1)
    M <- MKey[rownames(MKey)[component.dist(MKey)$membership==componente],
              rownames(MKey)[component.dist(MKey)$membership==componente]]
  else
    M <- MKey[rownames(MKey)[component.dist(MKey)$membership %in% as.list(componente)],
              rownames(MKey)[component.dist(MKey)$membership %in% as.list(componente)]]

  redecomp <- network(M,directed=FALSE)
  coords_redecomp <- gplot(redecomp, boxed.labels=TRUE,displaylabels=FALSE,arrow=FALSE,label.cex=0.5)
  
  return (coords_redecomp)
}

MPC <- MKey[rownames(MKey)[component.dist(MKey)$membership %in% as.list(comp2oumais)],
            rownames(MKey)[component.dist(MKey)$membership %in% as.list(comp2oumais)]]

gMPC <- degree(MPC,gmode="graph")            

redePC <- network(MPC,directed=FALSE)
coords_redePC <- gplot(redePC, boxed.labels=TRUE,displaylabels=FALSE,arrow=FALSE,label.cex=0.5)

Tab2DKeyword_total <- data.frame(Keyword=rownames(MPC),
                                 X=coords_redePC[,1],
                                 Y=coords_redePC[,2],
                                 Freq=tab_freq_pc[,2],
                                 CentrGrau=gMPC)
write.table(Tab2DKeyword_total,file="Nós_KEYWORDS.txt",quote=FALSE,sep=",",eol="\n",row.names=FALSE)

table(Tab2DKeyword_total$Freq)
table(Tab2DKeyword_total$CentrGrau)

# Cria lista para ser exportada para ArcView de arestas 2D da rede
#TabArestas2dKeyword <- data.frame(id=1:(sum(MKey>=1)/2),
TabArestas2dKeyword <- data.frame(id=1:100000,
                                  KW1="",
                                  KW2="",
                                  Valor=0,
                                  X_KW1=0,
                                  Y_KW1=0,
                                  X_KW2=0,
                                  Y_KW2=0,stringsAsFactors = FALSE)

k <- 0
for (i in 1:nrow(MKey)) {
   for (j in i:ncol(MKey)) {
      valor <- MKey[i,j]
      if (valor >= 1) {
        k <- k + 1
        TabArestas2dKeyword$KW1[k] <- dimnames(MKey)[[1]][i]
        TabArestas2dKeyword$KW2[k] <- dimnames(MKey)[[1]][j]
        TabArestas2dKeyword$Valor[k] <- valor
        TabArestas2dKeyword$X_KW1[k] <- Tab2DKeyword_total$X[i]
        TabArestas2dKeyword$Y_KW1[k] <- Tab2DKeyword_total$Y[i]
        TabArestas2dKeyword$X_KW2[k] <- Tab2DKeyword_total$X[j]
        TabArestas2dKeyword$Y_KW2[k] <- Tab2DKeyword_total$Y[j]
      }
   }
}
TabArestas2dKeyword <- TabArestas2dKeyword[1:k,] 
write.table(TabArestas2dKeyword,file="Arestas_KEYWORDS.txt",quote=FALSE,sep=",",eol="\n",row.names=FALSE)

TabArestas2dKeyword4 <- data.frame(id=1:100000,
                                  KW1="",
                                  KW2="",
                                  Valor=0,
                                  stringsAsFactors = FALSE)

k <- 0
for (i in 1:nrow(MKey4)) {
   for (j in i:ncol(MKey4)) {
      valor <- MKey4[i,j]
      if (valor >= 1) {
        k <- k + 1
        TabArestas2dKeyword4$KW1[k] <- dimnames(MKey4)[[1]][i]
        TabArestas2dKeyword4$KW2[k] <- dimnames(MKey4)[[1]][j]
        TabArestas2dKeyword4$Valor[k] <- valor
      }
   }
}
TabArestas2dKeyword4 <- TabArestas2dKeyword4[1:k,] 
write.table(TabArestas2dKeyword4,file="Arestas_2D_KEYWORDS4_2003_2016.txt",quote=FALSE,sep=",",eol="\n",row.names=FALSE)

comp1 <- rownames(MKey)[component.dist(MKey)$membership==2]
Mcomp1 <- MKey[comp1,comp1]

betMKey <- sna::betweenness(MKey,gmode="graph",ignore.eval=T)

TabBet <- data.frame(Keyword=rownames(MKey),
                           CentrBet=betMKey)
write.table(TabBet,file="Betweenness_KEYWORDS_global.txt",quote=FALSE,sep=",",eol="\n",row.names=FALSE)



coords_comp2oumais <- f.desenhacomp(comp2oumais)

plot(network(MKey[rownames(MKey)[component.dist(MKey)$membership==1],rownames(MKey)[component.dist(MKey)$membership==1]],directed=FALSE))

# Analisa os componentes da rede

# Componente Principal (2)
comp2 <- rownames(MKey)[component.dist(MKey)$membership==2]
comp1 <- rownames(MKey)[component.dist(MKey)$membership %in% comp2oumais]
Mcomp1 <- MKey[comp1,comp1]

# Analisa a estrutura do componente principal da rede (2)
network.edgecount(network(Mcomp1,directed=FALSE))
network.density(network(Mcomp1,directed=FALSE))
betMcomp1 <- sna::betweenness(Mcomp1,gmode="graph",ignore.eval=T)
gMcomp1 <- degree(Mcomp1,gmode="graph")


Rede2d <- gplot(MKey3,boxed.labels=TRUE,displaylabels=FALSE,arrow=FALSE,label.cex=0.5)
Rede2d

Rede3d <- gplot3d(MKey4,displaylabels=FALSE)
Rede3d

# Exporta rede e medidas de centralidade das palavras-chave
Tab2DKeyword <- data.frame(Keyword=rownames(MKey),
                           X=Rede2d[,1],
                           Y=Rede2d[,2],
                           CentrGrau=TabGrauKeyword$CentrGrau,
                           CentrBet=TabGrauKeyword$CentrBet)
write.table(Tab2DKeyword,file="Rede2D_KEYWORDS.txt",quote=FALSE,sep=",",eol="\n",row.names=FALSE)
