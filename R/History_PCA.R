library(ade4)
library(tidyverse)
library(ggrepel)

######
###################
#Small function for do PCA
# pca1 <- dudi.hillsmith(Tabla3$Value1, scannf = F, nf = 5)
# summary(pca1)
#
#

#Funcion con 4 componentes del PCA
# datos = con la base de datos con sólo valores numericos
# variables = la variable categórica
#titulo = Es el titulo de la grafica
#Multiplicador =  un valor para re-escalar la gráfica

pca_biplot <- function(datos, variables, titulo, multiplicador){
  #pca1
  pca1 <- dudi.pca(as.data.frame(datos), scannf = F, nf = 3)

  pca1a <- data.frame(pca1$li)
  pca1b <- data.frame(pca1$co)
  variables <- as.factor(variables)
  titulo <- as.character(c(titulo))
  Eigen <- pca1$eig
  PC1 <- round(100*(Eigen/sum(Eigen)),1)[1]
  PC2 <- round(100*(Eigen/sum(Eigen)),1)[2]

  p1 <- ggplot(pca1a,aes(x = Axis1, y = Axis2)) +
    theme_classic() +
    geom_hline(yintercept = 0, color = "gray70") +
    geom_vline(xintercept = 0, color = "gray70") +
    geom_point(aes(color = variables), alpha = 0.7, size = 3) +
    xlab(paste(PC1, "%", sep = " ")) +
    ylab(paste(PC2, "%", sep = " ") ) +
    #xlim(-5, 6) +
    ggtitle(titulo)

  p2 <- p1 +
    geom_point(data = pca1b, aes(x = multiplicador*Comp1, y = multiplicador*Comp2), alpha = 0) +
    geom_label_repel(data = pca1b, aes(x = multiplicador*Comp1, y = multiplicador*Comp2, label = rownames(pca1b)),
                     box.padding   = 0.7, point.padding = 0.5, size = 4, alpha = 0.7) +
    geom_segment(data =  pca1b, aes(x = 0, y = 0, xend = multiplicador*Comp1, yend = multiplicador*Comp2), arrow = arrow(length = unit(0.2,"cm")), alpha = 0.7, color = "black")

  p2
}

#pca_biplot(pca1$li, pca1$co, Tabla3$Var1$Grupos, pca1$eig,"PCA Grupos de K-means", 4)
#

#ejemplo con la base de datos iris
iris
head(iris)
iris1 <- list(var = iris[,1:4], value = iris[,5])
pca_biplot(iris1$var, iris1$value, "Hola", 2)

pca1 <- dudi.pca(as.data.frame(iris1$var), scannf = F, nf = 3)
summary(pca1)

pca1$Axis1
#Ahora con mtcars
head(mtcars)
mtcars1 <- list(var = mtcars[,-10], value = mtcars[,10])

pca_biplot(mtcars1$var, mtcars1$value, "Hola :)", 2 )
