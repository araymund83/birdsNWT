library("BAT") #version 2.0.1 #update.packages("BAT") #<--- check out that you have the version 2.0.1
library("FD") #version 1.1-12
library("ggplot2")
library("gridExtra")
library("labdsv")
library("hypervolume") #version 2.0.11
library("psych")
library("StatMatch")
library("TPD")

# Converting traits to a distance matrix(gower distance   -----------------
#and transforming them with a PCoA
gower.birds <- gower.dist(traits)
euc.pco <- pco(gower.birds,k=4)

barplot(euc.pco$eig)
(euc.pco$eig[1]+euc.pco$eig[2]+euc.pco$eig[3])/sum(euc.pco$eig[euc.pco$eig > 0]) #0.955 variance explained by first 3 axes

traits <- euc.pco$points
row.names(traits) <- row.names(traits)
#################################################################
## Calculating the functional alpha diversity of each plot using the kernel.alpha function

kernelFD <- BAT::kernel.alpha(comm=CanESM2_2011,trait=traits,abund=TRUE,method="gaussian",return.hv=TRUE) 
#abund= TRUE because samples are abundances
#return.hv= TRUE so to save the hypervolumes for subsequent analyses

results <- data.frame(Plot_type=as.character(urb),Richness=as.numeric(kernelFD[[1]])) ##extracting the functional data
traitsFD <- FD::functcomp(traits, as.matrix(CanESM2_2011), CWM.type = 'dom')

dummy$trait
dist <- gawdis::gawdis(traits[,c(1,2,4)], w.type = 'analytic')
round(ex1, 3)##just to see only 3 decimals, enough

traitsFD<- qs::qread('./tables/qs/comm/functComp2011.qs')


euc.pco <- pco(traitsFD,k=4)

trait.pca <- prcomp(traitsFD, center = TRUE, scale = TRUE)

#create a data frame with scores
scores  <- as.data.frame(trait.pca$x)
#plot observations
pcaPlot <- ggplot(data = scores, aes (x = PC1, y = PC2, label = rownames(scores))) +
  geom_hline(yintercept = 0, colour = 'gray65') +
  geom_vline(xintercept =0, colour = 'gray65') +
  geom_text(colour = 'tomato', alpha = 0.8, size = 4) +
  ggtitle('PCa plot for bird traits in the NWT')

library(devtools)
install_github("vqv/ggbiplot")

library(ggbiplot)
g <- ggbiplot(trait.pca, obs.scale = 1, var.scale = 1, 
              ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

