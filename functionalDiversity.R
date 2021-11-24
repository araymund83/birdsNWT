#begin of the code 
---
  Author: Caio Graco-Roza
output: html_document
editor_options: 
  chunk_output_type: console
---
  First, we generate the data:
  ```{r generate data}
require(tidyverse)
require(FD)
require(emstreeR)
require(ggConvexHull)
data <- FD::simul.dbFD() #Generate random data.
#pass the simulated community data. I choose community 38 and 39 because of the number of species
comm <- data.frame(data$abun[38:39,]); rownames(comm)<-c("Before","After") 
#Trait data
trait<- data.frame(data$traits);colnames(trait)<-c("T1","T2","T3")
trait <- trait %>% rownames_to_column("name") #Species name to column to use as index when we merge the comm and trait table
```
Regarding data preparation, we need to use the data in a long format for easier plotting. Also, we need only the trait 
information of the species that are present in a site so we first need to make a list object with the trait information of the
species that are present in each site.
```{r long format}
trait_comm<- comm %>%  
  rownames_to_column("Site") %>%  #We need to make site as a column for using as index when making pivot_longer()
  pivot_longer(cols=!Site) %>% #all species but not the sites
  full_join(trait,by="name") %>% #merge add species trait values to each of the lines in the table
  filter(value > 0) %>% #keep only species that are present
  mutate_at("Site", factor, levels=c("Before","After")) %>%  #Set the levels of the factor (just for the legend)
  group_by(Site) %>% #group by site to estimate the centroid
  mutate(centroid.T1 = mean(T1),
         centroid.T2 = mean(T2)) %>%  #centroid is simply the mean of coordinates in each axis
  group_by(Site,name) %>% 
  mutate(dist.cen = sqrt(sum((c(T1,T2) - c(centroid.T1,centroid.T2)) ^ 2))) %>% #calculate the distance of each species to its centroid
  ungroup() #ungroup the data
```
If one wants to estimate the centroid with abundance weights, just uncomment the lines of the following code
```{r}
#Abundance weighted centroid
trait_centroid<- trait_comm %>% 
  group_by(Site) %>% 
  #mutate(value = value/sum(value)) %>% #uncomment for weighted centroid 
  #mutate(T1 = T1 * value) %>% #uncomment for weighted centroid 
  #mutate(T2 = T2 * value) %>% #uncomment for weighted centroid 
  summarise(across(T1:T2, ~mean(.x)))
```
Then we set the plot aesthetics 
```{r}
theme_set(theme_bw())
theme_update(
  legend.text = element_text(size=10),
  legend.position ="bottom",
  legend.box="horizontal",
  legend.margin=margin(),
  panel.grid = element_blank(),
  axis.text = element_blank(),
  axis.ticks=element_blank(),
  plot.title = element_text(face = "bold", size=14),
  axis.title = element_text(size=12))
cpalette <- c("#E69F00", "#56B4E9") #The colours of the figure go here
```
# Functional identity (Plot A)
```{r}
(F_id<- trait_comm %>% 
    ggplot(aes(x=T1,y=T2, colour=Site))+
    geom_point(aes(size=value), alpha=.7) +
    geom_point(data=trait_centroid, aes(x=T1,y=T2,fill=Site), shape=22,colour="black",size=7)+
    labs(x="Trait 1", y="Trait 2", colour = "Species", size= "Abundance",title="Functional identity", fill="Centroid")+
    guides(colour=guide_legend(order=1, nrow=1), size=guide_legend(order=2, nrow=1)) +
    scale_colour_manual(values=cpalette)+
    scale_fill_manual(values=cpalette)+
    scale_size(range=c(1,7))+
    coord_fixed(ylim=c(0,1),xlim=c(0,1)))
```
#Funcional richnes
Usually the convexhulls are estimated by the {geometry} pachage with the function convhulln. However, we only need species coordinates to make a plot.
#We will need the package ggConvexhull. install with <devtools::install_github("cmartin/ggConvexHull")>
```{r}
(F_rich<- trait_comm %>% 
    ggplot(aes(x=T1,y=T2, colour= Site, fill=Site))+
    geom_convexhull(alpha=.3)+
    geom_point(shape=19,size = 2) +
    geom_point(data=trait_centroid, aes(x=T1,y=T2,fill=Site),shape=22, colour="black",size=7)+
    labs(x="Trait 1", y="Trait 2", colour = "Species", size= "Abundance",title="Functional Richness")+
    scale_colour_manual(values=cpalette)+
    scale_fill_manual(values=cpalette)+
    coord_fixed(ylim=c(0,1),xlim=c(0,1)) +
    theme(legend.position="none"))
```
# Funcional eveness
Functional evennes is a bit more tricky because we need to estimate the minimum spanning tree of the communities. For this we will use the emstreeR package.
```{r}
library(emstreeR)
#first for site one
mst_before <- trait_comm %>% filter(Site == "Before") %>%  select(T1,T2) %>%  data.frame() %>%   ComputeMST()
#after for size two
mst_after  <- trait_comm %>% filter(Site == "After") %>%  select(T1,T2) %>%  data.frame() %>%   ComputeMST()
#then we rbind the table 
mst_total <-  bind_rows(mst_before, mst_after) #do.call(rbind, list(mst_before,mst_after)) also works
#One could also run all within an lapply function if the number of sites is too large for spliting manually
split_trait <- split(trait_comm, trait_comm$Site) #splits dataframe by site and outputs a list 
list_MST <- lapply(split_trait, function(x) ComputeMST(x[,c("T1","T2")]))
mst_total <- map_df(list_MST, ~ data.frame(.x), .id="Site") #combine the list and create a column with the index of each list for easing the interpretation of the table 
#add columns to dataset
trait_comm$from <- mst_total$from
trait_comm$to <- mst_total$to
```
# Functional evenness
For the plot of the minimum spanning tree we will use the `stat_MST()`. We need one line of code for each stat_MST plot if we want to change the figure color. I still dont know how to solve this, but when I find out I will update the code.
```{r}
(F_eve <- trait_comm %>% ggplot(aes(x=T1,y=T2, colour=Site, group=Site)) +
    geom_point(aes(size=value,fill=Site), alpha=.7, shape=21)+
    stat_MST(data = trait_comm %>%  filter(Site == "Before"), aes(x=T1,y=T2,from=from,to=to), colour=cpalette[1])+
    stat_MST(data = trait_comm %>%  filter(Site == "After"), aes(x=T1,y=T2,from=from,to=to), colour=cpalette[2])+
    labs(x="Trait 1", y="Trait 2", colour = "Species", fill = "Species",size= "Abundance", title="Functional Eveness", subtitle = "Dependency: *emstreeR*")+
    guides(colour=guide_legend(order=1, nrow=1), size=guide_legend(order=2, nrow=1)) +
    scale_colour_manual(values=cpalette)+
    scale_fill_manual(values=cpalette)+
    scale_size(range=c(1,7))+
    coord_fixed(ylim=c(0,1),xlim=c(0,1))+
    theme(legend.position = "none"))
```
# Functional divergence 
According to Mouillout et a. 2013 the divergence measures the relative contribution of the most extreme species to the community. For visualizing this, the authors make a circle with a radius equivalent to the distance from the most abundant species to its centroid. Larger radius mean that the most abundant species is far from the centroid and therefore is an functional outlier. As we already estimated the distance of the species to their centroids, we just need to select the most abundance one and see the distance
```{r}
#Most abundant species
trait_max <- trait_comm %>% 
  group_by(Site) %>% 
  top_n(n=1, wt=value)
(F_div<- trait_comm %>% 
    group_by(Site) %>% 
    mutate(centroid.T1 = mean(T1),
           centroid.T2 = mean(T2)) %>% 
    group_by(Site,name) %>% 
    mutate(dist.cen = sqrt(sum((c(T1,T2) - c(centroid.T1,centroid.T2)) ^ 2))) %>% 
    ggplot(aes(x=T1,y=T2))+
    geom_point(aes(size=value, colour=Site), alpha=.3) +
    geom_point(data=trait_centroid, aes(x=T1,y=T2,fill=Site),shape=22, colour="black",size=7)+
    stat_ellipse(data = subset(trait_comm, Site == "Before"), aes(x=T1,y=T2), type="euclid", level=trait_max$dist.cen[1], colour=cpalette[1])+
    stat_ellipse(data = subset(trait_comm, Site == "After"), aes(x=T1,y=T2), type="euclid", level=trait_max$dist.cen[2], colour=cpalette[2])+
    scale_colour_manual(values=cpalette)+
    scale_fill_manual(values=cpalette)+
    scale_size(range=c(1,7))+
    labs(x="Trait 1", y="Trait 2", colour = "Species", size= "Abundance",title="Functional divergence")+
    guides(colour=guide_legend(order=1, nrow=1), size=guide_legend(order=2, nrow=1)) +
    coord_fixed(ylim=c(0,1),xlim=c(0,1))+
    theme(legend.position = "none"))
```
# Functional dispersion
The dispersion is simply the average distance of all species to their centroid. We already have the distance estimated, we just need to use `geom_segment()` to draw the lines. I also like to outline the convex hull (this is optional) so I leave the `geom_convexhull` here too
```{r}
(F_disp<- trait_comm %>% 
    group_by(Site) %>% 
    mutate(centroid.T1 = mean(T1),
           centroid.T2 = mean(T2)) %>% 
    group_by(Site,name) %>% 
    mutate(dist.cen = sqrt(sum((c(T1,T2) - c(centroid.T1,centroid.T2)) ^ 2))) %>% 
    ggplot(aes(x=T1,y=T2, colour=Site))+
    geom_convexhull(fill=NA)+
    geom_point(aes(size=value, colour=Site), alpha=.3) +
    geom_segment(aes(x=T1,xend=centroid.T1,y=T2,yend=centroid.T2), size=.1, linetype=2)+
    geom_point(data=trait_centroid, aes(x=T1,y=T2,fill=Site),shape=22, colour="black",size=7)+
    scale_colour_manual(values=cpalette)+
    scale_fill_manual(values=cpalette)+
    scale_size(range=c(1,7))+
    labs(x="Trait 1", y="Trait 2", colour = "Species", size= "Abundance",title="Functional dispersion")+
    guides(colour=guide_legend(order=1, nrow=1), size=guide_legend(order=2, nrow=1)) +
    coord_fixed(ylim=c(0,1),xlim=c(0,1))+
    theme(legend.position = "none"))
```
# Functional entropy 
The entropy is based on the Rao quadratic index, so to measure the entropy we will estimate the abundance differece between all species and connect the species with lines.  I don't like this plot much, but since it is in the paper I am reproducing it too. 
```{r}
#We first redo the trait data because we modified it above 
trait <- data.frame(data$traits); colnames(trait)<-c("T1","T2","T3")
#Here we make a table of pairwise coordinates and absolute differences in abundance (it can be the mean, but I am not sure)
entropy_data<-sapply(c("Before","After"), function(j){
subtrait <- trait[colnames(comm)[which(comm[j,]>0)],] #subtrait means only the the trait information of species present in site j
subcomm <- comm[j,which(comm[j,] > 0)]#subcomm means only the the abundance information of species present in site j
comb_dat<-t(combn(nrow(subtrait),2)) #combinatory matrix to extract the correct values for each possible combination
entropy_dat<-matrix(NA, nrow=choose(nrow(subtrait),2), ncol=5) #create empty matrix
colnames(entropy_dat) <- c("T1","T1_end","T2","T2_end","abun")
for (i in 1:nrow(entropy_dat)){ #fill it with for loop
  entropy_dat[i,] <- c(subtrait[comb_dat[i,1],1], #coordinate x of spp 1
                       subtrait[comb_dat[i,2],1],#coordinate x of spp 2
                       subtrait[comb_dat[i,1],2], #coordinate y of spp 1
                       subtrait[comb_dat[i,2],2], #coordinate y of spp 2
                       as.numeric(abs(subcomm[comb_dat[i,1]]-subcomm[comb_dat[i,2]]))) #difference in abundance (absolute)
}
return(entropy_dat)
}, simplify=FALSE, USE.NAMES=TRUE) #return a list object with all pairwise for each site
pair_data_ent <- map_df(entropy_data, ~ data.frame(.x), .id="Site") #combine lists 
```
Finally we can plot the data 
```{r}
 #Here, the size of the line depends on the abundance difference between species
(F_ent<- 
    trait_comm %>% 
    ggplot(aes(x=T1,y=T2, colour=Site))+
    geom_segment(data = pair_data_ent, aes(x=T1,xend=T1_end,y=T2,yend=T2_end, alpha=abun, colour=Site))+
    geom_point(aes(size=value), alpha=.5)+
    geom_point(data=trait_centroid, aes(x=T1,y=T2,fill=Site),shape=22, colour="black",size=7)+
    scale_colour_manual(values=cpalette)+
    scale_fill_manual(values=cpalette)+
    labs(x="Trait 1", y="Trait 2", colour = "Species", size= "Abundance",title="Functional entropy")+
    guides(colour=guide_legend(order=1, nrow=1), size=guide_legend(order=2, nrow=1))+
  coord_fixed()+
  theme(legend.position = "none"))
```
# Functional specialization 
How each species is different from the overall mean trait value 
```{r}
(F_esp <- trait_comm %>% 
  mutate(overall.cen.T1 = mean(T1),
         overall.cen.T2 = mean(T2)) %>% 
  ggplot(aes(x=T1,y=T2, colour=Site))+
  geom_point(aes(size=value), alpha=.7)+
  geom_segment(aes(xend=T1,x=overall.cen.T1,yend=T2,y=overall.cen.T2), arrow=arrow(length = unit(0.01, "npc")), colour="black", size=.2)+
  scale_colour_manual(values=cpalette)+
  scale_fill_manual(values=cpalette)+
  labs(x="Trait 1", y="Trait 2", colour = "Species", size= "Abundance",title="Functional especialization")+
  guides(colour=guide_legend(order=1, nrow=1), size=guide_legend(order=2, nrow=1))+
  coord_fixed()+
   theme(legend.position = "none"))
```
# Functional originality
The distance of the species from its nearest functional neighbour.
We will use same approach as in the entropy, but now we keep for each species, only the line with the closes neightbour. Note that instead of combn we now use expand grid, because the order of the comparison does not matter, meaning that even if sp60 is the nearest neighbour of sp36, we still want to evaluate the nearest neighbour using sp60 as the focal one.
```{r}
trait <- data.frame(data$traits); colnames(trait)<-c("T1","T2","T3")
ori_data<-sapply(c("Before","After"), function(j){
  subtrait <- trait[colnames(comm)[which(comm[j,]>0)],]
  subcomm <- comm[j,which(comm[j,] > 0)]
  comb_dat<-expand.grid(rownames(subtrait),rownames(subtrait))
  entropy_dat<-matrix(NA, nrow(comb_dat), ncol=7)
  colnames(entropy_dat) <- c("Focal_spp_1","Focal_spp_2","T1","T1_end","T2","T2_end","dist")
  for (i in 1:nrow(comb_dat)) {
    entropy_dat[i, ] <- c(
      rownames(subtrait)[comb_dat[i,1]],
      rownames(subtrait)[comb_dat[i,2]],#I need to add the name of the species as an index for later
      subtrait[comb_dat[i,1], 1],
      subtrait[comb_dat[i,2], 1],
      subtrait[comb_dat[i,1], 2],
      subtrait[comb_dat[i,2], 2],
      as.numeric(dist(rbind(
        subtrait[comb_dat[i,1], 1:2], subtrait[comb_dat[i,2],1:2 ]
      )))
    )
    }
  return(entropy_dat)
}, simplify=FALSE, USE.NAMES=TRUE)
ori_data<- lapply(ori_data, function(x){ data.frame(x) %>% 
         mutate_at(vars(-starts_with("Focal_spp")), as.numeric) %>% 
         mutate_at(vars(starts_with("Focal_spp")), as.factor)})
pair_data_ori <- map_df(ori_data, ~ data.frame(.x), .id="Site") %>% 
  group_by(Site, Focal_spp_1) %>%  #we group by site and species so we have only one pair of species per site
   slice_min(dist,n=2) %>% 
  filter(dist > 0)
```
Plot the originality 
```{r}
(F_ori <- trait_comm %>% 
    mutate(overall.cen.T1 = mean(T1),
           overall.cen.T2 = mean(T2)) %>% 
    ggplot(aes(x=T1,y=T2, colour=Site))+
    geom_point(aes(size=value), alpha=.7)+
  # ggrepel::geom_text_repel(aes(label=name))+
    geom_segment(data= pair_data_ori, aes(xend=T1,x=T1_end,yend=T2,y=T2_end, colour=Site), size=.2)+
     scale_colour_manual(values=cpalette)+
     scale_fill_manual(values=cpalette)+
    scale_size(range=c(0,7))+
    labs(x="Trait 1", y="Trait 2", colour = "Species", size= "Abundance",title="Functional originality")+
    guides(colour=guide_legend(order=1, nrow=1), size=guide_legend(order=2, nrow=1))+
    coord_fixed()+
    theme(legend.position = "none"))
```
Now we combine the plots using {patchwork}
```{r}
library(patchwork)
F_id + F_rich + F_eve + F_div + F_disp + F_ent + F_esp + F_ori + plot_layout(nrow=2, guides="collect") +plot_annotation(tag_levels = "A", title = "Functional diversity metrics")
```