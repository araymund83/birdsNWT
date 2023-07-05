
source('./R/courseToRose.R')

cogTable <-  qs::qread(file = glue('./tables/coGpi/coGpiTable_allsp.qs'))

cogDistTable <-qs::qread(file = glue('tables/distBear_coGpi/distBear_coGpi_all72spp.qs'))
qs::qsave(x =cogDistTable, file = glue('tables/distBear_coGpi/coGpiDistBearTable_72allsp.qs'))

cogDistTable <- cogDistTable %>% mutate(coursefrom2011 = (bearingfrom2011 + 360) %% 360)
cogDistTable <- cogDistTable %>% mutate(direction = sapply(coursefrom2011, courseToRose))

traits <- read.csv('./inputs/birdSpecies_traits.csv')
traits2 <- traits %>% dplyr::select(Group,species,Family)
colnames(traits2) <- c('Group', 'species', 'Family')
cogDistTable <- left_join(cogDistTable, traits2,by= 'species' )

# ## add the direction in characters
# rose_breaks <- c(0, 360/16, (1/32 + (1:15 / 16)) * 360, 360)
# rose_breaks <- c(0.00,22.50,33.75,56.25, 78.75, 101.25, 123.75, 146.25, 168.75,
#                  191.25, 213.75, 236.25, 258.75,281.25, 303.75, 326.25, 348.75)
# breaks <- seq(0, 360, by= 45)   
# rose_labs <- c(
#   "North", "North-Northeast", "Northeast", "East-Northeast",
#   "East", "East-Southeast", "Southeast", "South-Southeast",
#   "South", "South-Southwest", "Southwest", "West-Southwest",
#   "West", "West-Northwest", "Northwest", "North-Northwest",
#   "North"
# )
rose_labs2 <- c(
  "North",  "Northeast",
  "East",  "Southeast",
  "South",  "Southwest",
  "West",  "Northwest",
  'North')
# 
# cogDistTable <- cogDistTable %>%  mutate(
#   direction2 = cut(
#     coursefrom2011,
#     breaks = breaks,
#     labels = rose_labs2,
#     right = FALSE,
#     include.lowest = TRUE
#   )
# )
cogDistTable31 <- cogDistTable %>% filter(year == '2031')
cogDistTable91 <- cogDistTable %>% filter(year == '2091')
#get direction from bearing for the near and long terms 
cogDistTable31<- cogDistTable31 %>% 
  select(year, species, Group,model, distancefrom2011, coursefrom2011) %>% 
  mutate(direction = sapply(coursefrom2011, courseToRose))

cogDistTable91<- cogDistTable91 %>% 
  select(year, species, Group, model, distancefrom2011, coursefrom2011) %>% 
  mutate(direction = sapply(coursefrom2011, courseToRose))


#get the freq of direction by year 
freqDir <- cogDistTable %>% group_by(model, year) %>% 
  summarize(total = n(), as_tibble(table(direction)))
freqDir31  <- freqDir %>% filter(year == '2031') %>% group_by(model,direction) %>% 
 summarise(freq = n()) 

freqDir91  <- freqDir %>% filter(year == '2091')



summ_coGDist <- cogDistTable %>% group_by(year, model) %>% 
  summarise(mean_dist = mean(distance),
            mean_dist11 = mean(distancefrom2011),
            mean_course = mean(coursefrom2011)) %>% 
  mutate(direction = sapply(mean_course, courseToRose))

summ_cogDist31 <-summ_coGDist %>% filter(year == '2031')
summ_cogDist91 <-summ_coGDist %>% filter(year == '2091')

## get the frequency of each direction per year and gcm as a data frame 
distance.freq31 <- as.data.frame(table(cogDistTable31$direction, cogDistTable31$model))
names(distance.freq31) <- c('direction', 'model', 'freq')
distance.freq91<- as.data.frame(table(cogDistTable91$direction2, cogDistTable91$model))
names(distance.freq91) <- c('direction', 'model', 'freq')  

breaks <- seq(0, 360, by= 22.5)   



cogDistTable <- cogDistTable %>% mutate(bear.cut = cut(coursefrom2011, breaks, 
                                                       right = FALSE,
                                                       labels = seq(1,360, by = 45)))


                                     
a <- cogDistTable %>% dplyr::select(year, model, bear.cut,species,coursefrom2011, 
                                    distancefrom2011, bearingfrom2011T)
a31 <- a %>% filter(year == '2031')

aFreq31 <- course.freq31 %>% group_by(model,course) %>% 
  summarise(freq = sum(freq))
aFreq31 <- aFreq %>% filter(year == '2031')

disFreq31 <- distance.freq31 %>% group_by(model,distance) %>% 
  summarise(freq= sum(freq))

a91 <- a %>% filter(year == '2091')

# generate a histogram with values binned every 45º
breaks = seq(0, 360, by= 45)   
labels = c("N","NE","E", "SE", "S","SW","W","NW", 'N')
breaks = seq(0, 360, by= 45)   

#2031
a31$course.cut <- cut(a31$coursefrom2011, rose_breaks, labels = rose_labs, 
                       right=FALSE) 
course.freq31 = as.data.frame(table(a31$course.cut, a31$model))

distance.freq31 = as.data.frame(table(a31$distancefrom2011, a31$model))
names(distance.freq31) <- c('distance', 'model', 'freq')
distance.freq31 <- distance.freq31 %>% group_by(model)
names(course.freq31) <- c('course', 'model', 'freq')
distance.freq31$distance <- as.numeric(distance.freq31$distance)
course.freq31 <- course.freq31 %>% group_by(model)



coursePlot31 <- ggplot(course.freq31, aes(x = course.cut, y = as.numeric(distance.freq31$distance), fill = model))+
  coord_polar(start = 6.28, direction = 1) + #start 0 for north, direction 1 for cloclwise
  geom_bar(stat = 'identity') +
 # geom_bar(data = distance.freq31, stat = 'identity', aes(x = as.numeric(distance), y = freq, fill = model)) +
  
  scale_fill_manual(values = c('CanESM2' = "#FF6A00",'CCSM4' = "#C15CCB", 
                               'INM-CM4' = "#00868B")) +
  scale_x_continuous(breaks= seq(0,360, 45),
                     labels = labels) +
  scale_y_continuous(limits = c(0,500)) +
  #xlab("Bearing") +
  #ylab ('Distance')
  ggtitle(label = glue('2031')) +
  theme_bw() + 
  theme(plot.title = element_text(size = 16,face = 'bold',hjust = 0, vjust = 0.7),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        #text = element_text(size = 10, color = 'gray'),
        strip.text = element_text(size = 12),
        legend.position = "none") +
  facet_wrap(~model)


bearDistPlot31 <- ggplot(data=a31,
       aes(bearingfrom2011T, distancefrom2011, fill = model)) +
  geom_segment(aes(xend = bearingfrom2011T, yend = 0.1)) +
  geom_point() +
  scale_x_continuous(breaks= seq(0,360, 45),
                     labels = labels) +
  scale_fill_manual(values = c('CanESM2' = "#FF6A00",'CCSM4' = "#C15CCB", 
                               'INM-CM4' = "#00868B")) +
  coord_polar(start = -pi, direction = 1) +
  xlab("Bearing") +
  ylab('Distance') +
  ggtitle(label = glue('2031')) +
  theme_bw() + 
  theme(plot.title = element_text(size = 16,face = 'bold',hjust = 0, vjust = 0.7),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        #text = element_text(size = 10, color = 'gray'),
        strip.text = element_text(size = 12),
        legend.position = "none") +
  facet_wrap(~model)
out <- ('./graphs/figs/polar')
ifelse(!dir.exists(out), dir.create(out), 'Folder already exists')
ggsave(plot = bearDistPlot31,
       filename = glue('{out}/bearingDistancePlot_2031.png'), 
       units = 'in', width = 12, height = 9, dpi = 700)
##THIS IS THE GRAPH OF THE MANUSCRIPT 

#using course
courseDistPlot31 <- ggplot(data=cogDistTable31,
       aes(coursefrom2011, distancefrom2011, color = Group)) +
  geom_segment(aes(xend = coursefrom2011, yend = 0.1)) +
  geom_point() +
 # scale_y_continuous(limits = 0, max(a31$distancefrom2011)) +
  scale_x_continuous(limits = c(0,360), 
                     expand = c(0,0),
                     breaks= seq(0,360, 45),
                     labels = c("N","NE","E", "SE", "S","SW","W","NW","N")) +

  #scale_y_continuous(limits = c(min(a31$distancefrom2011), max(a31$distancefrom2011)))
  # scale_fill_manual(values = c('CanESM2' = "#FF6A00",'CCSM4' = "#C15CCB", 
  #                              'INM-CM4' = "#00868B")) +
  coord_polar(start = 6.28, direction = 1) +
  xlab("Direction") +
  ylab('Distance (km)') +
  ggtitle(label = glue(' a) 2011-2031')) +
  theme_bw() + 
  theme(plot.title = element_text(size = 16,face = 'bold',hjust = 0, vjust = 0.7),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.position = "bottom") +
  facet_wrap(~model)


out <- ('./graphs/figs/polar')
ifelse(!dir.exists(out), dir.create(out), 'Folder already exists')
ggsave(plot = courseDistPlot31,
       filename = glue('{out}/courseDistancePlot_2031_color.png'), 
       units = 'in', width = 14, height = 7, dpi = 700)


  
  #2091
a91$course.cut <- cut(a91$coursefrom2011, rose_breaks,labels = rose_labs, right=FALSE) 
course.freq91 = as.data.frame(table(a91$course.cut, a91$model))
distance.freq91 = as.data.frame(table(a91$distancefrom2011, a91$model))
names(distance.freq91) <- c('distance', 'model', 'freq')
names(course.freq91) <- c('course', 'model', 'freq')

bearPlot91 <- ggplot(bearing.freq91, aes(x = bearing.cut, y = Freq, fill = Var2))+
  coord_polar(start = 0, direction = 1) + #start 0 for north, direction 1 for cloclwise
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = c('CanESM2' = "#FF6A00",'CCSM4' = "#C15CCB", 
                               'INM-CM4' = "#00868B")) +
  scale_x_continuous(breaks= seq(0,360, 45)) +
  xlab("Bearing") +
  ggtitle(label = glue('2091')) +
  theme_bw() + 
  theme(plot.title = element_text(size = 16,face = 'bold',hjust = 0, vjust = 0.7),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        #text = element_text(size = 10, color = 'gray'),
        strip.text = element_text(size = 12),
        legend.position = "none") +
  facet_wrap(~Var2)

out <- ('./graphs/figs/polar')
ifelse(!dir.exists(out), dir.create(out), 'Folder already exists')
ggsave(plot = bearPlot91,
       filename = glue('{out}/bearingFreq_2091.png'), 
       units = 'in', width = 12, height = 9, dpi = 700)

bearDistPlot91 <- ggplot(data=a91,
                         aes(bearingfrom2011T, distancefrom2011, fill = model)) +
  geom_segment(aes(xend = bearingfrom2011T, yend = 0.1)) +
  geom_point() +
  scale_x_continuous(breaks= seq(0,360, 45),
                     labels = labels) +
  scale_fill_manual(values = c('CanESM2' = "#FF6A00",'CCSM4' = "#C15CCB", 
                               'INM-CM4' = "#00868B")) +
  coord_polar(start = 0, direction = 1) +
  xlab("Bearing") +
  ylab('Distance (km)') +
  ggtitle(label = glue('2091')) +
  theme_bw() + 
  theme(plot.title = element_text(size = 16,face = 'bold',hjust = 0, vjust = 0.7),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        #text = element_text(size = 10, color = 'gray'),
        strip.text = element_text(size = 12),
        legend.position = "none") +
  facet_wrap(~model)
out <- ('./graphs/figs/polar')
ifelse(!dir.exists(out), dir.create(out), 'Folder already exists')
ggsave(plot = bearDistPlot91,
       filename = glue('{out}/bearingDistancePlot_2091.png'), 
       units = 'in', width = 12, height = 9, dpi = 700)


#using course

courseDistPlot91 <- ggplot(data=cogDistTable91,
                           aes(coursefrom2011, distancefrom2011, color = Group)) +
  geom_segment(aes(xend = coursefrom2011, yend = 0.1)) +
  geom_point() +
  scale_x_continuous(limits = c(0,360), 
                     expand = c(0,0),
                     breaks= seq(0,360, 45),
                     labels = c("N","NE","E", "SE", "S","SW","W","NW","N")) +
  scale_fill_manual(values = c('CanESM2' = "#FF6A00",'CCSM4' = "#C15CCB", 
                               'INM-CM4' = "#00868B")) +
  coord_polar(start = 6.28, direction = 1) + #start is 360 grados in radians = 6.28
  xlab("Direction") +
  ylab('Distance (km)') +
  ggtitle(label = glue(' b) 2011-2091')) +
  theme_bw() + 
  theme(plot.title = element_text(size = 16,face = 'bold',hjust = 0, vjust = 0.7),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.position = "bottom") +
  facet_wrap(~model)


out <- ('./graphs/figs/polar')
ifelse(!dir.exists(out), dir.create(out), 'Folder already exists')
ggsave(plot = courseDistPlot91,
       filename = glue('{out}/courseDistancePlot_2091_color.png'), 
       units = 'in', width = 14, height = 7, dpi = 700)


gall <-ggsave(glue('{out}/courseDistPlotbothYears.png'), 
              ggpubr::ggarrange(courseDistPlot31,courseDistPlot91,
                                heights = c(1,1), 
                                widths = c(1,1),
                                align = 'hv', ncol = 1,
                                #labels = c('a)', "b)"),
                                vjust = 3,
                                legend = 'bottom',
                                common.legend = T), 
              units = 'in', height = 10, width = 14, dpi = 700)
  
  
 
