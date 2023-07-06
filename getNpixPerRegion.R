


path <- './maps/binnary/diffRas'
fls <- list.files(path, pattern = 'diffRas', full.names = TRUE)

# Function to use ---------------------------------------------------------
getNpixPerRegion <- function(spc){

  fls <- list.files(path, pattern = 'diffRas', full.names = TRUE)
  #spc <- spcs[2] # Run and erase
  gcm <- str_sub(basename(fls), start = 19, end = nchar(basename(fls)) - 4)
  gcm <- unique(gcm)
  yrs <- c('2031', '2091')
  fls <- grep(spc,fls, value = TRUE)
  
  cat('Start getting area for', spc, '\n')
  
  getVals<- map(.x = 1:length(gcm), .f = function(k){
    message(crayon::green('Loading files for', gcm[k]))
    fl <- grep(gcm[k], fls, value = TRUE)
    fl <- as.character(fl)
    valsYears <-  map(.x = 1:length(yrs), .f = function(yr){
      message(crayon::green('Year', yrs[yr]))
      sfl <- grep(yrs[yr], fl, value = TRUE)
      rst <- terra::rast(sfl)
      #extract the area of each cell that is contained within each polygon    
      tbl <- exactextractr::exact_extract(rst,ecrg, include_xy = TRUE)
      #add polygon names that the results will be grouped by
      names(tbl) <- c('Middle', 'North', 'South')
      #bind the list output into a df and calculate the proportion cover for each category
      vals <- bind_rows(tbl, .id = 'region') %>%
        drop_na()
      summ <- vals %>% group_by(region, value) %>% 
        summarise(n_pix = n()) %>% 
        mutate(total = sum(n_pix),
               area = (n_pix *6.25) /10000,
               rel_freq = (n_pix / total) * 100,
               species = spc,
               model = gcm[k], 
               year = yrs[yr])
      
      return(summ)
    })
    df <- bind_rows(valsYears)
    out <- glue('./qs/pixPerArea')
    ifelse(!dir.exists(out), dir.create(out), 'folder already exists')
    qsave(x = df, file = glue('{out}/{spc}_pixelPerRegion.qs'))
    cat('Done!\n')
    return(df)
  })
}
# Apply the function ------------------------------------------------------
area <- map(.x = spcs, .f = getNpixPerRegion)
areaPol <- bind_rows(area)


areaPol2<- rbind(areaPol, ALFL)

areaPol <- areaPol2 %>% mutate(range = case_when(value == -1 ~ 'loss',
                                                 value == 1 ~ 'gain',
                                                 value == 0 ~ 'stable'))

rangeChange <- areaPol %>% group_by(region, model,range) %>% 
   summarise(n = count(as.factor(value))) 
  

areaPol31 <- areaPol %>% filter(year == '2031')
areaPol91 <- areaPol %>% filter(year == '2091') %>% 
  arrange(area2) %>% 
  mutate(species= factor(species, levels = species))


p31 <- ggplot(areaPol31, aes(x = species, y = sort(area2), fill = region)) +
  geom_bar(stat = 'identity') + 
  scale_fill_manual(values = c('Middle' = "#FF6A00",'North' = "#C15CCB", 
                                'South' = "#00868B")) +
  labs(x = "", y = "Area (ha)", fill = "Region") + 
  geom_hline(yintercept= 0, linetype = 'dashed') +
  coord_flip() +
  theme_bw() + 
  theme(legend.position = 'right',
                     plot.title = element_text(size = 16,face = 'bold',hjust = 0, vjust = 0.7),
                     plot.subtitle = element_text(size = 12),
                     axis.title = element_text(size = 14),
                     axis.text.x = element_text(size = 10),
                     axis.text.y = element_text(size = 8),
                     legend.text = element_text(size = 11)) + 
  facet_wrap(~model)


out <- glue('./graphs/figs/rangeGainLoss') 
ifelse(!dir.exists(out), dir.create(out, recursive = TRUE), print('Folder already exist'))
ggsave(plot = p31,filename = glue('{out}/rangeGainLoss31sort.png'),
       units = 'in', width = 12, height = 9, dpi = 700 )
