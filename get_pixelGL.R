
path <- './qs/occurpi'
fls <- list.files(path, pattern = '2011-2091', full.names = TRUE)
range_table <- function(spc){
  #spc <- spcs[1]
   message(crayon::blue('Starting\n',spc, '\n'))
   tbl <- qs::qread(file = glue('{path}/occ_yrs_{spc}_pi.qs'))
   yrs <- c('2011','2031','2091')
   gcm <- unique(tbl$gc)
   
   Npix <- tbl %>%  group_by(gc) %>% summarise(pixels = n()) 
   Npix <- unique(Npix$pixels)
   
   dfm <- map(.x = 1:length(gcm), .f = function(k){
     message(crayon::green(gcm[k]))
     
     tble  <- tbl %>%  filter(gc == gcm[k]) 
     tble <- tble %>% 
       mutate(#p_change1191 = ((tble$y2091 - tble$y2011)/tble$y2011),
              change1131 = (tble$y2031 - tble$y2011),
              changeF31 = case_when(change1131 > 0 ~ 'positive',
                                    change1131 < 0 ~ 'negative',
                                    change1131 == 0 ~ 'stable'))
              # changeF31 = case_when(p_change1131 > 0.0 ~ 'positive', 
              #                       p_change1131 < 0 ~ 'negative',
              #                       p_change1131 == 0 ~ 'stable'))
      cellSize = 250 * 250
     
     summ31  <- tble %>% group_by(changeF31) %>% 
      summarise(count = n()) %>% 
       mutate(change = (count / Npix) * 100, 
              area_ha = (count * cellSize) / 10000,
              specie = spc,
              gcm = gcm[k])
     
     return(summ31)
    })
    out <- glue('./tables/rangeGL')
    ifelse(!dir.exists(out), dir.create(out, recursive = TRUE), print('Folder already exists'))
    df<-  bind_rows(dfm)
    qs::qsave(x =df , file = glue('{out}/{spc}_range1131.qs'))
   return(dfm)
   df<-  bind_rows(dfm)
   return(df)
}
   
# Apply the function ------------------------------------------------------
range_Table <- map(.x = spcs[1:72], .f = range_table)

rangeDF31<-  bind_rows(range_Table)  
out <- glue('./tables/rangeGL')
ifelse(!dir.exists(out), dir.create(out, recursive = TRUE), print('Folder already exists'))
qs::qsave(x =rangeDF31, file = glue('{out}/all_range1131.qs'))
qs::qsave(x =rangeDF, file = glue('{out}/all_range1191.qs'))
write.csv(rangeDF91, glue('{out}/rangeGL1191.csv'))
range91 <- qs::qread(file = glue('./tables/rangeGL/all_range1191.qs'))
range31 <- qs::qread(file = glue('./tables/rangeGL/all_range1131.qs'))


range <- rangeDF31 %>% group_by(changeF31, gcm) %>% 
  summarise(area_avg = mean(area_ha),
            p_pix = mean(count))

area_Gain <- rangeDF %>% group_by(gcm) %>%
  filter(changeF31 == 'positive') 
min(range_Gain$p_change)
max(range_Gain$p_change)

gain <- range_Gain %>% filter(p_change > 50)
unique(gain$specie)

library(ggplot2)
# Basic barplot
p<-ggplot(data=rangeDF, aes(x=p_change, y=specie, fill = changeFac)) +
  geom_bar(stat="identity")
p

# Horizontal bar plot
p + coord_flip()
