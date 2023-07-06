# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(dplyr, glue, raster,sf, tidyverse)

g <- gc(reset = TRUE)
rm(list = ls())

# Load data ---------------------------------------------------------------
root <- './outputs'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)
spcs <- spcs[1:72]
path <- glue('{dirs}/occurpi')
path <- as.character(path)
path <- path[1:72]

targetCRS <-  paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                    "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")


# source SDMtools functions  ----------------------------------------------
source('./R/COGravity.R')
source('./R/getXYcoords.R')
source('./R/asc.from.raster.R')
source('./R/wt.mean.R')

# Function to use ---------------------------------------------------------
get_COG<- function(spc){
  #spc <- spcs[5] # Run and erase
  cat('Start ', spc, '\n')
  dir <- grep(spc,path, value = TRUE)
  fls <- list.files(dir, pattern = 'pi', full.names = TRUE)
  yrs <- parse_number(basename(fls))
  yrs <- unique(yrs)
  gcm <- str_sub(basename(fls), start = 16, end = nchar(basename(fls)) - 7)
  gcm <- unique(gcm)

  
  cat('To apply to each gcm\n')
  cog <- map(.x = 1:length(gcm), .f = function(k){
    message(crayon::green('Loading files for', gcm[k]))
    fl <- grep(gcm[k], fls, value = TRUE)
    fl <- as.character(fl)
    getYrs <-  map(.x = 1:length(yrs), .f = function(yr){
      message(crayon::green('Year', yrs[yr]))
      sfl <- grep(yrs[yr], fl, value = TRUE)
      rst <- raster(sfl) 
      rst <- raster::trim(rst)
      #convert to asc object 
      ras_asc<- asc.from.raster(rst)
      coG<- COGravity(ras_asc)
      vals <- bind_rows(coG, .id = 'year')
      vals <- vals %>% mutate(species = spc,
                              model = gcm[k],
                              year = yrs[yr])
     
      return(vals)
    })
    return(getYrs)
  })

 df<-  bind_rows(cog)
 out <- glue('./tables/coGpi')
 qs::qsave(x =df, file = glue('{out}/{spc}_coGpi.qs'))
return(df)
}
# Apply the function ------------------------------------------------------
cogTable<- map(.x = spcs, .f = get_COG)


# Read all COG tables  ----------------------------------------------------
read_table <- function(specie){
 # specie <- spcs[2]
  message(crayon::blue('Starting\n', specie, '\n'))
  path <- glue('./tables/coG')
  table <- qs::qread(file = glue('{path}/{specie}_coG.qs'))
  cat('Done \n')
  return(table)
}  
# Apply the function ------------------------------------------------------
coG_Table <- map(.x = spcs, .f = read_table)

coG_all_Table <- bind_rows(coG_Table)  

ifelse(!dir.exists(out), dir.create(out, recursive = TRUE), print('Folder already exist'))
qs::qsave(cogDistTable, glue('{out}/coGDistTable_all72sp.qs'))
write.csv(cogDistTable, './coG_all72Spp.csv')


my_sf_df <- sf::st_as_sf(table, coords = c("COGx", "COGy"), crs = targetCRS) 
