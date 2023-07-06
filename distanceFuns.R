lat = map(cogDF2$geometry,2) %>% unlist()
lon = map(cogDF2$geometry,1) %>% unlist()
points <-
df<-  df %>% mutate(lat_fut = lead(lat),
                    lon_fut = lead(lon))

dist <- distHaversine(points)


## Create Bearing Function using Haversine formula 


#distance from point A to B 
#https://rosettacode.org/wiki/Haversine_formula#R
#takes lat and log in decimal degrees and output in km 
 greatCircleDistance <- function(lat1, lon1, lat2, lon2) {
   rad <- function(x) { x * pi/180 }
  a <- sin(0.5 * (rad(lat2) - rad(lat1)))
  b <- sin(0.5 * (rad(lon2) - rad(lon1)))
  12742 * asin(sqrt(a * a + cos(rad(lat1)) * cos(rad(lat2)) * b * b))
  }
 

bearing <- function(lat1, lon1, lat2, lon2){
 X <- cos(lat2) * sin(abs(lon2-lon1))
 Y <- (cos(lat1)*sin(lat2)) - (sin(lat1)*cos(lat2)*cos(abs(lon2 -lon1)))
 b_angle <- atan2(X,Y)
 radtodeg <- function(rad){((rad * 180)/(pi)) %%360}
 b_angle_deg <- radtodeg(b_angle)
}

haversineDistance <- function(p1, p2, r = 6378137 #)