# Set relative path
setwd("./flights")

# Read route data cityTocity and display titles
paths <- read.csv("citiesToCities.csv")
colnames(paths)

# Setup empty array of double the size of rows in paths, and set titles
new_paths <- array(0,dim=c(nrow(paths)*2,12))
colnames(new_paths) <- c('dep_city', 'dep_country', 'dep_lon', 'dep_lat', 'arr_city', 'arr_country', 'arr_lon', 'arr_lat', 'paths_num', 'path_id', 'path_order', 'distance')

# Fill the data (takes 30 sec, paste function converts to strings and carries out concatenation)
for (i in 1:nrow(paths)) { 
  new_paths[(i*2)-1,] <- c(paste(paths[i, 'departure.city']), paste(paths[i, 'departure.country']), paths[i, 'long..departure..decimal.'], paths[i, 'lat..departure..decimal.'], paste(paths[i, 'arrival.city']), paste(paths[i, 'arrival.country']), paths[i, 'long..departure..decimal..1'], paths[i, 'lat..departure..decimal..1'], paths[i, 'number.of.routes'], paste(paths[i, 'departure.city'], ',', paths[i, 'departure.country'], ' - ', paths[i, 'arrival.city'], ',', paths[i, 'arrival.country']), 1, paths[i, 'distance'])
  new_paths[(i*2),] <- c(paste(paths[i, 'arrival.city']), paste(paths[i, 'arrival.country']), paths[i, 'long..departure..decimal..1'], paths[i, 'lat..departure..decimal..1'], paste(paths[i, 'departure.city']), paste(paths[i, 'departure.country']), paths[i, 'long..departure..decimal.'], paths[i, 'lat..departure..decimal.'], paths[i, 'number.of.routes'], paste(paths[i, 'departure.city'], ',', paths[i, 'departure.country'], ' - ', paths[i, 'arrival.city'], ',', paths[i, 'arrival.country']), 2, paths[i, 'distance'])
}

#Write to csv
write.csv(new_paths, 'routes_from_R.csv')
