############
## Load data
############

load(paste0(data_folder,"poissons_env_temp.RData"))
load(paste0(data_folder,"reseaux_hydrographiques.rda"))
load(paste0(data_folder,"bassins_versants.rda"))

if(bassin == "loire") rht_sf <- st_transform(rht_loire, 32631)
if(bassin == "vilaine") rht_sf <- st_transform(rht_vilaine, 32631)
stream = as_sfnetwork(rht_sf)
catches_env_df <- left_join(catches,env)
catches_env_sf <- inner_join(points_geo_loire,catches_env_df)
catches_env_sf <- st_transform(catches_env_sf,crs = st_crs(stream))


# dim(catches)
# dim(elementary_samples)
# dim(env)
# dim(points_geo)
# dim(points_geo_loire)
# dim(points_geo_vilaine)
# dim(surveys)
# dim(tc_temp_chroniques)

# plot(st_geometry(rh_loire))
# plot(st_geometry(rht_loire),add=T,col="red")
# plot(st_geometry(bv_loire),add=T)
# plot(st_geometry(rh_vilaine))
# plot(st_geometry(rht_vilaine),add=T,col="red")
# plot(st_geometry(bv_vilaine),add=T)
