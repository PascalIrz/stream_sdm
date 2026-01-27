
## Look at different sectors and choose the ones 
## where there are stations
vec_sectors <- rht_loire_2$CdSecteurHydro
par(mfrow = c(2,2))
for(sect_i in unique(vec_sectors)){
  rht_test <- rht_loire_2 |>
    filter(CdSecteurHydro == sect_i)
  stream = as_sfnetwork(rht_test)

  df <- left_join(catches,env) |> 
    full_join(points_geo_loire) |> 
    filter(CdSecteurHydro == sect_i)
  df <- st_transform(df,crs = st_crs(stream))
  
  plot(stream,cex=0.05,main = sect_i)
  plot( df,
      add=TRUE, pch=19, cex=1,pal=viridis)
  
}

sector_of_interest <- "K4"
rht_test <- rht_loire_2 |>
  filter(CdSecteurHydro == sector_of_interest)
stream = as_sfnetwork(rht_test)

stream_plot <- ggplot() +
  geom_sf(data = st_as_sf(stream, "edges"), color = "steelblue") +
  geom_sf(data = st_as_sf(stream, "nodes"), size = 1, color = "black") +
  theme_minimal()
plot(stream_plot)

# Create your initial network
stream <- as_sfnetwork(st_transform(st_as_sf(rht_test, "edges"), 32631), directed = TRUE)

# Extract edges and nodes
edges <- st_as_sf(activate(stream, "edges"))
nodes <- st_as_sf(activate(stream, "nodes"))

# Reverse the direction so that is upstream not downstream
# 1. Swap 'from' and 'to'
# 2. Reverse the geometry of each linestring
edges_reversed <- edges %>%
  mutate(
    # Swap from/to
    from_new = to,
    to_new = from,
    # Reverse the linestring geometry
    geometry = st_reverse(geometry)
  ) %>%
  select(-from, -to) %>%
  rename(from = from_new, to = to_new)

# Rebuild the sfnetwork from reversed edges
stream_upstream <- sfnetwork(nodes, edges_reversed, directed = TRUE)

graph = sfnetwork_mesh( stream_upstream )

# Rescale
graph$table$dist = graph$table$dist / 1000  # Convert distance scale

## Shape data
#------------
df <- inner_join(points_geo_loire,catches) |> 
        inner_join(env) |> 
        filter(CdSecteurHydro == sector_of_interest) |> 
        filter(esp_code_alternatif %in% "GOU")
df <- st_transform(df,crs = st_crs(stream))
Data = data.frame( Count = log(df$effectif),
  st_coordinates(df),
  var = "species",  # Univariate model so only one value
  time = "2020",    # no time-dynamics, so only one value
  dist = "obs" )    # only one type of sampling in data

# # Plot data
# par(mfrow = c(1,1))
# plot(stream_upstream,cex=0.05)
# plot( st_sf(df[,"effectif"]),
# add=TRUE, pch=19, cex=0.5,pal=viridis)

## Fit model
#-----------
out = tinyVAST( data = Data,
  family = gaussian(),
  formula = Count ~ 1,
  spatial_domain = graph,
  space_column = c("X","Y"),
  variable_column = "var",
  time_column = "time",
  distribution_column = "dist",
  space_term = "" )

## Make predictions
#------------------
# Define plotting points
sf_plot = st_union( st_line_sample( activate(stream_upstream,"edges"), density=1/1000))
sf_plot = st_cast( sf_plot, "POINT" )

# Format as `newdata` for prediction
newdata = data.frame(
  Count = NA,
  st_coordinates(sf_plot),
  var = "species",  # Univariate model so only one value
  time = "2020",    # no time-dynamics, so only one value
  dist = "obs"    # only one type of sampling in data
)

# Extract predicted spatial variable
predict_df = predict( out, newdata = newdata )

# Plot stream predictions
par(mfrow = c(1,2))
plot(
  stream_upstream,
  main="Plot predictions",cex=0.25
)

plot(
  st_sf(sf_plot,"pred"=(predict_df)),
  add=TRUE, pch=19, cex=0.05, pal=viridis 
)