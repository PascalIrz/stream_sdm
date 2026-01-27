############
## Load data
############

############################
## Shape stream for tinyVAST
############################

################
## Fit real data
################


# Create your initial network

stream <- as_sfnetwork(st_transform(st_as_sf(rht_test, "edges"), 32631), directed = TRUE)

## Try to simplify network 
#-------------------------
## So that reverting the stream (downstream --> upstream) works fine

#-----------------------------------------------------------------
# Step 1: Extract edges from the sfnetwork
edges <- st_as_sf(activate(stream, "edges"))

# Step 2: Create a data frame of unique nodes
unique_nodes <- unique(c(edges$from, edges$to))
nodes_df <- data.frame(node_id = seq_along(unique_nodes), node = unique_nodes)

# Step 3: Define geometry for the nodes
# Assuming you have coordinates for nodes (example: x and y coordinates)
# You need to replace the following with actual coordinates for your nodes
coordinates <- data.frame(
  node = unique_nodes,
  geometry = st_sfc(lapply(seq_along(unique_nodes), function(i) st_point(c(sample(1:100, 1), sample(1:100, 1)))))
)

# Combine nodes_df with geometry
nodes_sf <- st_as_sf(merge(nodes_df, coordinates, by = "node"))

# Step 4: Identify upstream nodes
upstream_nodes <- unique_nodes[!unique_nodes %in% edges$to]

# Step 5: Filter edges to remove those connected to upstream nodes
filtered_edges <- edges %>%
  rename(from_node = from, to_node = to) %>%
  filter(!from_node %in% upstream_nodes & !to_node %in% upstream_nodes)

# Step 6: Update edges with node_id
edges_with_ids <- filtered_edges %>%
  left_join(nodes_df, by = c("from_node" = "node")) %>%
  rename(from = node_id) %>%
  left_join(nodes_df, by = c("to_node" = "node")) %>%
  rename(to = node_id)

# # Step 7: Create the new sfnetwork with the updated edges and nodes
# final_stream <- sfnetwork(
#   nodes = nodes_sf %>% filter(node %in% unique(c(edges_with_ids$from, edges_with_ids$to))),  # Keep only remaining nodes
#   edges = edges_with_ids,
#   directed = TRUE
# )

stream_filtered <- as_sfnetwork(st_transform(st_as_sf(filtered_edges[,colnames(rht_loire)], "edges"), 32631), directed = TRUE)

# Now final_stream should be created without errors
plot(stream,cex=0.05)
plot(stream_filtered,cex=0.05,col="red",add=T)
plot( spp_sf[,"log_effectif"],
      add=TRUE, pch=19, cex=1)


#-----------------------------------------------------------------
graph = sfnetwork_mesh_2( stream_filtered )
stream_in <- stream_filtered

# Extract edges and nodes
edges <- st_as_sf(activate(stream_filtered, "edges"))
nodes <- st_as_sf(activate(stream_filtered, "nodes"))

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

graph = sfnetwork_mesh_2( stream_upstream )
stream_in <- stream_upstream

# Rescale
graph$table$dist = graph$table$dist / 1000  # Convert distance scale

## Shape data
#------------
catches_env_sf <- st_transform(catches_env_sf,crs = st_crs(stream))
Data = data.frame( Count = log(catches_env_sf$effectif+1),
  st_coordinates(catches_env_sf),
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
par(mfrow = c(1,1))
plot(
  stream_upstream,
  main="Plot predictions",cex=0.25
)

plot(
  st_sf(sf_plot,"pred"=(predict_df)),
  add=TRUE, pch=19, cex=0.05, pal=viridis 
)

plot( spp_sf[,"log_effectif"],
      add=TRUE, pch=19, cex=0.5)
