######################
## Simplify the stream
######################

# Allows to reduce the network if the network is too big to perform geoprocessing operations
rht_network <- as_sfnetwork(st_transform(st_as_sf(rht_sf, "edges"), 32631), directed = TRUE)

# Extract edges from the sfnetwork
edges <- st_as_sf(activate(rht_network, "edges"))

# Create a data frame of unique nodes
unique_nodes <- unique(c(edges$from, edges$to))
nodes_df <- data.frame(node_id = seq_along(unique_nodes), node = unique_nodes)

# Define geometry for the nodes
# Assuming we have coordinates for nodes (example: x and y coordinates)
# We need to replace the following with actual coordinates for the nodes
coordinates <- data.frame(
  node = unique_nodes,
  geometry = st_sfc(lapply(seq_along(unique_nodes), function(i) st_point(c(sample(1:100, 1), sample(1:100, 1)))))
)

# Combine nodes_df with geometry
nodes_sf <- st_as_sf(merge(nodes_df, coordinates, by = "node"))

# Identify upstream nodes
upstream_nodes <- unique_nodes[!unique_nodes %in% edges$to]

# Filter edges to remove those connected to upstream nodes
filtered_edges <- edges %>%
  rename(from_node = from, to_node = to) %>%
  filter(!from_node %in% upstream_nodes & !to_node %in% upstream_nodes)

rht_network_filtered <- as_sfnetwork(st_transform(st_as_sf(filtered_edges[,colnames(rht_loire)], "edges"), 32631), directed = TRUE)

if(make_plots){
  par(mfrow = c(1,1))
  plot(rht_network_filtered,cex=0.05,col="darkgrey",main = "Simplified version of the stream")
  plot( spp_sf[,"log_effectif"],
        add=T, pch=19, cex=1)
}
