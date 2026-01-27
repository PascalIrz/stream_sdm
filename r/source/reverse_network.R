#######################################################
## Reverse the stream network (downstream --> upstream)
#######################################################

# Extract edges and nodes
edges <- st_as_sf(activate(rht_network_filtered, "edges"))
nodes <- st_as_sf(activate(rht_network_filtered, "nodes"))

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
