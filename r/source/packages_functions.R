#########################
## Packages and functions
#########################

library(tidyverse)
library(ggplot2)
library(Matrix)
library(sf)
library(sfnetworks)
library(tinyVAST)
library(sf)
library(units)
library(viridisLite)

## A modified version of sfnetwork_mesh_2
## The stopping message has been changed to a warning message
## and objects have been added to results to inspect the network
sfnetwork_mesh_2 <- function (stream){
    edges = st_as_sf(activate(stream, "edges"))
    nodes = st_as_sf(activate(stream, "nodes"))
    N = nrow(nodes)
    table = data.frame(from = edges$from, to = edges$to, dist = drop_units(st_length(edges)))
    number_of_parents = table(factor(table$to, levels = seq_len(N)))
    if (max(number_of_parents) > 1) {
      warning_message <- "Stream network has multiple parents for a node. Check if it is ordered upstream or if preprocessing operations add nodes with multiple parents."
    }else{
      warning_message <- "Stream OK"
    }
    print(warning_message)
    Dist_ss = sparseMatrix(i = table$to, j = table$from, x = table$dist, 
        dims = c(N, N))
    out = structure(list(n = N, table = table, stream = stream, 
        Dist_ss = Dist_ss,number_of_parents = number_of_parents,
        warning_message = warning_message), class = "sfnetwork_mesh")
    return(out)
}