######################
## Perform simulations
######################

## Parameters
#------------
alpha = 2
kappa = 1/5
# mean(graph$table$dist) * kappa = 0.63 -> exp(-0.63) = 0.5 average correlation

exp(- mean(graph$table$dist) * kappa)

## Latent variable
#-----------------
# simulate random field
omega_s = simulate_sfnetwork( n=1, sfnetwork_mesh=graph, theta=kappa)[,1]


## Sample locations along network
#--------------------------------
extrap = st_union( st_line_sample( activate(stream_upstream,"edges"), density=1/5000 ))
extrap = st_cast( extrap, "POINT" )

## Link sampled location to mesh nodes
#-------------------------------------
# Project to sampled locations
A_is = sfnetwork_evaluator( stream = graph$stream,
                            loc = st_coordinates(extrap) )
omega_i = (A_is %*% omega_s)[,1]

## Simulate observations
#-----------------------
Count_i = rpois( n=length(omega_i), lambda=exp(alpha + omega_i) )

## Shape data
#------------
Data = data.frame( Count = Count_i,
                   st_coordinates(extrap),
                   var = "species",  # Univariate model so only one value
                   time = "2020",    # no time-dynamics, so only one value
                   dist = "obs" )    # only one type of sampling in data

# Plot data
plot(stream_upstream,cex=0.5)
plot( st_sf(extrap,"omega"=omega_i),
    add=TRUE, pch=19, cex=0.5,pal=viridis)

## Fit model
#-----------
out = tinyVAST( data = Data,
  family = poisson(),
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
  st_sf(sf_plot,"pred"=log(predict_df)),
  add=TRUE, pch=19, cex=0.5, pal=viridis 
)

# Add true (simulated) values
plot(
  stream_upstream,
  main="Plot data",cex=0.25
)

plot(
  st_sf(extrap,"omega"=omega_i),
  add=TRUE, pch=19, cex=0.5, pal=viridis 
)
