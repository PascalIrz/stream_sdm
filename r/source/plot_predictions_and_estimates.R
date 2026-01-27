#################################
## Plot predictions and estimates
#################################

## Compute predictions
#---------------------

# Define plotting points
sf_plot = st_union( st_line_sample( activate(stream_upstream,"edges"), density=1/1000))
sf_plot = st_cast( sf_plot, "POINT" )

# Format as `newdata` for prediction
newdata = data.frame(
  Count = NA,
  st_coordinates(sf_plot),
  var = "species",  # Univariate model so only one value
  time = 2018,    # no time-dynamics, so only one value
  dist = "obs"    # only one type of sampling in data
)

# Extract predicted spatial variable
predict_df = predict( out, newdata = newdata )

## Plot predictions
#------------------
if(make_plots){

  par(mfrow = c(1,1))
  plot(
    stream_upstream,
    main="Plot predictions",cex=0.25
  )
  
  plot(
    st_sf(sf_plot,"pred"=log(predict_df)),
    add=TRUE, pch=19, cex=0.05, pal=viridis 
  )
  
  plot( spp_sf[,"log_effectif"],
        add=TRUE, pch=19, cex=0.5)
  
}

## Plot temporal index
#---------------------
index_delta_tc <- which(names(out$sdrep$par.random) == "delta_tc")

df_time_index <- data.frame(year = year_of_interest, 
  delta_tc = out$sdrep$par.random[index_delta_tc],
  sd_delta_tc = out$sdrep$diag.cov.random[index_delta_tc])

delta_tc_plot <- ggplot(data = df_time_index,aes(x=year,y=delta_tc))+
  geom_line()+
  geom_ribbon(aes(ymin = delta_tc - 1.96 * sd_delta_tc,
    ymax = delta_tc + 1.96 * sd_delta_tc), fill = "grey70", alpha=0.25)+
  theme_minimal()

if(make_plots) plot(delta_tc_plot)

res_lm <- lm(df_time_index$year~df_time_index$delta_tc)
summary(res_lm)
plot(res_lm)
