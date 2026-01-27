###############################
## Filter data (species, years)
###############################
spp_sf <- catches_env_sf |> 
  mutate(log_effectif = log(effectif + 1)) |> 
  filter(esp_code_alternatif == spp_to_plot) |> 
  filter(annee %in% year_of_interest)

if(make_plots){
  par(mfrow = c(1,1))
  plot(stream,cex=0.05,col="darkgrey")
  plot(spp_sf[,"log_effectif"],
      add=TRUE, pch=19, cex=1)
}
