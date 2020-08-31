library(phytools)

phy = read.newick("tree_pruned.new")
plot(phy)

species = read_csv("streptanthus.csv") %>% 
  mutate(height_range = max_height_cm - min_height_cm,
         elev_range = max_elevation_m - min_elevation_m)


plot(species$max_height_cm, species$min_height_cm)
plot(species$min_elevation_m, species$max_elevation_m)
plot(species$height_range, species$elev_range)
summary(lm(species$height_range~species$elev_range))

plot(species$max_height_cm, species$elev_range)
summary(lm(species$max_height_cm~species$elev_range))
plot(species$max_height_cm, species$elev_range)
summary(lm(species$max_height_cm~species$elev_range))

