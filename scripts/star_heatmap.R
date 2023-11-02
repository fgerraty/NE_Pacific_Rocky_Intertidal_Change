
#Star Assemblage Plot ######### Very rough draft

#Import Raw Data
stars <- read_csv("data/clean/stars.csv") %>% 
  group_by(georegion, year) %>% 
  summarise(DERIMB = mean(DERIMB), 
            EVATRO = mean(EVATRO), 
            HENSPP = mean(HENSPP), 
            MEDAEQ = mean(MEDAEQ), 
            ORTKOE = mean(ORTKOE),
            PISOCH = mean(PISOCH), 
            PYCHEL = mean(PYCHEL), 
            SOLSPP = mean(SOLSPP), 
            ASTSPP = mean(ASTSPP), 
            LETNAN = mean(LETNAN), 
            PATMIN = mean(PATMIN), 
            PISBRE = mean(PISBRE), 
            PISGIG = mean(PISGIG), 
            .groups = "drop") %>% 
  #Filter blank community data
  filter(rowSums(select(., c(3:15)) != 0) > 0)


stars_meta <- stars[,1:2]

stars_spp <- stars[,-c(1:2)] %>% 
  #Convert absolute abundance to relative abundance
  decostand(., method = "total") 

stars_spp_distmat <- vegdist(stars_spp, method = "bray")

stars_spp_distmat <- 
  as.matrix(stars_spp_distmat, labels = T)


stars_spp_NMS <-
  metaMDS(stars_spp_distmat,
          distance = "bray",
          k = 3,
          maxit = 999, 
          trymax = 500,
          wascores = TRUE)

nmds_df <- as.data.frame(scores(stars_spp_NMS))

plot_df <- cbind(stars_meta, nmds_df)



q <- ggplot(plot_df, aes(x=NMDS1, y=NMDS2))+
  geom_point(aes(color = georegion)) +
  theme_classic() 
q


p <- plot_ly(x = plot_df$NMDS1, y = plot_df$NMDS2, z = plot_df$NMDS3, 
             color = as.factor(plot_df$georegion),
             text = plot_df$year,
             type="scatter3d", mode="markers")

p <- ggplotly(p)

p


saveWidget(p, file = "3d_nmds_plot.html")
