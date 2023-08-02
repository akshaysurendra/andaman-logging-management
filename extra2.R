### extra 2
# previous regen plot where instead of comparing AR v/s NR of E & D stems in E and D patches we compared AR v/s NR total stems and species in E an D patches

fig4_inset <-
  plot_grid(
    ggplot(data = dat4 %>% filter(response=="instem"),
           aes(y=count,x=regen_type)) +
      geom_violin(aes(fill=plot_type)) +
      geom_jitter(width = 0.03) +
      facet_grid(.~plot_type) + theme_bw() +
      stat_compare_means(comparisons = comparisons_fig4,method = "t.test",
                         paired = F,label = "p.signif") +
      scale_fill_manual(name ="",values = c("grey90","grey60")) +
      guides(fill=F) + xlab("") +
      scale_x_discrete(labels = c("Planted saplings (AR)","Natural regeneration (NR)")) +
      ylab("Number of saplings \n(<10cm gbh & taller than 30cm)") +
      coord_flip(),
    ggplot(data = dat4 %>% filter(response=="insp"),
           aes(y=count,x=regen_type)) +
      geom_violin(aes(fill=plot_type)) +
      geom_jitter(width = 0.03) +
      facet_grid(.~plot_type) + theme_bw() +
      stat_compare_means(comparisons = comparisons_fig4,method = "t.test",
                         paired = F,label = "p.signif") +
      scale_fill_manual(name ="",values = c("grey90","grey60")) +
      guides(fill=F) + xlab("") +
      scale_x_discrete(labels = c("Planted saplings (AR)","Natural regeneration (NR)")) +
      ylab("Species richness of saplings\n(<10cm gbh & taller than 30cm)") +
      coord_flip(),
    nrow = 2)

# ggsave(plot = fig4,filename = "figure4.png",device = "png",width = 20,height = 14,units = "cm",dpi = 300)
