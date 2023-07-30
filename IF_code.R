#### July 2023 ####

### Prep
library(tidyverse)
library(readxl)
library(ggpubr)
library(ggrepel)
library(cowplot)

### Figure 1: see PPT file
### Figure 2: Large tree density& richness ~ logging-treatment, forest-type ####

d.lar <- read_csv(file = "plotdata_large.csv")

dat2.tmp <- data.frame()
for(i in 1:n_distinct(d.lar$plot_ID))
{
  iplot <- d.lar %>% filter(plot_ID==sort(unique(d.lar$plot_ID))[i]) %>% filter(code!="stump")
  idat2 <-
    c(instem = nrow(iplot),
      # ieH = diversity(x = iplot %>% pull(code) %>% table(),index = "shannon") %>% exp(),
      insp = n_distinct(iplot$code))

  dat2.tmp <- bind_rows(dat2.tmp,idat2)
}

dat2 <-
  dat2.tmp %>% mutate(plot_ID = sort(unique(d.lar$plot_ID))) %>%
  left_join(y = d.lar %>% select(plot_ID,treatment,forest_type) %>% unique())

comparisons_fig2 <- list( c("TL", "OL_far"), #c("TL", "OL_near"),
                          c("OL_far", "OL_near"),
                          c("baseline", "OL_near"),
                          c("baseline", "OL_far"),c("baseline", "TL"))

commongg_fig2 <- list(
  stat_compare_means(comparisons = comparisons_fig2,method = "t.test",
                          paired = F,label = "p.signif", size = 3.5),
  scale_fill_manual(name ="",values = c("grey90","grey60")),
  scale_x_discrete(labels=c("unlogged"," old policy (1990s)","new policy (2007-14)",
                            "old & new policy (2007-14 & 1990s)")),
  theme_bw(),guides(fill=F),xlab(""),
  theme(text = element_text(size=12)),coord_flip())

fig2 <-
  plot_grid(
  ggplot(data = dat2,
         aes(y=instem,x=treatment)) +
    geom_violin(aes(fill=forest_type)) +
    geom_jitter(width = 0.03,size=0.8) + facet_wrap(.~forest_type,nrow = 2) +
    ylab("Tree density (trees/0.49ha)") + commongg_fig2,

  ggplot(data = dat2,
         aes(y=insp,x=treatment)) +
    geom_violin(aes(fill=forest_type)) +
    geom_jitter(width = 0.03,size=0.8) + facet_wrap(.~forest_type,nrow = 2) +
    ylab("Species richness (species/0.49ha)") + commongg_fig2 +
    theme(axis.text.y = element_blank()),
  rel_widths = c(1.85,1),
  ncol = 2)

# ggsave(plot = fig2,filename = "figure2.png",device = "png",width = 20,height = 13,units = "cm",dpi = 300)

### Figure 3: Rank-abundance curves #####

d.lar <- read_csv(file = "plotdata_large.csv")

# d.lar %>% group_by(treatment,forest_type) %>% summarise(nplots = n_distinct(plot_ID))
# scaled each rank-abundance # of plots to make relative abundances comparable

d.rac_TL.e <- d.lar %>% filter(treatment=="TL",forest_type=="evergreen",code!="stump") %>%
  group_by(treatment,forest_type,species_ID) %>% summarise(nstem = n()) %>%
  arrange(-nstem) %>% mutate(cstem = cumsum(nstem),scaled_nstem = nstem/1.5,
                             counter = row_number(), cstemperc = cstem/max(cstem))

d.rac_OLnear.e <- d.lar %>% filter(treatment=="OL_near",forest_type=="evergreen",code!="stump") %>%
  group_by(treatment,forest_type,species_ID) %>% summarise(nstem = n()) %>%
  arrange(-nstem) %>% mutate(cstem = cumsum(nstem),scaled_nstem = nstem/2.17,
                             counter = row_number(), cstemperc = cstem/max(cstem))

d.rac_OLfar.e <- d.lar %>% filter(treatment=="OL_far",forest_type=="evergreen",code!="stump") %>%
  group_by(treatment,forest_type,species_ID) %>% summarise(nstem = n()) %>%
  arrange(-nstem) %>% mutate(cstem = cumsum(nstem),scaled_nstem = nstem/1,
                             counter = row_number(), cstemperc = cstem/max(cstem))

d.rac_baseline.e <- d.lar %>% filter(treatment=="baseline",forest_type=="evergreen",code!="stump") %>%
  group_by(treatment,forest_type,species_ID) %>% summarise(nstem = n()) %>%
  arrange(-nstem) %>% mutate(cstem = cumsum(nstem),scaled_nstem = nstem/1,
                             counter = row_number(), cstemperc = cstem/max(cstem))

d.rac_TL.d <- d.lar %>% filter(treatment=="TL",forest_type=="deciduous",code!="stump") %>%
  group_by(treatment,forest_type,species_ID) %>% summarise(nstem = n()) %>%
  arrange(-nstem) %>% mutate(cstem = cumsum(nstem), scaled_nstem = nstem/2,
                             counter = row_number(), cstemperc = cstem/max(cstem))

d.rac_OLnear.d <- d.lar %>% filter(treatment=="OL_near",forest_type=="deciduous",code!="stump") %>%
  group_by(treatment,forest_type,species_ID) %>% summarise(nstem = n()) %>%
  arrange(-nstem) %>% mutate(cstem = cumsum(nstem),scaled_nstem = nstem/3,
                             counter = row_number(), cstemperc = cstem/max(cstem))

d.rac_OLfar.d <- d.lar %>% filter(treatment=="OL_far",forest_type=="deciduous",code!="stump") %>%
  group_by(treatment,forest_type,species_ID) %>% summarise(nstem = n()) %>%
  arrange(-nstem) %>% mutate(cstem = cumsum(nstem),scaled_nstem = nstem/1,
                             counter = row_number(), cstemperc = cstem/max(cstem))

d.rac_baseline.d <- d.lar %>% filter(treatment=="baseline",forest_type=="deciduous",code!="stump") %>%
  group_by(treatment,forest_type,species_ID) %>% summarise(nstem = n()) %>%
  arrange(-nstem) %>% mutate(cstem = cumsum(nstem),scaled_nstem = nstem/1,
                             counter = row_number(), cstemperc = cstem/max(cstem))

treatment.labs <- c("unlogged","old policy (1990s)","new policy (2007-14)", "both (2007-14 & 1990s)")
names(treatment.labs) <- c("baseline", "OL_far", "OL_near","TL")

commongg_fig3 <- list(geom_line(alpha = 0.4),
                 geom_point(alpha = 0.8), ylim(0,35),
                 facet_wrap(forest_type ~ treatment,nrow = 2,scales = "free_x",
                            labeller = labeller(treatment = treatment.labs)),
                 ylab(""),xlab(""),theme_bw(),
                 geom_text_repel(aes(label = ifelse(cstemperc <= 0.5,
                                                    species_ID,""),fontface = 'italic'),
                                 size = 2.5,box.padding = 0.35,
                                 min.segment.length = 0,nudge_y = 5,
                                 segment.colour = "gray80",max.overlaps = Inf,
                                 xlim = c(NA,NA),ylim = c(NA,NA)),
                 theme(axis.text.x = element_blank(),
                       text = element_text(size=11),
                       panel.spacing = unit(0.1,"lines")),
                 coord_cartesian(clip = "off"))

specialgg_fig3_evergreenboth <- list(geom_line(alpha = 0.4),
                      geom_point(alpha = 0.8), ylim(0,35),
                      facet_wrap(forest_type ~ treatment,nrow = 2,scales = "free_x",
                                 labeller = labeller(treatment = treatment.labs)),
                      ylab(""),xlab(""),theme_bw(),
                      geom_text_repel(aes(label = ifelse(cstemperc <= 0.5,
                                                         species_ID,""),fontface = 'italic'),
                                      size = 2.5,box.padding = 0.35,
                                      min.segment.length = 0,nudge_y = 0,
                                      segment.colour = "gray80",max.overlaps = Inf,
                                      xlim = c(NA,NA),ylim = c(NA,NA)),
                      theme(axis.text.x = element_blank(),
                            text = element_text(size=11),
                            panel.spacing = unit(0.1,"lines")),
                      coord_cartesian(clip = "off"))



fig3 <-
  plot_grid(
    ggplot(data = d.rac_TL.d, aes(y = scaled_nstem,x=counter)) + commongg_fig3,
    ggplot(data = d.rac_OLnear.d, aes(y = scaled_nstem,x=counter)) + commongg_fig3 + theme(axis.text.y = element_blank()),
    ggplot(data = d.rac_OLfar.d, aes(y = scaled_nstem,x=counter)) + commongg_fig3 + theme(axis.text.y = element_blank()),
    ggplot(data = d.rac_baseline.d, aes(y = scaled_nstem,x=counter)) + commongg_fig3 + theme(axis.text.y = element_blank()),
    ggplot(data = d.rac_TL.e, aes(y = scaled_nstem,x=counter)) + specialgg_fig3_evergreenboth,
    ggplot(data = d.rac_OLnear.e, aes(y = scaled_nstem,x=counter)) + commongg_fig3 + theme(axis.text.y = element_blank()),
    ggplot(data = d.rac_OLfar.e, aes(y = scaled_nstem,x=counter)) + commongg_fig3 + theme(axis.text.y = element_blank()),
    ggplot(data = d.rac_baseline.e, aes(y = scaled_nstem,x=counter)) + commongg_fig3 + theme(axis.text.y = element_blank()),greedy = TRUE,

    nrow = 2)

# ggsave(plot = fig3,filename = "figure3.png",device = "png",width = 21,height = 12,units = "cm",dpi = 300)

### Figure 4: Regen ####
d.nra <- read_xlsx(path = "nra_data.xlsx",sheet = 1)

dat4.tmp <- data.frame()
for(i in 1:n_distinct(d.nra$plot_name))
{
  iplot <- d.nra %>% filter(plot_name == sort(unique(d.nra$plot_name))[i])
  idat3 <-
    c(instem_ar = sum(iplot$nra_sure) + sum(iplot$nra_mostly) + sum(iplot$unsure),
      instem_nr = sum(iplot$natural),
      insp_ar = iplot %>% filter(nra_sure>0 | unsure>0 | nra_mostly >0) %>% pull(species) %>% n_distinct(),
      insp_nr = iplot %>% filter(natural>0) %>% pull(species) %>% n_distinct())
  dat4.tmp <- bind_rows(dat4.tmp,idat3)
}

dat4 <-
  dat4.tmp %>%
  mutate(plot_name = sort(unique(d.nra$plot_name))) %>%
  right_join(x = .,y = d.nra %>%
               select(plot_name,plot_type) %>% distinct()) %>%
  pivot_longer(cols = c(-plot_name,-plot_type),values_to = "count") %>%
  separate(col = "name",into = c("response","regen_type"),sep = "_",remove = T)

comparisons_fig4 <- list( c("ar", "nr"))

fig4 <-
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

##### TRIAL - NMDS plots

library(vegan)

### large tree NMDS

nmds_lar1 <-
  d.lar %>%
  group_by(code,forest_type,treatment,plot_ID) %>% summarize(count = n()) %>%
  reshape2::dcast(formula = code ~ plot_ID,fun.aggregate = sum,value.var = "count") %>%
  column_to_rownames("code") %>%
  metaMDS(comm = .,distance = "bray",k = 2,autotransform = TRUE,plot = F)

nmds_lar2 <-
  bind_rows(
nmds_lar1$points[,c("MDS1","MDS2")] %>%
  as.data.frame() %>% rownames_to_column("ID_column") %>% mutate(dtype = "species"),
nmds_lar1$species[,c("MDS1","MDS2")] %>%
  as.data.frame() %>% rownames_to_column("ID_column") %>% mutate(dtype = "plots")) %>%
  left_join(x = .,y= d.lar %>% select(treatment,forest_type,plot_ID) %>% distinct(),
            by = c("ID_column"="plot_ID"))

plot_nmds_large_all <-
ggplot(data = nmds_lar2,
       aes(y=MDS2,x=MDS1,shape=dtype,colour=treatment)) +
  geom_point() + ggrepel::geom_text_repel(aes(label=ifelse(dtype=="species",ID_column,""))) +
  scale_colour_manual(name="",values=c("blue","forestgreen","yellow","grey30")) +
  theme_bw() + theme(legend.position = "top") +
  coord_equal()

dom_sp <-
  d.lar %>% group_by(treatment,forest_type,code) %>% summarize(nstem = n()) %>%
  group_split(.keep = "code") %>% # 8 groups, 4 logging treatments x 2 forest type
  map(.f = function(x) #  in each group, find top 50% species
    x %>% arrange(-nstem) %>% ungroup() %>%
      mutate(csum = cumsum(nstem),csum_perc = csum/sum(nstem)) %>%
      filter(csum_perc <=0.5) %>% pull(code)) %>%
  unlist() %>% unique()

plot_nmds_large_dom <-
  ggplot(data = nmds_lar2,
         aes(y=MDS2,x=MDS1,shape=dtype,colour=treatment)) +
  geom_point() + ggrepel::geom_text_repel(aes(label=ifelse(dtype=="species" & ID_column %in% dom_sp,
                                                           ID_column,""))) +
  scale_colour_manual(name="",values=c("blue","forestgreen","yellow","grey30")) +
  theme_bw() + theme(legend.position = "top") +
  coord_equal()

### NRA NMDS

nmds_nra0 <-
  d.nra %>%
  mutate(tot_ar = nra_sure + nra_mostly + unsure,
         tot_nr = natural) %>%
  select(plot_name,plot_type,species,tot_ar,tot_nr) %>%
  pivot_longer(cols = c(tot_ar,tot_nr),names_to = "regeneration_type",values_to = "counts")

nmds_nra1 <-
  nmds_nra0 %>%
  reshape2::dcast(formula = species ~ plot_name,fun.aggregate = sum,value.var = "counts") %>%
  column_to_rownames("species") %>%
  metaMDS(comm = .,distance = "bray",k = 2,autotransform = TRUE,plot = F,)

nmds_nra2 <-
  bind_rows(
  nmds_nra1$points[,c("MDS1","MDS2")] %>%
      as.data.frame() %>% rownames_to_column("ID_column") %>% mutate(dtype = "species"),
  nmds_nra1$species[,c("MDS1","MDS2")] %>%
  as.data.frame() %>% rownames_to_column("ID_column") %>% mutate(dtype = "plots")) %>%
  left_join(x = .,y= nmds_nra0 %>% select(plot_type,plot_name) %>% distinct(),
            by = c("ID_column"="plot_name"))

plot_nmds_nra_all <-
  ggplot(data = nmds_nra2,
         aes(y=MDS2,x=MDS1,shape=dtype,colour = plot_type)) +
  geom_point() + ggrepel::geom_text_repel(aes(label=ifelse(dtype=="species",ID_column,""))) +
  theme_bw() + theme(legend.position = "top") +
  coord_equal()

# AR

nmds_nra1 <-
  nmds_nra0 %>% filter(regeneration_type=="tot_ar",counts>0) %>%
  reshape2::dcast(formula = species ~ plot_name,
                  fun.aggregate = sum,value.var = "counts") %>%
  column_to_rownames("species") %>%
  metaMDS(comm = ., distance = "bray", k = 2,autotransform = TRUE, plot = F)

nmds_nra2 <-
  bind_rows(
    nmds_nra1$points[,c("MDS1","MDS2")] %>%
      as.data.frame() %>% rownames_to_column("ID_column") %>% mutate(dtype = "species"),
    nmds_nra1$species[,c("MDS1","MDS2")] %>%
      as.data.frame() %>% rownames_to_column("ID_column") %>% mutate(dtype = "plots")) %>%
  left_join(x = .,y= nmds_nra0 %>% select(plot_type,plot_name) %>% distinct(),
            by = c("ID_column"="plot_name"))

plot_nmds_nra_AR <-
  ggplot(data = nmds_nra2,
         aes(y=MDS2,x=MDS1,shape=dtype,colour = plot_type)) +
  geom_point() + ggrepel::geom_text_repel(aes(label=ifelse(dtype=="species",ID_column,""))) +
  theme_bw() + theme(legend.position = "top") +
  coord_equal()

# NR

nmds_nra1 <-
  nmds_nra0 %>% filter(regeneration_type=="tot_nr",counts>0) %>%
  reshape2::dcast(formula = species ~ plot_name,
                  fun.aggregate = sum,value.var = "counts") %>%
  column_to_rownames("species") %>%
  metaMDS(comm = ., distance = "bray", k = 2,autotransform = TRUE, plot = F)

nmds_nra2 <-
  bind_rows(
    nmds_nra1$points[,c("MDS1","MDS2")] %>%
      as.data.frame() %>% rownames_to_column("ID_column") %>% mutate(dtype = "species"),
    nmds_nra1$species[,c("MDS1","MDS2")] %>%
      as.data.frame() %>% rownames_to_column("ID_column") %>% mutate(dtype = "plots")) %>%
  left_join(x = .,y= nmds_nra0 %>% select(plot_type,plot_name) %>% distinct(),
            by = c("ID_column"="plot_name"))

plot_nmds_nra_NR <-
  ggplot(data = nmds_nra2,
         aes(y=MDS2,x=MDS1,shape=dtype,colour = plot_type)) +
  geom_point() + ggrepel::geom_text_repel(aes(label=ifelse(dtype=="species",ID_column,""))) +
  theme_bw() + theme(legend.position = "top") +
  coord_equal()

ggsave(filename = "nmds_largetrees_all.png",plot = plot_nmds_large_all,device = "png",scale = 1.3,dpi = 300)
ggsave(filename = "nmds_largetrees_dominants.png",plot = plot_nmds_large_dom,device = "png",scale = 1.3,dpi = 300)
ggsave(filename = "nmds_nra_together.png",plot = plot_nmds_nra_all,device = "png",scale = 1.3,dpi = 300)
ggsave(filename = "nmds_nra_sidebyside.png",plot = cowplot::plot_grid(plot_nmds_nra_AR,plot_nmds_nra_NR,nrow = 1),
       device = "png",scale = 3,dpi = 300)





