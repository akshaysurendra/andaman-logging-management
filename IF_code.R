#### IF - happening this time - July 2023 ####

# fig1 - a) map of study sites from aerial view (as in FEM), b) regen plots in Bakultala
# fig2 - large tree: density & richness
# fig3 - Rank-abundance curves with names of species, one inset for each logging treatment
# fig4 - regeneration: density, richness



## cancelled:
# AGB plot
# no of stumps
#d.stm <- read_csv(file = "data/plotdata.csv") %>% filter(code=="stump")
# dat1 <- d.stm %>% group_by(forest_type,treatment,plot_ID)%>% summarise(nstump = n()) %>%
#     right_join(x = .,y = d.lar %>% select(treatment,forest_type,plot_ID) %>% distinct())
# comparisons_fig1 <- list( c("TL", "OL_far"), c("TL", "OL_near"), c("OL_far", "OL_near"))
# fig1 <- ggplot(data = dat1 %>% filter(treatment!="baseline"), aes(y=nstump,x=treatment)) +
#   geom_violin(aes(fill=forest_type)) + geom_jitter(width = 0.03) + facet_grid(.~forest_type) + theme_bw() +
#   stat_compare_means(comparisons = comparisons_fig1,method = "t.test",paired = F,label = "p.signif") +
#   scale_fill_manual(name ="",values = c("grey90","grey60")) + scale_x_discrete(labels=c("once (~25 yrs ago)",
#                             "once (~7 yrs ago)", "twice (~7 & ~25 yrs ago)")) + guides(fill=F) + xlab(NULL) +
#   ylab("Number of logging stumps (per 0.49ha)") +  coord_flip()
# ggsave(plot = fig1,filename = "figure2.png",device = "png",width = 20,height = 7,units = "cm",dpi = 300)


### Prep
setwd("C:/Users/aksha/Desktop/IF/")
library(tidyverse)
library(readxl)
library(vegan)
library(ggpubr)
library(ggrepel)
library(sf)
library(OpenImageR)

### Figure 1: Map etc. (PPT)

### Figure 2: Large tree density and diversity ~ logging-treatment, forest-type #####

d.lar <- read_csv(file = "data/plotdata.csv") %>%
  mutate(gbh_cm = ifelse(is.na(gbh_cm),0,gbh_cm),
         gbh_cm_stem2 = ifelse(is.na(gbh_cm_stem2),0,gbh_cm_stem2),
         gbh_cm_stem3 = ifelse(is.na(gbh_cm_stem3),0,gbh_cm_stem3)) %>%
  mutate(gbheff = sqrt(gbh_cm^2 + gbh_cm_stem2^2 + gbh_cm_stem3^2)) %>%
  filter(gbheff >=180)

dat2.tmp <- data.frame()
for(i in 1:n_distinct(d.lar$plot_ID))
{
  iplot <- d.lar %>% filter(plot_ID==sort(unique(d.lar$plot_ID))[i]) %>% filter(code!="stump")
  idat2 <-
    c(instem = nrow(iplot),
      #iagb = sum(iplot$agb_Mg),
      insp = n_distinct(iplot$code),
      ieH = diversity(x = iplot %>% pull(code) %>% table(),index = "shannon") %>% exp())
  dat2.tmp <- bind_rows(dat2.tmp,idat2)
}

dat2 <-
  dat2.tmp %>% mutate(plot_ID = sort(unique(d.lar$plot_ID))) %>%
  left_join(y = d.lar %>% select(plot_ID,treatment,forest_type) %>% unique())

comparisons_fig2 <- list( c("TL", "OL_far"), #c("TL", "OL_near"),
                          c("OL_far", "OL_near"),
                          c("baseline", "OL_near"),
                          c("baseline", "OL_far"),c("baseline", "TL"))

fig2 <-
  cowplot::plot_grid(
  ggplot(data = dat2,aes(y=instem,x=treatment)) +
  geom_violin(aes(fill=forest_type)) +
  geom_jitter(width = 0.03) +
  facet_grid(.~forest_type) + theme_bw() +
  stat_compare_means(comparisons = comparisons_fig2,method = "t.test",
                     paired = F,label = "p.signif") +
  scale_fill_manual(name ="",values = c("grey90","grey60")) +
  scale_x_discrete(labels=c("unlogged",
                            "once (~25 yrs ago)",
                            "once (~7 yrs ago)",
                            "twice (~7 & ~25 yrs ago)")) +
  guides(fill=F)+ xlab("") + ylab("Tree density (gbh>=180cm in 0.49ha)") + coord_flip(),

  ggplot(data = dat2,aes(y=insp,x=treatment)) +
  geom_violin(aes(fill=forest_type)) +
  geom_jitter(width = 0.03) +
  facet_grid(.~forest_type) + theme_bw() +
  stat_compare_means(comparisons = comparisons_fig2,method = "t.test",
                     paired = F,label = "p.signif") +
  scale_fill_manual(name ="",values = c("grey90","grey60")) +
    scale_x_discrete(labels=c("unlogged",
                              "once (~25 yrs ago)",
                              "once (~7 yrs ago)",
                              "twice (~7 & ~25 yrs ago)")) +
    guides(fill=F) + xlab("") + ylab("Species richness (gbh>=180cm in 0.49ha)") + coord_flip(),
  nrow = 3)

ggsave(plot = fig2,filename = "figure2.png",device = "png",width = 20,height =21,units = "cm",dpi = 300)

### Figure 3: Rank-abundance curves #####

d.lar <- read_csv(file = "data/plotdata.csv") %>%
  mutate(gbh_cm = ifelse(is.na(gbh_cm),0,gbh_cm),
         gbh_cm_stem2 = ifelse(is.na(gbh_cm_stem2),0,gbh_cm_stem2),
         gbh_cm_stem3 = ifelse(is.na(gbh_cm_stem3),0,gbh_cm_stem3)) %>%
  mutate(gbheff = sqrt(gbh_cm^2 + gbh_cm_stem2^2 + gbh_cm_stem3^2)) %>%
  filter(gbheff >=180)

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

treatment.labs <- c("unlogged","once (~25 yrs ago)","once (~7 yrs ago)","twice (~7 & ~25 yrs ago)")
names(treatment.labs) <- c("baseline", "OL_far", "OL_near","TL")

commongg <- list(geom_line(alpha = 0.4),
                 geom_point(alpha = 0.8), ylim(0,35),
                 facet_wrap(forest_type ~ treatment,nrow = 2,scales = "free_x",
                            labeller = labeller(treatment = treatment.labs)),
                 ylab(""),xlab(""),theme_bw(),theme(axis.text.x = element_blank()),
                 geom_text_repel(aes(label = ifelse(cstemperc <= 0.5,
                                                    species_ID,"")),
                                 size = 3.5,box.padding = 2,
                                 min.segment.length = 0,
                                 segment.colour = "gray80",max.overlaps = 20))

fig3 <-
  cowplot::plot_grid(
    ggplot(data = d.rac_TL.d, aes(y = scaled_nstem,x=counter)) + commongg,
    ggplot(data = d.rac_TL.e, aes(y = scaled_nstem,x=counter)) + commongg,
    ggplot(data = d.rac_OLnear.d, aes(y = scaled_nstem,x=counter)) + commongg,
    ggplot(data = d.rac_OLnear.e, aes(y = scaled_nstem,x=counter)) + commongg,
    ggplot(data = d.rac_OLfar.d, aes(y = scaled_nstem,x=counter)) + commongg,
    ggplot(data = d.rac_OLfar.e, aes(y = scaled_nstem,x=counter)) + commongg,
    ggplot(data = d.rac_baseline.d, aes(y = scaled_nstem,x=counter)) + commongg,
    ggplot(data = d.rac_baseline.e, aes(y = scaled_nstem,x=counter)) + commongg,
    nrow = 4)

ggsave(plot = fig3,filename = "figure3.png",device = "png",width = 20,height = 28,units = "cm",dpi = 300)

### Figure 4: Regen ####
d.nra <- read_xlsx(path = "data/nra_data.xlsx",sheet = 1)

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
  cowplot::plot_grid(
    ggplot(data = dat4 %>% filter(response=="instem"),
         aes(y=count,x=regen_type)) +
  geom_violin(aes(fill=plot_type)) +
  geom_jitter(width = 0.03) +
  facet_grid(.~plot_type) + theme_bw() +
  stat_compare_means(comparisons = comparisons_fig3,method = "t.test",
                     paired = F,label = "p.signif") +
  scale_fill_manual(name ="",values = c("grey90","grey60")) +
  guides(fill=F) + xlab("") +
  scale_x_discrete(labels = c("Planted saplings (AR)","Natural regeneration (NR)")) +
  ylab("Number of saplings \n(<10cm gbh & taller than 30cm)") +
  coord_flip(),
  ggplot(data = dat3 %>% filter(response=="insp"),
         aes(y=count,x=regen_type)) +
    geom_violin(aes(fill=plot_type)) +
    geom_jitter(width = 0.03) +
    facet_grid(.~plot_type) + theme_bw() +
    stat_compare_means(comparisons = comparisons_fig3,method = "t.test",
                       paired = F,label = "p.signif") +
    scale_fill_manual(name ="",values = c("grey90","grey60")) +
    guides(fill=F) + xlab("") +
    scale_x_discrete(labels = c("Planted saplings (AR)","Natural regeneration (NR)")) +
    ylab("Species richness of saplings\n(<10cm gbh & taller than 30cm)") +
    coord_flip(),
  nrow = 2)

ggsave(plot = fig4,filename = "figure4.png",device = "png",width = 20,height = 14,units = "cm",dpi = 300)



