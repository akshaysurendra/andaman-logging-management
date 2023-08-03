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
    c(instem = nrow(iplot) * (1/0.49), # scaled to 1 ha
      # ieH = diversity(x = iplot %>% pull(code) %>% table(),index = "shannon") %>% exp(),
      insp = n_distinct(iplot$code) * (1/0.49)) # scaled to 1 ha

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
                          paired = F,label = "p.format", size = 3.5),
  scale_fill_manual(name ="",values = c("grey90","grey60")),
  scale_x_discrete(labels=c("unlogged"," old policy (1990s)","new policy (2007-14)",
                            "old & new policy (2007-14 & 1990s)")),
  theme_bw(),guides(fill=F),xlab(""),
  theme(text = element_text(size=12)),coord_flip())

fig2 <-
  plot_grid(
  ggplot(data = dat2,
         aes(y=instem,x=treatment)) +
    geom_boxplot(aes(fill=forest_type)) +
    geom_jitter(width = 0.03,size=0.8) + facet_wrap(.~forest_type,nrow = 2) +
    ylab("Tree density (trees/ha)") + commongg_fig2,

  ggplot(data = dat2,
         aes(y=insp,x=treatment)) +
    geom_violin(aes(fill=forest_type)) +
    geom_jitter(width = 0.03,size=0.8) + facet_wrap(.~forest_type,nrow = 2) +
    ylab("Species richness (species/ha)") + commongg_fig2 +
    theme(axis.text.y = element_blank()),
  rel_widths = c(1.85,1),
  ncol = 2)

ggsave(plot = fig2,filename = "figure2.png",device = "png",width = 24,height = 13,units = "cm",dpi = 300)

### Figure 3: Rank-abundance curves #####

d.lar <- read_csv(file = "plotdata_large.csv")

# d.lar %>% group_by(treatment,forest_type) %>% summarise(nplots = n_distinct(plot_ID))
# scaled each rank-abundance # of plots to make relative abundances comparable

d.rac_TL.e <- d.lar %>% filter(treatment=="TL",forest_type=="evergreen",code!="stump") %>%
  group_by(treatment,forest_type,species_ID) %>% summarise(nstem = n()) %>%
  arrange(-nstem) %>% mutate(cstem = cumsum(nstem),scaled_nstem = (nstem/1.5)*(1/0.49),
                             counter = row_number(), cstemperc = cstem/max(cstem))

d.rac_OLnear.e <- d.lar %>% filter(treatment=="OL_near",forest_type=="evergreen",code!="stump") %>%
  group_by(treatment,forest_type,species_ID) %>% summarise(nstem = n()) %>%
  arrange(-nstem) %>% mutate(cstem = cumsum(nstem),scaled_nstem = (nstem/2.17)*(1/0.49),
                             counter = row_number(), cstemperc = cstem/max(cstem))

d.rac_OLfar.e <- d.lar %>% filter(treatment=="OL_far",forest_type=="evergreen",code!="stump") %>%
  group_by(treatment,forest_type,species_ID) %>% summarise(nstem = n()) %>%
  arrange(-nstem) %>% mutate(cstem = cumsum(nstem),scaled_nstem = (nstem/1)*(1/0.49),
                             counter = row_number(), cstemperc = cstem/max(cstem))

d.rac_baseline.e <- d.lar %>% filter(treatment=="baseline",forest_type=="evergreen",code!="stump") %>%
  group_by(treatment,forest_type,species_ID) %>% summarise(nstem = n()) %>%
  arrange(-nstem) %>% mutate(cstem = cumsum(nstem),scaled_nstem = (nstem/1)*(1/0.49),
                             counter = row_number(), cstemperc = cstem/max(cstem))

d.rac_TL.d <- d.lar %>% filter(treatment=="TL",forest_type=="deciduous",code!="stump") %>%
  group_by(treatment,forest_type,species_ID) %>% summarise(nstem = n()) %>%
  arrange(-nstem) %>% mutate(cstem = cumsum(nstem), scaled_nstem = (nstem/2)*(1/0.49),
                             counter = row_number(), cstemperc = cstem/max(cstem))

d.rac_OLnear.d <- d.lar %>% filter(treatment=="OL_near",forest_type=="deciduous",code!="stump") %>%
  group_by(treatment,forest_type,species_ID) %>% summarise(nstem = n()) %>%
  arrange(-nstem) %>% mutate(cstem = cumsum(nstem),scaled_nstem = (nstem/3)*(1/0.49),
                             counter = row_number(), cstemperc = cstem/max(cstem))

d.rac_OLfar.d <- d.lar %>% filter(treatment=="OL_far",forest_type=="deciduous",code!="stump") %>%
  group_by(treatment,forest_type,species_ID) %>% summarise(nstem = n()) %>%
  arrange(-nstem) %>% mutate(cstem = cumsum(nstem),scaled_nstem = (nstem/1)*(1/0.49),
                             counter = row_number(), cstemperc = cstem/max(cstem))

d.rac_baseline.d <- d.lar %>% filter(treatment=="baseline",forest_type=="deciduous",code!="stump") %>%
  group_by(treatment,forest_type,species_ID) %>% summarise(nstem = n()) %>%
  arrange(-nstem) %>% mutate(cstem = cumsum(nstem),scaled_nstem = (nstem/1)*(1/0.49),
                             counter = row_number(), cstemperc = cstem/max(cstem))

treatment.labs <- c("unlogged","old policy (1990s)","new policy (2007-14)", "both (2007-14 & 1990s)")
names(treatment.labs) <- c("baseline", "OL_far", "OL_near","TL")

commongg_fig3 <- list(geom_line(alpha = 0.4),
                 geom_point(alpha = 0.8), ylim(0,70),
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
                      geom_point(alpha = 0.8), ylim(0,70),
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

ggsave(plot = fig3,filename = "figure3.png",device = "png",width = 21,height = 12,units = "cm",dpi = 300)

### Figure 4: Regen ####

d.nra <- read_xlsx(path = "nra_data.xlsx",sheet = 1)
d.treecode <- read_csv("treecodes.csv")
d.nra_fin <-
  left_join(x = d.nra,
            y = d.treecode %>% select(code_new,species_ID,tmp_phenology),
            by = c("species"="code_new"))

n_unknown_stems <-
  d.nra_fin[which(is.na(d.nra_fin$tmp_phenology)),] %>%
  mutate(unknown_nos = nra_sure+natural+unsure+nra_mostly) %>%
  pull(unknown_nos) %>% sum()

d.nra_fin <- d.nra_fin %>% filter(species!="uid")
nra_plotlist <- sort(unique(d.nra$plot_name))

dat4.tmp <- tibble()
for(i in 1:n_distinct(d.nra$plot_name))
{
  iplot <- d.nra_fin %>% filter(plot_name == nra_plotlist[i])
  iplot_E <- iplot %>% filter(tmp_phenology=="E")
  iplot_D <- iplot %>% filter(tmp_phenology=="D")
  idat3 <-
    c(#instem_ar = sum(iplot$nra_sure) + sum(iplot$nra_mostly) + sum(iplot$unsure),
      #instem_nr = sum(iplot$natural),
      #insp_ar = iplot %>% filter(nra_sure>0 | unsure>0 | nra_mostly >0) %>% pull(species) %>% n_distinct(),
      #insp_nr = iplot %>% filter(natural>0) %>% pull(species) %>% n_distinct(),
      instemevergreen_ar = sum(iplot_E$nra_sure) + sum(iplot_E$nra_mostly) + sum(iplot_E$unsure),
      instemdeciduous_ar = sum(iplot_D$nra_sure) + sum(iplot_D$nra_mostly) + sum(iplot_D$unsure),
      instemevergreen_nr = sum(iplot_E$natural),
      instemdeciduous_nr = sum(iplot_D$natural))

  dat4.tmp <- bind_rows(dat4.tmp,idat3)
}

dat4 <-
  dat4.tmp %>%
  mutate(plot_name = nra_plotlist) %>%
  full_join(x = d.nra %>% select(plot_name,plot_type) %>% distinct(),
            y = .) %>%
  pivot_longer(cols = c(-plot_name,-plot_type),values_to = "count") %>%
  separate(col = "name",into = c("response","regen_type"),sep = "_",remove = T)

comparisons_fig4 <- list( c("ar", "nr"))

aov(data = dat4[dat4$regen_type=="ar",],formula = count ~ plot_type:response) %>% summary() # for maintext


nra.labs_resp <- c("deciduous saplings in","evergreen saplings in")
names(nra.labs_resp) <- c("instemdeciduous","instemevergreen")

nra.labs_ftype <- c("deciduous patches","evergreen patches")
names(nra.labs_ftype) <- c("deciduous","evergreen")

fig4 <-
ggplot(data = dat4 %>% filter(response %in% c("instemevergreen","instemdeciduous")),
         aes(y=count,x=regen_type)) +
      geom_boxplot(aes(fill=response)) +
      facet_grid(.~response + plot_type,
                 labeller = labeller(response = nra.labs_resp,
                                     plot_type = nra.labs_ftype),
                 scales = "free") +
  theme_bw() + scale_fill_manual(name ="",values = c("grey90","grey60")) +
  guides(fill=F) + xlab("") +
  scale_x_discrete(labels = c("Planted \n saplings \n(AR)",
                              "Natural \n regeneration \n(NR)")) +
  ylab("Number of saplings \n(<10cm girth & >30cm in height)") +
stat_compare_means(comparisons = comparisons_fig4,method = "t.test",
                   paired = F,label = after_stat("p.format"))

ggsave(plot = fig4,filename = "figure4.png",device = "png",width = 22,height = 14,units = "cm",dpi = 300)


