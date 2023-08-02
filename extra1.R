
##### code snippet to try out some NMDS visualization of large-tree and
# regeneration communities --

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
