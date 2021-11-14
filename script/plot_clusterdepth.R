tb = cbind(readRDS("data/design.rds"),
           readRDS("data/signal.rds"))%>%
  pivot_longer(`-200`:`600`,names_to = "time",values_to = "signal")%>%
  mutate(time = as.numeric(time))%>%
  group_by(id,visibility,time)%>%
  summarise(signal = mean(signal),.groups = "drop")

design =
  tb%>%
  pivot_wider(names_from = time,values_from = signal)


ymat<-
  design%>%
  select(`0.5`:`600`)%>%
  as.matrix()

design <-
  design%>%
  select(id,visibility)

set.seed(42)
tb_perm = tibble(pi = seq(from = 0,to = 3))%>%
  mutate(design = map(pi,function(pii){
    if(pii==0){
      design
    }else{
      design%>%
        group_by(id)%>%
        mutate(visibility = visibility[sample(c(1:2),replace=F)])%>%
        ungroup()}
  }))%>%
  mutate(pi = case_when(pi == 0~"obs.",
                        TRUE~ as.character(glue("perm: #{pi}"))))%>%
  mutate(pi = factor(pi),
         pi = relevel(pi, ref="obs."))

fstat= list()
length(fstat)= nrow(tb_perm)
for(i in seq_along(fstat)){
  modi = clusterlm(ymat~visibility+Error(id/visibility),data=tb_perm$design[[i]],np=1)
  tb_f = summary(modi,table_type = "full")$visibility%>%
    as_tibble()%>%
    select(fisher)%>%
    rename(fstat = fisher)%>%
    mutate(time = colnames(ymat),
           time = as.numeric(time))
  tb_f$cluster = as.numeric(permuco:::get_cluster.matrix(matrix(tb_f$fstat,nrow=1),
                                                         modi$threshold,alternative = "greater",
                                                         side="all"))
  
  tb_f$clusterdepth = as.numeric(permuco:::get_clusterdepth_head(matrix(tb_f$cluster,nrow=1),
                                                                 border = "ignore"))
  
  fstat[[i]] = tb_f
  
}


colnames(ymat) = paste0("t",colnames(ymat))

tb_perm$fstat = fstat

threshold = modi$threshold




gg_depth_f <-
  tb_perm%>%
  select(pi,fstat)%>%
  unnest(fstat)%>%
  mutate(cluster = case_when(cluster==0~NA_real_,
                             TRUE~cluster))%>%
  mutate(clusterdepth = case_when(clusterdepth==0~NA_real_,
                                  TRUE~clusterdepth))%>%
  mutate(fcluster = case_when(is.na(cluster)~NA_real_,
                              TRUE~fstat))%>%
  ggplot()+
  geom_line(aes(x=time,y = fstat))+
  geom_hline(yintercept = threshold)+
  facet_grid(rows = vars(pi))+
  scale_x_continuous(expand = c(0,0))+
  theme_jf()


tb_depth<- 
tb_perm%>%
  select(pi,fstat)%>%
  unnest(fstat)%>%
  mutate(cluster = case_when(cluster==0~NA_real_,
                             TRUE~cluster))%>%
  mutate(clusterdepth = case_when(clusterdepth==0~NA_real_,
                             TRUE~clusterdepth))%>%
  mutate(fcluster = case_when(is.na(cluster)~NA_real_,
                              TRUE~fstat))%>%
  filter(!is.na(clusterdepth))

tb_depth<- 
  tb_depth%>%
  select(pi,cluster)%>%
  distinct()%>%
  expand_grid(clusterdepth = seq_len(max(tb_depth$clusterdepth)))%>%
  left_join(tb_depth, by= c("pi" = "pi", "cluster" ="cluster",
                            "clusterdepth" = "clusterdepth"))%>%
  mutate(fcluster= case_when(is.na(fcluster)~0,
                          TRUE~fcluster))

tb_depth_max = tb_depth%>%
  group_by(pi,clusterdepth)%>%
  summarise(fcluster = max(fcluster),.groups = "drop")

gg_depth_depth <-
  tb_depth%>%
  ggplot()+
  geom_line(aes(x=clusterdepth,y = fcluster,group=interaction(pi,cluster)),alpha = 0.5)+
  geom_point(aes(x=clusterdepth,y = fcluster,group=interaction(pi,cluster)),alpha = 0.5)+
  geom_line(data= tb_depth_max, aes(x=clusterdepth,y = fcluster))+
  geom_point(data= tb_depth_max, aes(x=clusterdepth,y = fcluster))+
  geom_hline(yintercept = threshold)+
  facet_grid(rows = vars(pi))+
  scale_x_continuous(expand = c(0,0))+
  theme_jf()
  
gg_cdepth = gg_depth_f+gg_depth_depth+  
  plot_annotation(
    title = 'Distribution by cluster-depth',
    caption = "The clusters are aligned by cluster-depth and we compute the null distribution for each cluster depth.",
  theme = theme_jf())
