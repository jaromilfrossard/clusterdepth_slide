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
tb_perm = tibble(pi = seq(from = 0,to = 8))%>%
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
  
  fstat[[i]] = tb_f
  
}


colnames(ymat) = paste0("t",colnames(ymat))

tb_perm$fstat = fstat

threshold = modi$threshold



gg_cl <-
  tb_perm%>%
  select(pi,fstat)%>%
  unnest(fstat)%>%
  mutate(cluster = case_when(cluster==0~NA_real_,
                             TRUE~cluster))%>%
  mutate(fcluster = case_when(is.na(cluster)~NA_real_,
                              TRUE~fstat))%>%
  ggplot()+
  geom_ribbon(aes(x=time,ymin = 0,ymax = fcluster ),alpha = 0.4)+
  geom_line(aes(x=time,y = fstat))+
  geom_point(aes(x=time,y = fstat))+
  geom_hline(yintercept = threshold)+
  scale_x_continuous(expand = c(0,0))+
  labs(
    title = "The cluster mass test",
    x="time [ms]", y = "F-Statistic",
    caption= "Using a pre-defined threshold, we define clusters. For each cluster, we\ncompute the cluster mass as the sum of statistics within the cluster. The\nnull distribution of the cluster mass is computed by permutation.")+
  facet_wrap(~pi)+
  theme_jf()

