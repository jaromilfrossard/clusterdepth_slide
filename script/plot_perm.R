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
tb_perm = tibble(pi = seq(from = 0,to = 2))%>%
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
  fstat[[i]] = tb_f
  
}


colnames(ymat) = paste0("t",colnames(ymat))

tb_perm$fstat = fstat
tb_perm$signal = replicate(n = nrow(tb_perm),as.data.frame(ymat),simplify = F)
tb_ggy <-  
  tb_perm%>%
  transmute(pi,
            design = pmap(list(design,signal),bind_cols))%>%
  unnest(design)%>%
  pivot_longer(cols = t0.5:t600,names_to = "time",values_to = "signal")%>%
  mutate(
    time = str_remove(time,"t"),
    time = as.numeric(time))




ggy <- 
  tb_ggy%>%
  ggplot(aes(x = time, y = signal,group = interaction(id,visibility), color = visibility))+
  stat_summary(aes(y = signal,group = visibility,colour=visibility), fun=mean, geom="line",lwd=2)+
  geom_line(alpha=0.5)+
  geom_vline(xintercept = 0)+
  scale_y_reverse()+
  scale_x_continuous(expand = c(0,0))+
  labs(y = "EEG signal",
       x = "time [ms]")+
  facet_grid(rows = vars(pi))+
  theme_jf()+
  theme(plot.margin = margin(0,20,0,0))

ggf <-
  tb_perm%>%
  select(pi,fstat)%>%
  unnest(fstat)%>%
  group_by(pi)%>%
  mutate(fmax = max(fstat),
         fmax = case_when(fstat<fmax~NA_real_,
                          TRUE~fmax))%>%
  ggplot(aes(x=time,y = fstat))+
  geom_line()+
  geom_point()+
  geom_point(aes(x=time,y = fmax), color = "red",size=4)+
  scale_x_continuous(expand = c(0,0))+
  labs(x="time [ms]", y = "F-Statistic")+
  facet_grid(rows = vars(pi))+
  theme_jf()

gg_perm<-
  (ggy+ggf)+
  plot_annotation(
    title = 'Permuted data and permuted statistics',
    theme = theme_jf())

