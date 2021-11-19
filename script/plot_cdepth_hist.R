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

if(!file.exists("data/distr_cdepth.rds")){
  mi = clusterlm(ymat~visibility+Error(id/visibility),data=design,
                 np=4000,return_distribution = TRUE)
  
  di <- mi$multiple_comparison$visibility$uncorrected$distribution
  
  cluster_head <-  permuco:::get_cluster.matrix(
    distribution =  di, threshold = mi$threshold,
    alternative = "greater", side= "starting")
  
  depth_head <- permuco:::get_clusterdepth_head(cluster_head, border = "ignore")
  
  distr_head <- permuco:::depth_distribution(di, head_mat = depth_head)
  
  colnames(distr_head) <- paste("depth",seq_len(ncol(distr_head)),sep="_")
  
  distr_head <- data.frame(distr_head)
  

  
  
  stats <- data.frame(mi$multiple_comparison$visibility$uncorrected$main)%>%
    select(statistic )
  
  data = list(cdepth = distr_head,
              stats = stats,
              threshold = mi$threshold)
  saveRDS(data, "data/distr_cdepth.rds")}



data = readRDS("data/distr_cdepth.rds")


tb_hist <-
  data$cdepth%>%
  pivot_longer(cols = everything(.),
               names_to = "depth",
               values_to = "statistic")%>%
  mutate(depth = str_remove(depth,fixed("depth_")),
         depth = as.integer(depth))%>%
  filter(depth%in%(1:5))

tb_quantile<-
  tb_hist%>%
  group_by(depth)%>%
  summarise(q95 =quantile(statistic,probs=0.95))

gg_cdepth_hist <-
  tb_hist%>%
  ggplot( aes(x = statistic))+
  geom_histogram(breaks =c(0,0.25,seq(from = data$threshold, to = 12, by = .25)))+
  geom_vline(xintercept = data$threshold)+
  #geom_vline(data= tb_quantile,aes(xintercept = q95),color="red")+
  labs(title = "Histogram of the cluster depth tests",
       x = "F Statistic")+
  facet_grid(rows = vars(depth))+
  theme_jf()+ 
  theme(legend.position='bottom',
        legend.title = element_blank())
  
gg_cdepth_matrix<-
  data$cdepth%>%
  select(depth_1:depth_5)%>%
  rename(`1st depth` = depth_1,
         `2nd depth` = depth_2,
         `3rd depth` = depth_3,
         `4th depth` = depth_4,
         `5th depth` = depth_5)%>%
  GGally::ggpairs(
    upper = list(continuous = "points"),
    lower = list(continuous ="blank"),
    diag = list(continuous = "blank"),
    progress = F,
    bins = 40)+
  theme_jf()+
  labs(title = "Correlation of the cluster depth tests")

