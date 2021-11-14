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

if(!file.exists("data/distr_cm.rds")){
  mi = clusterlm(ymat~visibility+Error(id/visibility),data=design,
                 np=4000,return_distribution = TRUE )
  
  distr = tibble(
    cm = mi$multiple_comparison$visibility$clustermass$distribution)
  
  stats <- data.frame(mi$multiple_comparison$visibility$uncorrected$main)%>%
    select(statistic )
  
  data = list(cm = distr,
              stats = stats)
  saveRDS(data, "data/distr_cm.rds")}



data = readRDS("data/distr_cm.rds")

cm = data$cm
q95<-cm$cm%>%quantile(0.95)

gg_hist_cm<-
  ggplot(cm, aes(x = cm))+
  geom_histogram(breaks =seq(from = 0, to = 4000, by = 50))+
  geom_vline(xintercept = q95)+
  labs(title = "Histogram of maximum of the clustermass",
       subtitle = "The vertical line is the 95 percentile.",
       x = "Cluster Mass")+
  theme_jf()+ 
  theme(legend.position='bottom',
        legend.title = element_blank())
