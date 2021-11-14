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

if(!file.exists("data/distr_maxt.rds")){
  mi = clusterlm(ymat~visibility+Error(id/visibility),data=design,
               np=4000,return_distribution = TRUE )

distr = data_frame(
  max = apply(mi$multiple_comparison$visibility$uncorrected$distribution,1,max))

stats <- data.frame(mi$multiple_comparison$visibility$uncorrected$main)%>%
  select(statistic )

data = list(maxt = distr,
            stats = stats)
saveRDS(data, "data/distr_maxt.rds")}

data = readRDS("data/distr_maxt.rds")

maxt = data$maxt
q95<-maxt$max%>%quantile(0.95)

gg_hist_maxt<-
  ggplot(maxt, aes(x = max))+
  geom_histogram(breaks =seq(from = 0, to = 80, by = 5))+
  geom_vline(xintercept = q95)+
  labs(title = "Histogram of maximum of F statistics",
       subtitle = "The vertical line is the 95 percentile.",
       x = "Maximum of F")+
  theme_jf()+ 
  theme(legend.position='bottom',
        legend.title = element_blank())

