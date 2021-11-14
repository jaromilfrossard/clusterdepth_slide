tb = cbind(readRDS("data/design.rds"),
           readRDS("data/signal.rds"))%>%
  pivot_longer(`-200`:`600`,names_to = "time",values_to = "signal")%>%
  mutate(time = as.numeric(time))%>%
  group_by(id,visibility,time)%>%
  summarise(signal = mean(signal),.groups = "drop")


gg_erps<-
  tb%>%
  ggplot(aes(x=time,y=signal,group=interaction(visibility,id),colour=visibility))+
  stat_summary(aes(y = signal,group = visibility,colour=visibility), fun=mean, geom="line",lwd=2)+
  geom_line(alpha=0.5)+
  geom_vline(xintercept = 0)+
  scale_y_reverse()+
  scale_x_continuous(expand = c(0,0))+
  labs(title = "Erps for 15 subjects in 2 conditions",
       subtitle = "Channel P07. Sampling rate: 512Hz",
       y = "EEG signal",
       x = "time [ms]",
       caption ="source: (Tipura, Renaud, and Pegna 2019)")+
  theme_jf()+
  theme(plot.margin = margin(0,20,0,0))