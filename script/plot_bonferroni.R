

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
  select(`-200`:`-0.5`)%>%
  as.matrix()

design <-
  design%>%
  select(id,visibility)

mod = clusterlm(ymat~visibility+Error(id/visibility),data=design,np=1)
tb_f = summary(mod,table_type = "full")$visibility%>%
  as_tibble()%>%
  select(fisher)%>%
  rename(fstat = fisher)%>%
  mutate(time = colnames(ymat),
         time = as.numeric(time),
         pvalue = pf(q =fstat, 1,14,lower.tail = F))

threshold = mod$threshold[1]
gg_f<-
  tb_f%>%
  ggplot(aes(x=time,y = fstat))+
  geom_line()+
  geom_point()+
  geom_hline(yintercept = threshold)+
  scale_x_continuous(expand = c(0,0))+
  labs(x="", y = "F-Statistic")+
  theme_jf()


gg_p<-
  tb_f%>%
  ggplot(aes(x=time,y = pvalue))+
  geom_line()+
  geom_point()+
  scale_x_continuous(expand = c(0,0))+
  labs(x="time [ms]", y = "p-value")+
  geom_hline(yintercept = 0.05,colour = "black")+
  geom_hline(yintercept = 0.05/ncol(ymat),colour = "red")+
  theme_jf()

gg_bonf<-
  (gg_f/gg_p)+
  plot_annotation(
    title = 'F-Statistic and p-value under H0',
    subtitle = 'In the bottom panel, the univariate (black) and\nBonferroni (red) threshold are shown.',
    theme = theme_jf())