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

if(!file.exists("data/data_all_multcomp.rds")){
model = clusterlm(ymat~visibility+Error(id/visibility),data=design,
               np=4000,return_distribution = TRUE,
               multcomp = c("maxT","troendle","clustermass",
                            "clusterdepth", "tfce"))

tb_pvalue <- 
tibble(
  time_ms = as.numeric(colnames(ymat)),
  time = seq_along(colnames(ymat)),
  fstat = model$multiple_comparison$visibility$uncorrected$main[,1],
  clusterid = model$multiple_comparison$visibility$clustermass$main[,3],
  pvalue_maxt = summary(model,multcomp = "maxT",table_type = "full")[[1]][,2],
  pvalue_troendle = summary(model,multcomp = "troendle",table_type = "full")[[1]][,2],
  pvalue_cm = summary(model,multcomp = "clustermass",table_type = "full")[[1]][,3],
  pvalue_cd = summary(model,multcomp = "clusterdepth",table_type = "full")[[1]][,2],
  pvalue_tfce = summary(model,multcomp = "tfce",table_type = "full")[[1]][,3],
  clustermass = summary(model,multcomp = "clustermass",table_type = "full")[[1]][,2],
  tfce = summary(model,multcomp = "tfce",table_type = "full")[[1]][,2]
)

saveRDS(list(tb_pvalue= tb_pvalue,threshold= model$threshold),"data/data_all_multcomp.rds")}

data<- readRDS("data/data_all_multcomp.rds")


gg_signal_maxt<-
  data$tb_pvalue%>%
  ggplot(aes(x=time_ms,y=fstat))+
  geom_point()+
  geom_line()+
  labs(title = "Max-T multiple comparison procedure",
       subtitle = "Significant p-pvalue in red",
       x = "time [ms]", y = "F statistic")+
  geom_point(data= filter(data$tb_pvalue,pvalue_maxt<0.05),color="red")+
  theme_jf()


gg_signal_cm<-
  data$tb_pvalue%>%
  ggplot(aes(x=time_ms,y=fstat))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = data$threshold)+
  labs(title = "Cluster mass test",
       subtitle = "Significant cluster in red and non-significant in grey",
       x = "time [ms]", y = "F statistic")+
  geom_point(data= filter(data$tb_pvalue,pvalue_cm<0.05),color="red")+
  geom_point(data= filter(data$tb_pvalue,pvalue_cm>0.05),color="grey")+
  theme_jf()


gg_signal_tfce_up<-
  data$tb_pvalue%>%
  ggplot(aes(x=time_ms,y=fstat))+
  geom_point()+
  geom_line()+
  labs(x = NULL, y = "F statistic")+
  geom_point(data= filter(data$tb_pvalue,pvalue_tfce<0.05),color="red")+
  theme_jf()


gg_signal_tfce_down<-
  data$tb_pvalue%>%
  ggplot(aes(x=time_ms,y=tfce))+
  geom_point()+
  geom_line()+
  labs(x = "time [ms]", y = "TFCE")+
  geom_point(data= filter(data$tb_pvalue,pvalue_tfce<0.05),color="red")+
  theme_jf()


gg_signal_tfce<-
  gg_signal_tfce_up/gg_signal_tfce_down+
  plot_annotation(
    title = "TFCE",
    subtitle = "Significant cluster in red and non-significant in grey",
    theme = theme_jf())




gg_signal_cd<-
  data$tb_pvalue%>%
  ggplot(aes(x=time_ms,y=fstat))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = data$threshold)+
  labs(title = "Cluster depth Tests",
       subtitle = "Significant time points in red and non-significant in grey",
       x = "time [ms]", y = "F statistic")+
  geom_point(data= filter(data$tb_pvalue,pvalue_cd<0.05),color="red")+
  geom_point(data= filter(data$tb_pvalue,pvalue_cd>0.05),color="grey")+
  theme_jf()




