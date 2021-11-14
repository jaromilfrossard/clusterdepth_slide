


for(i in list.files("../permut_altern/function",full.names = T)){source(i)}

nt =80
thresh = 3.9
betamax = 0.8


sigma = as.matrix(Matrix::nearPD(circulant(corr_fun_exp(tau = 0:(nt-1),nu=2,R = nt/3)))$mat)

tb_beta = tibble(beta_name = c("Full null", "Alone", "Both", "Tail","Head", "Center"),
                 beta = list(rep(0,nt),
                             c(rep(0,55),rep(betamax*1.6,5),rep(0,20)),
                             c(rep(0,38),rep(betamax,5),rep(0,37)),
                             c(rep(0,20),rep(betamax,15),rep(0,45)),
                             c(rep(0,45),rep(betamax,15),rep(0,20)),
                             c(rep(0,25),rep(betamax,12),rep(0,6),rep(betamax,12),rep(0,25))))%>%
  mutate(beta = pmap(list(beta,beta_name),function(bi,bni){
    if(bni=="Alone"){bi*0.8}else if(bni=="Both"){bi*0.9}else{bi}
    
  }))

tb_design =
  expand_grid(f1 = LETTERS[1:2],
              id = 1:10)%>%
  select(-id)


tb <- tibble(seed =44, #44, 68
             design =  list(tb_design),
             sigma = list(sigma))%>%
  expand_grid(tb_beta)

tb<-
  tb%>%
  mutate(err = pmap(list(sigma,design,seed),function(sii,dei,seedi){
    set.seed(seedi)
    MASS:::mvrnorm(nrow(dei), mu =rep(0,nt),Sigma = sii)}))

tb<-
  tb%>%
  mutate(fit = pmap(list(beta,design),function(bei,dei){
    model.matrix(~f1,dei,contrasts.arg = list(f1 = contr.sum))%*%rbind(0,bei)
  }))

tb<-
  tb%>%
  mutate(signal = pmap(list(fit,err),`+`))


tb<-
  tb%>%
  expand_grid(seed_perm = c(1))%>%
  mutate(signal = pmap(list(signal,seed_perm),function(si,seedi){
    if(seedi ==1){
      permut = 1:nrow(si)
    }else{
      set.seed(seedi)
      permut = sample(nrow(si),nrow(si))
    }
    si[permut,]
  }))



tb$model = vector(mode = "list", length = nrow(tb))

for(i in seq_len(nrow(tb))){
  if(i ==1){np=200;rd =T}else{np = 2;rd=F}
  tb$model[[i]] = permuco::clusterlm(tb$signal[[i]]~f1,data = tb$design[[i]], np= np,threshold = thresh,return_distribution = T)
}






tb_beta<-
  tb%>%
  select(seed_perm,beta_name,beta)%>%
  unnest(beta)%>%
  group_by(seed_perm,beta_name)%>%
  mutate(sample = seq_len(n()))%>%
  ungroup()


tb_signal<-
  tb%>%
  mutate(data_plot =pmap(list(design,signal),function(x,y)bind_cols(x,as.data.frame(y))))%>%
  select(seed_perm,data_plot,beta_name)%>%
  unnest(data_plot)%>%
  group_by(seed_perm,beta_name)%>%
  mutate(participant = seq_len(n()))%>%
  ungroup()%>%
  pivot_longer( -c(seed_perm,f1,participant,beta_name),names_to = "sample",values_to = "signal")%>%
  mutate(sample = str_remove(sample,"V"),
         sample = as.numeric(sample))%>%
  rename(factor=f1)%>%
  mutate(type = case_when(seed_perm==1~"obs",
                          TRUE ~"perm"))




##########################################3




#######################################
tb_stat<-
  tb%>%
  mutate(stat = map(model,function(mi){
    tibble(statistic = mi$multiple_comparison$f1$uncorrected$main[,1])%>%
      mutate(sample =seq_len(n()))
  }))%>%
  select(seed_perm,stat,beta_name)%>%
  unnest(stat)%>%
  left_join(tb_beta,by = c("seed_perm" = "seed_perm", "sample" = "sample", "beta_name" = "beta_name"))%>%
  filter(sample>15&sample<66)%>%
  group_by(beta_name)%>%
  mutate(sample = 1:n())%>%
  ungroup()

maxs = max(tb_stat$sample)


tb_stat<-
  tb_stat%>%
  group_by(seed_perm,beta_name)%>%
  mutate(cluster = permuco:::get_cluster_matrix(matrix(statistic,nrow=1),threshold = thresh,
                                                side ="all")%>%as.integer())%>%
  mutate(cluster_lgc =  case_when(cluster==0~NA,
                                  TRUE~TRUE))%>%
  mutate(stat_cluster = statistic*cluster_lgc)%>%
  mutate(true_effect = abs(beta)>1e-8)%>%
  mutate(true_effect_na = case_when(true_effect~true_effect,
                                    !true_effect~NA))%>%
  mutate(type = if_else(cluster>0, "Non-significant",NA_character_),
         type = case_when(beta_name%in%c("Head","Tail", "Both","Center")&cluster>0~"False positive",
                          TRUE~type),
         type = case_when(true_effect~"True positive",
                          TRUE~type))



tb_bg <-
  tb_stat%>%
  ungroup()%>%
  mutate(ymax = max(statistic))%>%
  group_by(beta_name)%>%
  summarise(xmin = min(sample[replace_na(true_effect_na,F)]),
            xmax = max(sample[replace_na(true_effect_na,F)]),
            ymax = mean(ymax))%>%
  mutate(xmin = case_when(xmin == Inf~ NA_real_,
                          TRUE~xmin),
         xmax = case_when(xmax == -Inf~ NA_real_,
                          TRUE~xmax))%>%
  filter(!beta_name=="Center")

### ADD center grey aera
tb_stat%>%
  filter(beta_name=="Center")%>%
  filter(beta!=0)
tb_bg<-
  tb_bg %>%
  bind_rows(tibble(beta_name = "Center", xmin = 11,xmax = 22,ymax = tb_bg$ymax[1]))%>%
  bind_rows(tibble(beta_name = "Center", xmin = 29,xmax = 40,ymax = tb_bg$ymax[1]))%>%
  mutate(beta_name = factor(beta_name, levels=c("Full null", "Alone", "Head","Tail","Both", "Center")))



gg_fp<-
  tb_stat%>%
  filter(seed_perm==1)%>%
  mutate(beta_name = factor(beta_name, levels=c("Full null", "Alone", "Head","Tail","Both","Center")))%>%
  ggplot()+
  geom_rect(data=tb_bg,aes(ymin=-Inf,ymax=+Inf,xmin=xmin,xmax=xmax),fill = "grey",alpha = 0.7)+
  geom_area(aes(x = sample, y=stat_cluster,fill =  cluster_lgc,group =cluster),fill ="#E69F00",alpha =0.6,show.legend = F)+
  #  geom_area(aes(x = sample, y = stat_cluster*true_effect,fill =  cluster_lgc,group =cluster),alpha =0.7,show.legend = F)+
  #scale_fill_manual(na.value="transparent",values ="#E69F00")+
  geom_line(aes(x = sample,y = statistic),size = 1.2)+
  geom_point(aes(x = sample,y = statistic,color = type))+
  scale_color_discrete(na.value="transparent",na.translate=FALSE)+
  # geom_line(size = 1, aes(x = sample*true_effect_na ,y= 15,group = beta_name),color ="red",
  #           arrow = arrow(length=unit(0.30,"cm"),angle = 90, ends="both", type = "open"))+
  geom_hline(yintercept = thresh,linetype = "dashed")+
  scale_x_continuous(limits = c(1,maxs), expand = c(0, 0))+
  labs(y = "Statistic",
       x = "Time points",
       title = "Point-Wise Interpretation",
       caption = "The grey areas represent time points under H1.")+
  facet_wrap(~beta_name, ncol = 2)+
  theme_jf()+
  theme(legend.position='bottom',
        legend.title = element_blank())


