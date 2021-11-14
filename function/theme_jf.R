theme_jf = function(){
  xaringanthemer::theme_xaringan()+
    theme(panel.border = element_rect(colour="black",linetype = "solid",fill="transparent"))+
    theme(legend.position = "bottom",
          plot.caption = element_text(hjust=0,face="italic"))
}