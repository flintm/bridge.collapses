dt <- 1
t <- c(seq(dt,200,by=dt), seq(250,2500,by=100),seq(3000,12000,by=1000))

Tr <- 1/t^2
Trp10 <- 0.9*1/t^2
Trm10 <- 1.1*1/t^2
df <- data.frame(t,Tr,Trp10,Trm10)
df.melt <- melt(df, id.vars = 't')
p<-ggplot(df.melt, aes(t,value,linetype=variable))+geom_line(size = 0.2) + scale_x_log10(limits=c(0.99,1001),breaks=c(1,10,100,1000),expand=c(0,0)) + 
  scale_y_log10() +
  scale_linetype_manual(values = c("solid","dotted","dotted"))+
  labs(x = expression(paste(T["R"]," [years]")),y = 'Probability of Exceedence') +
  guides(linetype = FALSE) +
  theme(panel.background   = element_rect(fill = "white",color="black"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.x       = element_line(color = "black"),
        axis.title.y  = element_text(color = "black", size = textP$sub[outputType]),
        axis.ticks.y       = element_line(color = "black"),
        axis.ticks.length  = unit(0.16, "cm"), 
        # axis.ticks.margin  = unit(0.16, "cm"),
        axis.text.x   = element_text(color = "black", size = textP$reg[outputType], angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y   = element_text(color = "black", size = textP$reg[outputType]),
        axis.title.x  = element_text(color = "black", size = textP$sub[outputType])
  )
p
ggsave('HazardShift.pdf',width=4,height=4)

p<-ggplot(subset(df.melt,variable == 'Tr'), aes(t,value,linetype=variable))+geom_line(size = 0.2) + scale_x_log10(limits=c(0.99,1001),breaks=c(1,10,100,1000),expand=c(0,0)) + 
  scale_y_log10(limits = c(min(df.melt$value),max(df.melt$value))) +
  # scale_linetype_manual(values = c("solid","dotted","dotted"))+
  labs(x = expression(paste(T["R"]," [years]")),y = 'Probability of Exceedence') +
  guides(linetype = FALSE) +
  theme(panel.background   = element_rect(fill = "white",color="black"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.x       = element_line(color = "black"),
        axis.title.y  = element_text(color = "black", size = textP$sub[outputType]),
        axis.ticks.y       = element_line(color = "black"),
        axis.ticks.length  = unit(0.16, "cm"), 
        # axis.ticks.margin  = unit(0.16, "cm"),
        axis.text.x   = element_text(color = "black", size = textP$reg[outputType], angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y   = element_text(color = "black", size = textP$reg[outputType]),
        axis.title.x  = element_text(color = "black", size = textP$sub[outputType])
  )
p
ggsave('HazardShiftNoC.pdf',width=4,height=4)
