#################################################################################
#
###### DOUBLE Y AXIS PLOT: PRECIPITATION LINES ON N DEPOSITION geom_bars   ######
#
#################################################################################


# NOTA: Nadeem o Dave avevano consigliato di aggiungere ai grafici una curva di rainfall. 
# To be added, 07/02/2017
# see: http://stackoverflow.com/questions/7476022/geom-point-and-geom-line-for-multiple-datasets-on-same-graph-in-ggplot2
# i.e. each layer can have its own dataset! Problemi: scala, secondary y axis scale and labels
# ma soprattutto: http://drawar.github.io/posts/dual-y-axis-ggplot2/ molto ben spiegato!!! Sperando funzioni...

# preparing the long RF file
RF.m.vol = transform(RF.m.vol, mY = as.yearmon(as.character(mY), "%Y%m"))
RF.m.vol$month = format(RF.m.vol$mY, "%m")
RF.m.vol$year = format(RF.m.vol$mY, "%Y")
colnames(RF.m.vol)[2] = "value"
RF.m.vol$variable = "prec"
RF.m.vol$value = RF.m.vol$value/280


# 1st: create the prec line plot to be later combined with bar plots
library(gtable) # this needs to be loaded B4 ggplot, or gtable_and_grob won't run

prec <- ggplot(RF.m.vol, aes(month, value)) + 
  geom_line(colour = "dodgerblue3", group = "variable") +  
  facet_grid(year ~ .) +
  ggtitle("precipitation\n") +
  labs(x=NULL,y=NULL) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,200), sec.axis = sec_axis(~.*5, name = "Relative humidity [%]")) +
  theme(
    panel.background = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_line(color = "gray50", size = 0.5),
    panel.grid.major.x = element_blank(),
    text = element_text(),
    axis.text.y = element_text(colour="dodgerblue3", size=14),
    axis.text.x = element_text(size = 14),
    axis.ticks = element_line(colour = 'gray50'),
    axis.ticks.length = unit(.25, "cm"),
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    plot.title = element_text(hjust = 0.85, vjust=2.12, colour = "dodgerblue3", size = 14))

#  Combine the plots: the function

double_axis_graph <- function(graf1,graf2){
  graf1 <- graf1
  graf2 <- graf2
  
  gtable1 <- ggplot_gtable(ggplot_build(graf1))
  gtable2 <- ggplot_gtable(ggplot_build(graf2))
  
  
  
  par <- c(subset(gtable1[['layout']], name=='panel', select=t:r))
  graf <- gtable_add_grob(gtable1, gtable2[['grobs']][[which(gtable2[['layout']][['name']]=='panel')]],
          par['t'],par['l'],par['b'],par['r'])
  
  ia <- which(gtable2[['layout']][['name']]=='axis-l')
  
  ga <- gtable2[['grobs']][[ia]]
  
  ax <- ga[['children']][[2]]
  
  ax[['widths']] <- rev(ax[['widths']])
  
  ax[['grobs']] <- rev(ax[['grobs']])
  
  ax[['grobs']][[1]][['x']] <- ax[['grobs']][[1]][['x']] - unit(1,'npc') + unit(0.15,'cm')
  
  graf <- gtable_add_cols(graf, gtable2[['widths']][gtable2[['layout']][ia, ][['l']]], length(graf[['widths']])-1)
  
  graf <- gtable_add_grob(graf, ax, par['t'], length(graf[['widths']])-1, par['b'])
  
  return(graf)
  
}

# put the plots together

g1.1 <- plot.RFfog + theme_bw() + theme(legend.position="top")
g2.1 <- prec + theme_bw() + theme(panel.grid=element_blank()) +
  theme(panel.background = element_rect(fill = NA))
plot(double_axis_graph(g1.1,g2.1))
