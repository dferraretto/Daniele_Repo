##############################################################################
#                 TF AND SF BARRELS CALIBRATION PLOT                         #
##############################################################################
rm(list=ls())

calibration <- read_delim("~/Daniele_Repo/Chapter_Methodology/Calibration.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

summary(calibration)
calibration$barrel = as.factor(calibration$barrel)



lm_eqn <- function(calibration){
  m <- lm(vol ~ depth, calibration);
  eq <- substitute(italic(volume) == a + b %.% italic(depth)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

library(plyr)

split_regr = ddply(calibration, c("barrel"), lm_eqn) # è utile per imparare coze nuove ma ora ci semplifichiamo la vita

#splot_regr = data.frame(barrel = c("SF", "TFQ", "TFR"), R = c(expression(paste("y = 1.3756x + 0.6078;", "R"^2, " = 0.999")), 
                      #                                       expression(paste("y = 1.3756x + 0.6078;", "R"^2, " = 0.999")),
                      #                                       expression(paste("y = 1.3756x + 0.6078;", "R"^2, " = 0.999"))))

# NON RIESCO A FARGLI PRENDERE L'ESPRESSIONE DIMMERDA IN DATA.FRAME. MA SE NON LA PRENDO COSI COME FACCIO?

library(ggplot2)

ggplot(data = calibration, aes(x = depth, y = vol)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(barrel ~ .) +
  geom_text(data=split_regr, aes(x=c(11,12,10), y=c(70,70,70), label=V1), 
            parse = TRUE, inherit.aes = FALSE,show.legend=F) +
  theme(panel.border = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA)) +
  labs(x = "depth (cm)", y = "volume (l)") +
    ggtitle ("Calibration curves by barrel type") +
  theme_bw(base_size = 14) +
theme(plot.title=element_text(size = 16, hjust = 0.5))
  
