library(Calahanlab)
library(ggplot2)
source("EACCRPBT.R")
source("Settings.R")

# Figure 3 - linear productivity model
this.fig.dir <- paste0(fig.dir, "Figure 3/")
fig.fn <- paste0(this.fig.dir, "Figure 3.tiff")

a3.df <- data.frame(lat=c(0, 90))
a3.df$prod <- mod.m * a3.df$lat + mod.b

line.size = 0.05
text.size = 7.5
legend.text.size = 4
theme.overrides <- list(theme(axis.line = element_line(color='black', size=line.size),
      plot.margin = unit(c(2, 2, 2, 2),'mm'),
      axis.title.x = element_text(family="Arial", color="black", size=text.size, margin=margin(1,0,0,0,"mm")),
      axis.title.y = element_text(family="Arial", color="black", face="bold", size=text.size,  margin=margin(0,1,0,0,"mm"), angle=90),
      axis.ticks = element_line(size=0.3),
      axis.text.x = element_text(family="Arial", color="black", size=text.size, margin=margin(0.5,0,0,0,"mm")),
      axis.text.y = element_text(family="Arial", color="black", face="bold", size=text.size, margin=margin(0,0.5,0,0,"mm")),
      axis.line.x = element_line(size=0.3, color="black"),
      axis.line.y = element_line(size=0.3, color="black"),
      legend.key = element_rect(fill="white"),
      legend.position = "right",
      legend.title = element_text(family="Arial", color="black", face="bold", size=legend.text.size),
      legend.text = element_text(family="Arial", color="black", face="bold", size=legend.text.size),
      legend.key.height=unit(2, "mm")
      )
)

plot <- ggplot(data=a3.df, aes(x=lat, y=prod)) +
    geom_line() +
    theme.opts +
    theme.overrides +
    labs(x="Absolute value of latitude (degrees)", y=expression("Algal Productivity (t ha"^-1*" yr"^-1*")"), linetype=1)
ggsave(fig.fn, plot=plot, width=fig.wid/2, height=fig.hgt/2, dpi=fig.ldpi)
RemoveWhiteEdges(fig.fn, fig.fn, fig.rdpi)
ResaveTIFF(fig.fn, fig.fn, fig.ldpi, fig.ldpi, fig.wid)
