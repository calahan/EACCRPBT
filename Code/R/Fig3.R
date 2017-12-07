library(Calahanlab)
library(ggplot2)
source("Code/R/EACCRPBT.R")
source("Code/R/Settings.R")

# Figure 3 - linear productivity model
this_fig_dir <- paste0(fig_dir, "Figure 3/")
fig_fn <- paste0(this_fig_dir, "Figure 3.tiff")

a3_df <- data.frame(lat=c(0, 90))
a3_df$prod <- mod_m * a3_df$lat + mod_b

line_size = 0.05
text_size = 7.5
legend_text_size = 4
theme_overrides <- list(theme(axis.line = element_line(color='black', size=line_size),
      plot.margin = unit(c(2, 2, 2, 2),'mm'),
      axis.title.x = element_text(family="Arial", color="black", size=text_size, margin=margin(1,0,0,0,"mm")),
      axis.title.y = element_text(family="Arial", color="black", face="bold", size=text_size,  margin=margin(0,1,0,0,"mm"), angle=90),
      axis.ticks = element_line(size=0.3),
      axis.text.x = element_text(family="Arial", color="black", size=text_size, margin=margin(0.5,0,0,0,"mm")),
      axis.text.y = element_text(family="Arial", color="black", face="bold", size=text_size, margin=margin(0,0.5,0,0,"mm")),
      axis.line.x = element_line(size=0.3, color="black"),
      axis.line.y = element_line(size=0.3, color="black"),
      legend.key = element_rect(fill="white"),
      legend.position = "right",
      legend.title = element_text(family="Arial", color="black", face="bold", size=legend_text_size),
      legend.text = element_text(family="Arial", color="black", face="bold", size=legend_text_size),
      legend.key.height=unit(2, "mm")
      )
)

plot <- ggplot(data=a3_df, aes(x=lat, y=prod)) +
    geom_line() +
    theme_opts +
    theme_overrides +
    labs(x="Absolute value of latitude (degrees)", y=expression("Algal Productivity (t ha"^-1*" yr"^-1*")"), linetype=1)
ggsave(fig_fn, plot=plot, width=fig_wid/2, height=fig_hgt/2, dpi=fig_ldpi)
RemoveWhiteEdges(fig_fn, fig_fn, fig_rdpi)
ResaveTIFF(fig_fn, fig_fn, fig_ldpi, fig_ldpi, fig_wid)
