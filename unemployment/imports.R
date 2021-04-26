library(dplyr)
library(Ecdat)
library(survival)
library(ggplot2)
library(lmtest)

data(UnempDur); data <- UnempDur; rm(UnempDur)

myblack <- '#191919'
theme_set(ggthemes::theme_tufte() +
    theme(plot.margin = margin(.1, .1, .1, .1, "cm"),
        axis.text = element_text(size = 6, family = 'Lato'),
        axis.title = element_text(size = 6, family = 'Segoe UI', hjust = .95),
        axis.ticks = element_line(size = .2),
        legend.text = element_text(family = 'Operator Mono Medium', size = 6),
        legend.title = element_text(family = 'Operator Mono Medium', size = 6),
        strip.text = element_text(size = 6),
        panel.background = element_rect(fill = '#fafbfc', colour = '#fafbfc'),
        plot.background = element_rect(fill = '#fafbfc', colour = '#fafbfc')
        ))

plot_save <- function(name, width = 2, height = 2, dpi = 320) {
    path <- paste0('~/mhoirup.github.io/assets/unemp/', name)
    ggplot2::ggsave(path, width = width, height = height, dpi = dpi)
}
