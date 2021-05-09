library(Ecdat)
library(dplyr)
library(flexsurv)
library(ggplot2)
library(lmtest)
library(survival)
library(xtable)

data(UnempDur); data <- UnempDur; rm(UnempDur)

sans <- 'Segoe UI'
sans <- 'Roboto Condensed'
figures <- 'Lato'
mono <- 'Operator Mono Medium'
myblack <- '#111111'
mywhite <- '#fafbfc'

theme_set(ggthemes::theme_tufte() +
    theme(
        axis.text = element_text(size = 6, family = sans),
        # axis.ticks = element_line(size = .2),
        axis.ticks = element_blank(),
        axis.title = element_text(size = 6, family = sans, hjust = .95),
        legend.background = element_rect(fill = mywhite, colour = mywhite),
        legend.key = element_rect(fill = mywhite, colour = mywhite),
        legend.text = element_text(family = mono, size = 6),
        legend.title = element_text(family = mono, size = 6),
        panel.background = element_rect(fill = mywhite, colour = mywhite),
        plot.background = element_rect(fill = mywhite, colour = mywhite),
        plot.margin = margin(.1, .1, .1, .1, "cm"),
        plot.title = element_text(size = 6, family = sans),
        strip.text = element_text(size = 6, family = sans, hjust = 0),
        ))

fullwidth <- theme(
    axis.text = element_text(size = 13),
    axis.title = element_text(size = 13),
    plot.title = element_text(size = 14),
    strip.text = element_text(size = 13)
)

plot_save <- function(name, width = 2, height = 1.5, dpi = 320) {
    path <- paste0('~/mhoirup.github.io/assets/unemp/', name)
    ggplot2::ggsave(path, width = width, height = height, dpi = dpi)
}
