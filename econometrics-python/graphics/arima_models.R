library(ggplot2)

myblack <- '#191919'
theme_set(ggthemes::theme_tufte() +
    theme(plot.margin = margin(.1, .1, .1, .1, "cm"),
        axis.text = element_text(size = 6, family = 'Lato'),
        axis.title = element_text(size = 6, family = 'Lato', hjust = .95),
        axis.ticks = element_line(size = .2),
        legend.text = element_text(family = 'Operator Mono Medium', size = 6),
        legend.title = element_text(family = 'Operator Mono Medium', size = 6),
        ))

plot_save <- function(name, width = 2, height = 2) {
    path <- paste0('~/Downloads/tufte-jekyll-master/assets/numpy-arima/', name)
    ggsave(path, width = width, height = height)
}

data <- quantmod::getSymbols.FRED('DEXUSEU', auto.assign = FALSE)
data <- na.omit(data['2018/'])

lims <- as.POSIXct(strptime(c('2020-01-01', '2021-07-01'), format = '%Y-%m-%d'))

lims <- as.Date(c('2020-01-01', '2021-07-01'))

ggplot(data, aes(Index, DEXUSEU)) +
    ggthemes::geom_rangeframe() +
    geom_line(colour = myblack, size = .2) +
    scale_x_date(labels = scales::date_format("%h '%y")) +
    labs(x = 'Date', y = 'USD/EUR')

plot_save('series_level.png')


ggplot(na.omit(diff(log(data))) * 100, aes(Index, DEXUSEU)) +
    ggthemes::geom_rangeframe() +
    geom_line() +
    scale_y_continuous(labels = scales::number)

