source('~/Code/unemployment/imports.R')

tibble::as.tibble(data)
summaries <- dsummary(data, digits = 2)
fcode <- function(x) ifelse(is.na(x), NA, paste0('`', x, '`'))

Reduce(function(...) full_join(..., by = c('var', 'type')), summaries) %>%
    select(-contains('NAs')) %>%
    arrange(match(var, names(data))) %>%
    mutate_at(vars(var, type, mode), fcode) %>%
    mutate(mode = ifelse(!is.na(mode), paste0(mode, ' (',
        formatC(prop.mode, 2, format = 'f'), '%)'), '')) %>%
    select(-prop.mode) %>%
    mutate_all(function(x) ifelse(is.na(x), '', x)) %>%
    knitr::kable()

ggplot(data, aes(spell)) +
    ggthemes::geom_rangeframe() +
    geom_bar(fill = myblack, colour = 'white', width = 0.5) +
    scale_x_continuous(breaks = c(0, 10, 20, 30), limits = c(0, 30)) +
    scale_y_continuous(breaks = c(0, 300, 600), limits = c(0, 600)) +
    labs(x = 'Unemployment Duration', y = 'Count')

plot_save('spell_hist.png')

select(data, contains('censor')) %>%
    tidyr::gather(key, value) %>%
    mutate(value = ifelse(value == 1, 'True', 'False')) %>%
    ggplot(aes(value)) +
    geom_bar(fill = myblack, colour = 'white') +
    scale_y_continuous(breaks = c(0, 1500, 3000), labels = scales::number) +
    labs(x = 'Value', y = 'Count') +
    facet_wrap(~ key) +
    theme(strip.text = element_text(family = 'Operator Mono Medium', size = 6),
        axis.text.x = element_text(family = 'Operator Mono Book'),
        axis.ticks.x = element_blank())

plot_save('censored_hist.png')
sum(data$censor4); nrow(data) - sum(data$censor4)

ggplot(data, aes(age)) +
    ggthemes::geom_rangeframe() +
    geom_bar(fill = myblack, colour = 'white', width = 0.7) +
    scale_y_continuous(breaks = c(0, 75, 150)) +
    labs(x = 'Age', y = 'Count')

plot_save('age_hist.png')
sum(data$age < 40)
(sum(data$age < 40) / nrow(data)) * 100

densities <- data[, grepl('rate', names(data))] %>%
    lapply(function(x) density(x)[c('x', 'y')]) %>%
    as.data.frame()

stack(select(densities, contains('.x'))) %>%
    cbind(stack(select(densities, contains('.y')))) %>%
    setNames(c('value', 'ind', 'density', 'var')) %>%
    mutate(var = gsub('\\..*', '', var)) %>%
    ggplot(aes(value, density)) +
    ggthemes::geom_rangeframe(sides = 'b') +
    geom_area(fill = myblack, size = 0.5) +
    facet_wrap(~ var, ncol = 1) +
    scale_y_continuous(breaks = c(0, 5, 10), labels = scales::number) +
    theme(strip.text = element_text(family = 'Operator Mono Medium', hjust = 1)) +
    labs(x = 'Value', y = 'Density')

plot_save('rates_density.png', height = 2.5)

length(unique(data$reprate))
length(unique(data$disrate))

select(data, spell, censor4) %>%
    mutate(censor4 = ifelse(censor4 == 1, 'Censor4 = True',
        'Censor4 = False')) %>%
    ggplot(aes(spell)) +
    ggthemes::geom_rangeframe() +
    geom_bar(fill = myblack, colour = 'white') +
    scale_x_continuous(breaks = c(0, 10, 20, 30), limits = c(0, 30)) +
    scale_y_continuous(breaks = c(0, 250, 500)) +
    facet_wrap(~ censor4) +
    theme(strip.text = element_text(family = 'Operator Mono Medium', size = 19,
        vjust = -1.2), axis.text = element_text(size = 16),
        axis.title = element_text(size = 16)) +
    labs(x = 'Unemployment Duration', y = 'Count')

ggsave('~/mhoirup.github.io/assets/unemp/spell_censor4.png')

plot_save('spell_censor4.png', height = 1.3, width = 2, dpi = 320)
cor(data$censor4, data$spell)


ggplot(data, aes(reprate)) +
    ggthemes::geom_rangeframe() +
    geom_density(fill = myblack, size = .2) +
    scale_y_continuous(breaks = c(0, 5, 10), labels = scales::number) +
    labs(x = 'Replacement Rate', y = 'Density')

plot_save('reprate_hist.png')



ggplot(data, aes(logwage)) +
    ggthemes::geom_rangeframe() +
    geom_density(fill = myblack, size = .2) +
    scale_x_continuous(labels = scales::number) +
    labs(x = 'Logarithmic Weekly Earnings', y = 'Density')

plot_save('logwage_hist.png')

ggplot(data, aes(tenure)) +
    ggthemes::geom_rangeframe() +
    geom_bar(fill = myblack, colour = 'white') +
    labs(x = 'Tenure', y = 'Count')

plot_save('tenure_hist.png')

ggplot(data, aes(age^2, tenure^2)) +
    geom_point() +
    ggthemes::geom_rangeframe()

uvnonparam <- survfit(Surv(spell, censor4 == 0) ~ 1, data)

data.frame(dur = uvnonparam$time, surv = uvnonparam$surv) %>%
    ggplot(aes(dur, surv)) +
    ggthemes::geom_rangeframe() +
    geom_line(colour = myblack) +
    geom_ribbon(aes(ymin = uvnonparam$lower, ymax = uvnonparam$upper),
        alpha = .2) +
    scale_x_continuous(breaks = seq(0, 30, 10), limits = c(0, 30)) +
    scale_y_continuous(breaks = c(0, .5, 1), limits = c(0, 1)) +
    labs(x = 'Unemployment Duration', y = 'Survivor Function Estimate')

plot_save('kaplan_meier.png')

mvnonparam <- summary(survfit(Surv(spell, censor4 == 0) ~ ui, data))

data.frame(time = mvnonparam$time, surv = mvnonparam$surv,
    strata = mvnonparam$strata, lower = mvnonparam$lower,
    upper = mvnonparam$upper) %>%
    mutate(strata = recode(strata, 'ui=yes' = 'True', 'ui=no' = 'False')) %>%
    ggplot(aes(time, surv)) +
    ggthemes::geom_rangeframe() +
    geom_line(aes(lty = strata), colour = myblack) +
    geom_ribbon(aes(ymin = lower, ymax = upper, group = strata), alpha = .2,
        show.legend = FALSE) +
    scale_x_continuous(breaks = seq(0, 30, 10), limits = c(0, 30)) +
    scale_y_continuous(breaks = c(0, .5, 1)) +
    labs(x = 'Unemployment Duration', y = 'Survivor Function Estimate',
        lty = 'ui') +
    theme(legend.position = c(0.8, 0.8),
        plot.margin = margin(.1, -.09, .1, .1, "cm"))

plot_save('kaplan_meier_mv.png')

chazci <- function(model, alpha = .05) {
    lower <- model$cumhaz * exp(qnorm(alpha / 2, 0, 1) * model$std.chaz)
    upper <- model$cumhaz * exp(-qnorm(alpha / 2, 0, 1) * model$std.chaz)
    data.frame(lower, upper)
}

data.frame(dur = uvnonparam$time, chaz = uvnonparam$cumhaz) %>%
    cbind(chazci(uvnonparam)) %>%
    ggplot(aes(dur, chaz)) +
    ggthemes::geom_rangeframe() +
    geom_line(colour = myblack) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2) +
    scale_x_continuous(breaks = seq(0, 30, 10), limits = c(0, 30)) +
    scale_y_continuous(breaks = c(0, 1, 2, 3), limits = c(0, 3)) +
    labs(x = 'Unemployment Duration', y = 'Cumulative Hazard Estimate') +
    theme(axis.ticks = element_line(size = .2))

plot_save('nelson_aalen.png')

data.frame(time = mvnonparam$time, chaz = mvnonparam$cumhaz,
    strata = mvnonparam$strata) %>%
    cbind(chazci(mvnonparam)) %>%
    mutate(strata = recode(strata, 'ui=yes' = 'True', 'ui=no' = 'False')) %>%
    ggplot(aes(time, chaz)) +
    ggthemes::geom_rangeframe() +
    geom_line(aes(lty = strata), colour = myblack) +
    geom_ribbon(aes(ymin = lower, ymax = upper, group = strata), alpha = .2,
        show.legend = FALSE) +
    scale_x_continuous(breaks = seq(0, 30, 10), limits = c(0, 30)) +
    scale_y_continuous(labels = scales::number) +
    labs(x = 'Unemployment Duration', y = 'Cumulative Hazard Estimate',
        lty = 'ui') +
    theme(legend.position = c(.5, .9), legend.direction = 'horizontal',
        legend.text = element_text(family = 'Operator Mono Medium', size = 6),
        legend.title = element_text(family = 'Operator Mono Medium', size = 6),
        plot.margin = margin(.1, -.09, .1, .1, "cm"))

plot_save('nelson_aalen_mv.png')
