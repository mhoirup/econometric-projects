source('~/Code/unemployment/imports.R')

glimpse(data)
dsummary(data)

ggplot(data, aes(spell)) +
    ggthemes::geom_rangeframe() +
    geom_bar(fill = myblack, colour = 'white') +
    scale_x_continuous(breaks = c(0, 10, 20, 30), limits = c(0, 30)) +
    scale_y_continuous(breaks = c(0, 200, 400, 600), limits = c(0, 600)) +
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

select(data, spell, censor4) %>%
    mutate(censor4 = ifelse(censor4 == 1, 'Censor4 = True',
        'Censor4 = False')) %>%
    ggplot(aes(spell)) +
    ggthemes::geom_rangeframe() +
    geom_bar(fill = myblack, colour = 'white') +
    scale_x_continuous(breaks = c(0, 10, 20, 30), limits = c(0, 30)) +
    scale_y_continuous(breaks = c(0, 250, 500)) +
    facet_wrap(~ censor4) +
    theme(strip.text = element_text(family = 'Operator Mono Medium', size = 4),
        axis.text = element_text(size = 4),
        axis.title = element_text(size = 4)) +
    labs(x = 'Unemployment Duration', y = 'Count')

plot_save('spell_censor4.png', height = 1.3, width = 2, dpi = 320)

group_by(data, spell) %>%
    mutate(censored_percentage = (sum(censor4) * 100) / n()) %>%
    select(spell, censored_percentage) %>%
    distinct() %>%
    arrange(spell)


ggplot(data, aes(age)) +
    ggthemes::geom_rangeframe() +
    geom_histogram(binwidth = 2, fill = myblack, colour = 'white') +
    scale_x_continuous(labels = scales::number) +
    labs(x = 'Age', y = 'Count')

plot_save('age_hist.png')

ggplot(data, aes(reprate)) +
    ggthemes::geom_rangeframe() +
    geom_density(fill = myblack, size = .2) +
    scale_y_continuous(breaks = c(0, 5, 10), labels = scales::number) +
    labs(x = 'Replacement Rate', y = 'Density')

plot_save('reprate_hist.png')
length(unique(data$reprate))
sort(unique(data$reprate))

ggplot(data, aes(disrate)) +
    ggthemes::geom_rangeframe() +
    geom_density(fill = myblack, size = .2) +
    scale_y_continuous(breaks = c(0, 3, 6), labels = scales::number) +
    labs(x = 'Disregard Rate', y = 'Density')

plot_save('disrate_hist.png')
length(unique(data$disrate))
sort(unique(data$disrate))

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