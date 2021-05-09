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
    theme(strip.text = element_text(family = mono, size = 6),
        axis.text.x = element_text(family = mono), axis.ticks.x = element_blank())

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
    theme(strip.text = element_text(family = mono, hjust = 1)) +
    labs(x = 'Value', y = 'Density')

plot_save('rates_density.png', height = 2.5)
length(unique(data$reprate))
length(unique(data$disrate))

select(data, spell, censor4) %>%
    mutate(censor4 = ifelse(censor4 == 1, 'Censor4 = True',
        'Censor4 = False')) %>%
    ggplot(aes(spell)) +
    ggthemes::geom_rangeframe() +
    geom_bar(fill = myblack, colour = 'white', width = 0.3) +
    scale_x_continuous(breaks = c(0, 10, 20, 30), limits = c(0, 30)) +
    scale_y_continuous(breaks = c(0, 250, 500)) +
    facet_wrap(~ censor4) +
    theme(strip.text = element_text(family = mono, size = 19, vjust = -1.2),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16)) +
    labs(x = 'Unemployment Duration', y = 'Count')

ggsave('~/mhoirup.github.io/assets/unemp/spell_censor4.png')
cor(data$censor4, data$spell)

ggplot(data, aes(logwage)) +
    ggthemes::geom_rangeframe() +
    geom_density(fill = myblack, size = .2) +
    scale_x_continuous(labels = scales::number) +
    labs(x = 'Logarithmic Weekly Earnings', y = 'Density')

plot_save('logwage_hist.png')

ggplot(data, aes(tenure)) +
    ggthemes::geom_rangeframe() +
    geom_bar(fill = myblack, colour = 'white', width = 0.7) +
    labs(x = 'Tenure', y = 'Count')

plot_save('tenure_hist.png')
sum(data$tenure <= 5)
(sum(data$tenure <= 5) / nrow(data)) * 100

nonparam <- survfit(Surv(spell, censor4 == 0) ~ 1, data)
chazci <- function(model, alpha = .05) {
    lower <- model$cumhaz * exp(qnorm(alpha / 2, 0, 1) * model$std.chaz)
    upper <- model$cumhaz * exp(-qnorm(alpha / 2, 0, 1) * model$std.chaz)
    data.frame(lower, upper)
}

flimits <- function(x) c(floor(min(x)), ceiling(max(x)))
data.frame(estimate = nonparam$surv, lower = nonparam$lower,
    upper = nonparam$upper, type = 'Kaplan-Meier Estimator') %>%
    rbind(data.frame(estimate = nonparam$cumhaz, chazci(nonparam),
        type = 'Nelson-Aalen Estimator')) %>%
    mutate(spell = rep(nonparam$time, 2)) %>%
    ggplot(aes(spell, estimate)) +
    ggthemes::geom_rangeframe() +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2) +
    scale_y_continuous(limits = function(x) flimits(x),
        labels = function(x) formatC(x, 1, format = 'f'),
        breaks = scales::pretty_breaks(3)) +
    scale_x_continuous(limits = c(0, 30), breaks = c(0, 10, 20, 30)) +
    facet_wrap(~ type, scales = 'free_y') +
    labs(x = 'Unemployment Duration', y = 'Estimate') +
    fullwidth

ggsave('~/mhoirup.github.io/assets/unemp/nonparam_uni.png')

nonparam_stratifed <- summary(survfit(Surv(spell, censor4 == 0) ~ ui, data))
data.frame(estimate = nonparam_stratifed$surv, lower = nonparam_stratifed$lower,
    upper = nonparam_stratifed$upper, type = 'Kaplan-Meier Estimator',
    strata = nonparam_stratifed$strata) %>%
    rbind(data.frame(estimate = nonparam_stratifed$cumhaz,
        chazci(nonparam_stratifed), type = 'Nelson-Aalen Estimator',
        strata = nonparam_stratifed$strata)) %>%
    mutate(spell = rep(nonparam_stratifed$time, 2), strata = recode(strata,
        'ui=yes' = 'Recieved UI', 'ui=no' = 'No UI')) %>%
    ggplot(aes(spell, estimate, lty = strata)) +
    ggthemes::geom_rangeframe() +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2, show.legend = FALSE) +
    scale_y_continuous(limits = function(x) flimits(x),
        labels = function(x) formatC(x, 1, format = 'f'),
        breaks = scales::pretty_breaks(3)) +
    scale_x_continuous(limits = c(0, 30), breaks = c(0, 10, 20, 30)) +
    facet_wrap(~ type, scales = 'free_y') +
    guides(linetype = guide_legend(override.aes = list(size =  1))) +
    labs(x = 'Unemployment Duration', y = 'Estimate', lty = '') +
    theme(legend.text = element_text(family = sans, size = 12),
        legend.position = 'bottom', legend.key.width = unit(3, 'cm')) +
    fullwidth

ggsave('~/mhoirup.github.io/assets/unemp/nonparam_stratified.png')

fdist <- function(dist, ...) MASS::fitdistr(data$spell, dist, ...)
distributions <- c('Exponential', 'Weibull', 'Log-Normal', 'Log-Logistic')

expo <- fdist('exponential')[[1]]
weibull <- fdist('weibull')[[1]]
lnorm <- fdist('lognormal')[[1]]
loglogis <- fdist(actuar::dllogis, list(shape = 1, rate = 1))[[1]]

pdfs <- data.frame(
    dexp(data$spell, expo),
    dweibull(data$spell, weibull[1], weibull[2]),
    dlnorm(data$spell, lnorm[1], lnorm[2]),
    actuar::dllogis(data$spell, loglogis[1], loglogis[2])) %>%
    setNames(distributions)

cbind(select(data, spell), pdfs) %>%
    tidyr::gather(key, value, -spell) %>%
    ggplot(aes(spell)) +
    geom_density(aes(fill = 'Sample Density'), alpha = .2, colour = mywhite) +
    scale_fill_manual(values = myblack) +
    geom_line(aes(spell, value), colour = myblack, size = .8) +
    facet_wrap(~ key) +
    scale_y_continuous(breaks = c(0, .1, .2), limits = c(NA, .2)) +
    ggthemes::geom_rangeframe(aes(spell, value)) +
    scale_x_continuous(limits = c(0, 30), breaks = c(0, 10, 20, 30)) +
    labs(x = 'Unemployment Duration', y = 'Density', fill = '') +
    theme(legend.position = 'bottom', legend.text = element_text(family = sans)) +
    fullwidth

ggsave('~/mhoirup.github.io/assets/unemp/pdfs.png')

survivor_functions <- data.frame(
    1 - pexp(data$spell, expo),
    1 - pweibull(data$spell, weibull[1], weibull[2]),
    1 - plnorm(data$spell, lnorm[1], lnorm[2]),
    1 - actuar::pllogis(data$spell, loglogis[1], loglogis[2])) %>%
    setNames(distributions)

cbind(select(data, spell), survivor_functions) %>%
    distinct() %>%
    arrange(spell) %>%
    mutate(kaplan_meier = nonparam$surv) %>%
    tidyr::gather(key, value, -spell, -kaplan_meier) %>%
    ggplot(aes(spell, value)) +
    ggthemes::geom_rangeframe() +
    geom_area(aes(y = kaplan_meier), fill = myblack, alpha = .2, colour = mywhite) +
    geom_line(colour = myblack, size = .8) +
    facet_wrap(~ key) +
    scale_y_continuous(limits = c(NA, 1), breaks = c(0, .5, 1)) +
    labs(x = 'Unemployment Duration', y = 'Survivor Function') +
    scale_x_continuous(limits = c(0, 30), breaks = c(0, 10, 20, 30)) +
    fullwidth

ggsave('~/mhoirup.github.io/assets/unemp/surv_funcs.png')

setNames(pdfs / survivor_functions, distributions) %>%
    cbind(spell = data$spell, .) %>%
    distinct() %>%
    arrange(spell) %>%
    mutate(nelson_aalen = nonparam$cumhaz) %>%
    mutate_at(vars(all_of(distributions)), cumsum) %>%
    tidyr::gather(key, value, -spell, -nelson_aalen) %>%
    ggplot(aes(spell, value)) +
    ggthemes::geom_rangeframe() +
    geom_area(aes(y = nelson_aalen), fill = myblack, alpha = .2, colour = mywhite) +
    geom_line(colour = myblack, size = .8) +
    facet_wrap(~ key) +
    scale_y_continuous(limits = c(0, NA), breaks = c(0, 2.5, 5)) +
    scale_x_continuous(limits = c(0, 30), breaks = c(0, 10, 20, 30)) +
    labs(x = 'Unemployment Duration', y = 'Cumulative Hazard') +
    fullwidth

ggsave('~/mhoirup.github.io/assets/unemp/chaz.png')
