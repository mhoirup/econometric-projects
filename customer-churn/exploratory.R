source('~/Code/customer-churn/imports.R')

fcode <- function(x) paste0('`', x, '`')
summaries <- dsummary(data, 2)
mutate(summaries$categorical, var = fcode(var), type = fcode(type)) %>%
    knitr::kable()

mutate(summaries$numeric, var = fcode(var), type = fcode(type)) %>%
    knitr::kable()

# Data visualisation. First every variable is plotted against 'churn'
# together with its histogram. Patterns were found in the variables
# 'n_transactions', 'transaction_amount', 'change_n_transactions',
# 'change_transaction_amount' and 'n_contracts', and they have been graphed
# separately.

princomp <- prcomp(select_if(data, is.numeric), center = F, scale = T)
summary(princomp)

data.frame(princomp$x[,1:2]) %>%
    mutate(churn = data$churn) %>%
    ggplot(aes(PC1, PC2, fill = churn, colour = churn)) +
    geom_point(shape = 21, size = 4) +
    scale_fill_manual(values = c('#000000', '#ffffff')) +
    scale_colour_manual(values = c('#ffffff', '#000000'), guide = 'none') +
    guides(fill = guide_legend(override.aes = list(size =  10))) +
    scale_x_continuous(name = 'First Component', labels = number) +
    scale_y_continuous(name = 'Second Component', labels = number) +
    labs(fill = '', title = 'First Two Principal Components of the Data',
        subtitle = 'Cumulative Proportion: .869')

ggsave(paste0(dir, 'churn_pca.png'))

pdf('~/projects/credit-card-churn/viz.pdf', onefile = T)
for (var in names(data)) {
    type <- ifelse(!is.numeric(data[,var]) | length(unique(data[,var])) < 10,
        'count', 'stat'
        )
    histogram <- ggplot(data, aes_string(var)) +
        geom_histogram(aes(y = ..count.., fill = churn), stat = type) +
    graph <- ggplot(data, aes_string(var, 'churn')) +
        geom_point(aes(fill = churn), shape = 21, position = 'jitter')
    print(cowplot::plot_grid(graph, histogram, nrow = 2))
}

graphics.off()

write_image <- function(x, width=11.3, height=5) {
    ggsave(paste0(dir, x, '.png'), width=width, height=height)
}

graph <- function(varx, vary, ...) {
    ggplot(data, aes_string(varx, vary)) +
    geom_point(aes(fill = churn, colour = churn), shape = 21, size = 3, ...) +
    scale_fill_manual(values = c('#000000', '#ffffff')) +
    scale_colour_manual(values = c('#ffffff', '#000000'), guide = 'none') +
    guides(fill = guide_legend(override.aes = list(size =  4))) +
    labs(fill = '')
}

density <- function(var, width) {
    ggplot(data, aes_string(var)) +
    geom_histogram(binwidth = width, colour = 'black', fill = 'white') +
    geom_density(aes(fill = factor(churn, levels = c('Existing Customer',
        'Attrited Customer')), y = width * ..count..), alpha = .8) +
    scale_fill_manual(values = c('#ffffff', '#000000')) +
    scale_colour_manual(values = c('#000000', '#ffffff'), guide = 'none') +
    theme(legend.position = 'none')
}

graph('n_transactions', 'change_n_transactions', position = 'jitter') +
    scale_y_continuous(labels = number) +
    labs(x = 'Transactions', y = 'Change',
        title = 'Number of Transactions v. Change in Transactions',
        subtitle = 'Total last 12 months and change from Q4 to Q1'
        )

write_image('churn_n_transactions')

histogram1 <- density('n_transactions', 5) +
    scale_y_continuous(breaks = c(0, 300, 600, 900), labels = number) +
    labs(x = 'Transactions', y = 'Frequency', subtitle = 'Binwidth: 5.0',
        title = 'Number of Transactions'
        )

histogram2 <- density('change_n_transactions', .1) +
    scale_y_continuous(breaks = c(0, 700, 1400, 2100), labels = number) +
    scale_x_continuous(labels = number) +
    labs(x = 'Change', y = '', subtitle = 'Binwidth: 0.1',
        title = 'Change in Transactions'
        )

cowplot::plot_grid(histogram1, histogram2, align = 'hv')
write_image('churn_trans_hist', height=3)

graph('transaction_amount', 'change_transaction_amount') +
    scale_x_continuous(label = dollar) +
    scale_y_continuous(label = number) +
    labs(x = 'Amount', y = 'Change',
        title = 'Transactions Amount v. Change Amount',
        subtitle = 'Total last 12 months and change from Q4 to Q1'
        )

write_image('churn_amount')

histogram1 <- density('transaction_amount', 500) +
    scale_y_continuous(breaks = seq(0, 1600, 400)) +
    scale_x_continuous(labels = dollar, breaks = c(0, 7500, 15000)) +
    labs(x = 'Amount', y = 'Frequency', subtitle = 'Binwidth: 500',
        title = 'Transaction Amount'
        )

histogram2 <- density('change_transaction_amount', .1) +
    scale_x_continuous(labels = number, breaks = seq(0, 4)) +
    scale_y_continuous(breaks = seq(0, 2100, 700)) +
    labs(x = 'Change', y = '', subtitle = 'Binwidth: 0.1',
        title = 'Change Transaction Amount'
        )

cowplot::plot_grid(histogram1, histogram2, align = 'hv')
write_image('churn_amount_hist', height=3)

scatter <- graph('n_contracts', 'churn', position = 'jitter') +
    theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
    labs(x = '', y = '', title = 'Number of Contracts')

bar <- ggplot(data, aes(n_contracts, fill = churn)) +
    geom_bar(position = 'dodge', colour = 'black') +
    scale_fill_manual(values = c('#000000', '#ffffff')) +
    theme(legend.position = 'none') +
    scale_x_continuous(breaks = seq(0, 6)) +
    labs(x = 'Contracts', y = 'Frequency')

plot_grid(scatter, bar, nrow = 2, align = 'hv')
write_image('churn_contracts')

# Correlation analysis. The pairs ('average_open_to_buy', credit_limit'),
# ('n_transactions', 'transaction_amount') and ('age', 'months_on_book') all
# exhibit correlations above the arbitrary threshold of .65.

corr_table <- function(data, threshold = 0) {
    data.frame(as.table(cor(select_if(data, is.numeric)))) %>%
        tidyr::unite(vars, c('Var1', 'Var2'), sep = ' ') %>%
        mutate(vars = sapply(strsplit(vars, ' '),
            function(x) paste(sort(x), collapse = '/'))) %>%
        rename(corr = Freq) %>%
        unique() %>%
        filter(abs(corr) >= threshold & abs(corr) != 1) %>%
        tidyr::separate(vars, c('var1', 'var2'), sep = '/') %>%
        arrange(desc(corr)) %>%
        return()
}

corr_table(data, .65) %>%
    mutate_at(vars(var1, var2), fcode) %>%
    knitr::kable()

graph('average_open_to_buy', 'credit_limit') +
    geom_smooth(aes(group = 1), method = 'loess', se = F, colour = red,
        size = 1, show.legend = F) +
    scale_x_continuous(labels = dollar) +
    scale_y_continuous(labels = dollar) +
    labs(x = 'Average Open-To-Buy', y = 'Credit Limit',
        title = 'Average Open-To-Buy v. Credit Limit',
        subtitle = 'Averaged over the last 12 months'
        )

write_image('churn_corr1')

graph('age', 'months_on_book', position = 'jitter') +
    geom_smooth(aes(group = 1), method = 'loess', se = F, colour = red,
        size = 1, show.legend = F) +
    theme(legend.position = 'none') +
    scale_y_continuous(breaks = c(20, 40, 60), limits = c(NA, 60)) +
    scale_x_continuous(breaks = c(30, 50, 70)) +
    labs(x = 'Age', y = 'Months on Book',
        title = 'Customer Age v. Months on Book',
        subtitle = 'Artificial jitter added for clarity.'
        )

write_image('churn_corr2')

graph('transaction_amount', 'n_transactions') +
    geom_smooth(aes(group = 1), method = 'loess', se = F, colour = red,
        size = 1, show.legend = F) +
    scale_x_continuous(labels = dollar) +
    theme(legend.position = 'none') +
    labs(x = 'Amount', y = 'Transactions',
        subtitle = 'Total over the last 12 months.',
        title = 'Transaction Amount v. Number of Transactions'
        )

write_image('churn_corr3')