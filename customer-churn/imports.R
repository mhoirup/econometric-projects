library(dplyr)
library(lmtest)
library(sandwich)
library(wesanderson)
library(cowplot)
library(scales)
library(Hmisc)

# Data from https://www.kaggle.com/sakshigoyal7/credit-card-customers
# Removed are the columns with unique customer IDs and the results from a
# naive bayes classifier. Columns have been renamed.

dir <- '~/mhoirup.github.io/images/'
data <- read.csv('~/Code/customer-churn/data.csv')[, 2:21]

names(data) <- c('churn', 'age', 'gender', 'n_dependents', 'education',
    'marriage', 'income', 'card_type', 'months_on_book', 'n_products',
    'months_inactive', 'n_contracts', 'credit_limit', 'revolving_balance',
    'average_open_to_buy', 'change_transaction_amount', 'transaction_amount',
    'n_transactions', 'change_n_transactions', 'average_utility_ratio'
    )

# The following variables are measured for the preceeding year:
# 'months_inactive', 'n_contracts', 'average_open_to_buy', 'transaction_amount'
# The following variables denote change from Q4 to Q1:
# 'change_transaction_amount', 'change_n_transactions'

red <- '#FD6467'
# With the ipsum theme you have to be explicit with each theme element apparently.
theme_set(hrbrthemes::theme_ipsum() + theme(
    axis.title.x = element_text(family = 'HK Grotesk', size = 12),
    axis.title.y = element_text(family = 'HK Grotesk', size = 12),
    axis.text.x = element_text(family = 'HK Grotesk', size = 12),
    axis.text.y = element_text(family = 'HK Grotesk', size = 12),
    plot.title = element_text(family = 'HK Grotesk', size = 14),
    plot.subtitle = element_text(family = 'HK Grotesk', size = 12),
    legend.text = element_text(family = 'HK Grotesk', size = 12),
    legend.title = element_text(family = 'HK Grotesk', size = 12),
    legend.position = 'top', legend.justification = 'right',
    plot.margin = margin(.1, .1, .1, .1, "cm"),
    ))
