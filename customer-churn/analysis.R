source('~/Code/customer-churn/imports.R')

# Data prep. Convert 'churn', create 'reduced_expenditure' and 'reduced_use',
# rescale variates in dollar amount, remove 'average_open_to_buy', and reformat
# categorical values
mutate(data, churn = as.integer(churn == 'Attrited Customer'),
    reduced_expenditure = change_transaction_amount < 1,
    reduced_use = change_n_transactions < 1) %>%
    # mutate_at(vars(credit_limit, transaction_amount, revolving_balance),
    #     function(x) x / 100) %>%
    select(-contains('change'), -average_open_to_buy) %>%
    mutate(income = ifelse(income == 'Unknown', income,
        gsub('[^0-9.-]', '', income))) %>%
    mutate(income = case_when(
        income == '40' ~ 'under40',
        income == '120' ~ 'over120',
        T ~ income)) %>%
    mutate_if(is.character, function(x) gsub(' ', '-', tolower(x))) %>%
    mutate_if(is.character, function(x) paste0('.', x)) %>%
    mutate_if(is.character, as.factor) -> data

# Univarate analysis of association between the covariates and `churn`.
# All variates except for `card_type` were shown to be significant, so
# we continue with only 'card_type' removed.

regressors <- names(data)[names(data) != 'churn']
discrete <- sapply(regressors, function(v) {
    is.factor(data[, v]) | is.logical(data[, v])
    })

discrete <- names(discrete[discrete])
continuous <- regressors[!(regressors %in% discrete)]

# silence chi^2 approximation warning
suppressWarnings(lapply(discrete,
    function(v) chisq.test(data$churn, data[,v])[c(1, 3)]) %>%
    do.call(rbind.data.frame, .) %>%
    cbind(var = discrete) %>%
    ggplot(aes(p.value, var)) +
    geom_vline(xintercept = .2, colour = red, lty = 'dashed') +
    geom_point(aes(size = statistic), shape = 21, fill = 'white') +
    theme(legend.justification = 'right') +
    scale_size_continuous(range = c(6, 10)) +
    scale_x_continuous(limits = c(0, 1)) + labs(
        x = 'P-Value',
        y = '',
        size = 'Test Statistic',
        title = 'Pearson\'s Chi-Square Test on Discrete Covariates'
        ))

chisq.test(data$churn, data$card_type)
ggsave(paste0(dir, 'churn_pearson.png'))

lapply(continuous, function(v) t.test(data[, v] ~ data$churn)[c(1, 3)]) %>%
    do.call(rbind.data.frame, .) %>%
    cbind(var = continuous) %>%
    ggplot(aes(p.value, var)) +
    geom_vline(xintercept = .2, colour = red, lty = 'dashed') +
    geom_point(aes(size = statistic), shape = 21, fill = 'white') +
    theme(legend.justification = 'right') +
    scale_size_continuous(range = c(2, 10)) +
    scale_x_continuous(limits = c(0, 1)) + labs(
        x = 'P-Value',
        y = '',
        size = 'Test Statistic',
        title = 'Student\'s t-Test on Continuous Covariates',
        subtitle = paste('Variance assumption dropped, so technically',
        'Welch\'s t-Test.')
        )

ggsave(paste0(dir, 'churn_student.png'))

# Multivariate analysis where we iteratively peel off one insignificant
# variate at a time. Over three iterations, we remove 'education' and
# 'average_utility_ratio'. Other covariates are also found to be insignificant,
# but are shown to have an ajusting effect on other covariates.

removed <- 'card_type'
regressors <- regressors[!(regressors %in% removed)]

regr <- function(regressors, interactions = NULL) {
    # To create regressions a little easier
    regressors <- paste(regressors, collapse = '+')
    if (!is.null(interactions)) {
        interactions <- paste(paste(interactions[, 1], interactions[, 2],
            sep = ':'), collapse = '+')
        regressors <- paste(regressors, '+', interactions)
    }
    form <- as.formula(paste('churn ~ ', regressors))
    do.call('glm', list(form, as.name('data'), family = binomial))
}

selection <- data.frame(); i <- 1; trimmed <- T
while (trimmed) {
    prior <- length(regressors); seen <- c()
    unrestricted <- regr(regressors)
    output <- coeftest(unrestricted)
    for (v in names(which(sort(output[,4], decreasing = T) > .05))) {
        # To find the categorical variable of an insignificant dummy
        if (!(v %in% regressors)) {
            v <- regressors[which.max(nchar(str_match(v, regressors)))]
        }
        # This conditional is just to make sure that categoricals aren't
        # repeatedly tested for, in the case of multiple insignificant dummies
        if (!any(v == seen)) {
            seen <- append(seen, v)
            restricted <- regr(regressors[which(regressors != v)])
            likelihood_ratio <- lrtest(unrestricted, restricted)[2, 4:5]
            # Making sure that the removal of a variable does not change the
            # magnitude of remaining coefficients by more than 20%
            new_params <- restricted$coefficients
            params <- output[names(new_params), 1]
            differences <- abs(((new_params - params) / params)) * 100
            dmax <- c(names(which.max(differences)), max(differences))
            candidate <- data.frame(
                iter = i, var = v, statistic = likelihood_ratio[[1]],
                pvalue = likelihood_ratio[[2]], dmax_var = dmax[1],
                dmax_perc = as.double(dmax[2])
                )
            selection <- rbind(selection, candidate)
            if (candidate$pvalue > .05 & candidate$dmax_perc < 20) {
                regressors <- regressors[regressors != candidate$var]
                break
            }
        }
    }
    trimmed <- prior > length(regressors); i <- i + 1
}

mutate_if(selection, is.numeric, round, 3) %>%
    mutate_at(vars(var, dmax_var), fcode) %>%
    knitr::kable()

results <- coeftest(unrestricted)
results[which(rownames(results) %in% c('age', 'months_on_book')), ] %>%
    knitr::kable()

regressors <- regressors[regressors != 'months_on_book']
regression <- regr(regressors)
lrtest(regr(c(regressors, 'card_type')), regression)
regressors <- append(regressors, 'card_type')
regression <- regr(regressors)

# Variance inflation factors - income exceeds 5, but not by much, so we'll
# keep it in our specification

data.frame(car::vif(regression)) %>%
    setNames(c('gvif', 'df', 'vif')) %>%
    mutate(var = regressors, vif = vif^2) %>%
    select(-df) %>%
    gather(key, value, -var) %>%
    ggplot(aes(value, var, fill = key, colour = key)) +
    geom_point(shape = 21, size = 6) +
    guides(colour = F) +
    scale_fill_manual(labels = c('Generalised VIF', 'VIF'),
        values = c('#000000', '#ffffff')) +
    scale_colour_manual(values = c('#ffffff', '#000000')) +
    scale_x_continuous(limits = c(NA, NA), labels = number) +
    labs(x = '', y = '', title = 'Variance Inflation Factors',
        fill = ''
        )

ggsave(paste0(dir, 'churn_vif.png'))

# Check linearity of continuous predictors in the log-odds. For each numeric
# variable we draw a graph of the variable versus the log-odds of the current
# model. A local regression line is fitted to help infer linearity

continuous <- regressors[regressors %in% continuous]
logit <- log(regression$fitted.values / (1 - regression$fitted.values))

i <- 0; for (pair in split(continuous, ceiling(seq_along(continuous) / 3))) {
    # Suppress warnings from the loess functions
    suppressWarnings(select(data, pair, churn) %>%
        mutate(logit = logit, churn = ifelse(churn == 1, 'Attrited Customer',
            'Existing Customer')) %>%
        gather(key, value, -logit, -churn) %>%
        ggplot(aes(value, logit)) +
        geom_point(aes(fill = churn, colour = churn), size = 4, shape = 21) +
        geom_smooth(method = 'loess', colour = red, se = F, size = 2) +
        scale_fill_manual(values = c('#000000', '#ffffff')) +
        scale_colour_manual(values = c('#ffffff', '#000000'), guide = 'none') +
        facet_wrap(~ key, scales = 'free_x') +
        scale_y_continuous(labels = number) +
        labs(x = 'Values', y = 'Log-Odds', fill = '',
             title = ifelse(i == 0, paste('Linearity in the Log-Odds'), '')) +
        theme(strip.text = element_text(size = 20),
            legend.position = ifelse(i == 0, 'top', 'none')) +
        guides(fill = guide_legend(override.aes = list(size =  10)))
        )
    ggsave(paste0(dir, 'churn_predictor_scale', i, '.png'))
    i <- i + 1
}

rescale <- c(
    'age', 'credit_limit', 'months_inactive', 'n_transactions',
    'transaction_amount'
    )

power_set <- c(-2, -1, -.5, 0, .5, 1, 2, 3)
pcombi <- unique(t(apply(expand.grid(power_set, power_set), 1, sort)))
f1 <- function(x, p) if (p != 0) (x + 1e-9)^p else log(x + 1e-3)
f2 <- function(x, p1, p2) if (p1 == p2) f1(x, p1) * log(x + 1e-3) else f1(x, p2)
Lmain <- as.double(logLik(regression)); fracp <- data.frame()
deviances <- data.frame()

for (v in rescale) {
    deviance1 <- 1e+9; deviance2 <- 1e+9; x <- data[, v]
    for (p in power_set) {
        pregression <- suppressWarnings(select(data, churn, regressors) %>%
            select(-v) %>%
            mutate(pterm = f1(data[, v], p)) %>%
            glm(churn ~ ., ., family = binomial)
            )
        deviance <- pregression$deviance
        deviances <- rbind(deviances, data.frame(
            var = v, power = p, deviance = deviance
            ))
        if (deviance < deviance1) {
            p1regression <- pregression; power1 <- p; deviance1 <- deviance
        }
    }
    for (i in 1:nrow(pcombi)) {
        p1 <- pcombi[i,1]; p2 <- pcombi[i,2]
        pregression <- suppressWarnings(select(data, churn, regressors) %>%
            select(-v) %>%
            mutate(pterm1 = f1(data[, v], p1),
                pterm2 = f2(data[, v], p1, p2)) %>%
            glm(churn ~ ., ., family = binomial)
            )
        powers <- paste(p1, p2); deviance <- pregression$deviance
        deviances <- rbind(deviances, data.frame(
            var = v, power = powers, deviance = deviance
            ))
        if (deviance < deviance2) {
            p2regression <- pregression; power2 <- powers; deviance2 <- deviance
        }
    }
    L <- as.double(sapply(list(p1regression, p2regression), logLik))
    statistics <- -2 * c(L[1] - L[2], Lmain - L[2])
    pvs <- sapply(1:2, function(i) pchisq(statistics[i], i + 1, lower.tail = F))
    fracp <- rbind(fracp, data.frame(
        var = v, power1, power2, deviance1, deviance2, H2v0 = statistics[2],
        H2v0p = pvs[2], H2v1 = statistics[1], H2v1p = pvs[1]
        ))
}

ggplot(deviances, aes(var, deviance)) +
    geom_hline(yintercept = regression$deviance, lty = 'dashed', colour = red) +
    geom_jitter(width = .3, size = 4, colour = 'white',
        shape = 21, fill = 'black') +
    scale_y_continuous(labels = number) +
    labs(x = '', y = 'Deviance',
        title = 'Deviance of Fitted Fractional Polynomial Models',
        subtitle = 'Dashed line at deviance of main effects model')

fnapprox <- function(var, powers, knots = NULL) {
    logits <- data.frame(var = data[,var])
    for (set in powers) {
        fpdata <- data[, c('churn', regressors)]
        fpdata <- mutate(fpdata, Term1 = f1(data[,var], set[1]))
        if (length(set) == 2) {
            fpdata <- mutate(fpdata, Term2 = f2(data[,var], set[1], set[2]))
        }
        fpregr <- suppressWarnings(
            glm(churn ~ ., select(fpdata, -var), family = binomial)
            )
        fplogit <- log(fpregr$fitted.values / (1 - fpregr$fitted.values))
        logits <- cbind(logits, fplogit)
    }
    splines <- data.frame(rcspline.eval(data[, var]))
    sregr <- suppressWarnings(
        setNames(splines, paste0('spline', seq_along(splines))) %>%
        cbind(select(data, churn, regressors[regressors != var]), .) %>%
        glm(churn ~ ., ., family = binomial)
        )
    splinelogit <- log(sregr$fitted.values / (1 - sregr$fitted.values))
    logits <- cbind(logits, splinelogit)
    setNames(logits, c('var', names(powers), 'spline'))
}

mutate_at(fracp, vars(contains('H')), formatC, 3, format = 'f')

powers <- list(
    standard = 1, best1 = 0, chosen1 = 2, best2 = c(-2, -2),
    chosen2 = c(0, 2)
)

fnapprox('transaction_amount', powers) %>%
    gather(key, value, -var) %>%
    mutate(key = factor(key, levels = c('standard', 'best1', 'chosen1',
        'best2', 'chosen2', 'spline'))) %>%
    mutate(key = recode(key, standard = 'Standard', best1 = 'Best 1FP',
        chosen1 = 'Chosen 1FP', best2 = 'Best 2FP', chosen2 = 'Chosen 2FP',
        spline = 'Restricted cubic spline (4 knots)')) %>%
    ggplot(aes(var, value)) +
    geom_point(size = 1, alpha = .5) +
    geom_smooth(method = 'loess', colour = 'royalblue2', se = F) +
    facet_wrap(~ key, scales = 'free_y') +
    # scale_x_continuous(breaks = c(40, 80, 120)) +
    scale_y_continuous(limits = c(NA, NA), labels = number,
        breaks = pretty_breaks(4)) +
    labs(x = 'n_transactions', y = 'Logarithmic Odds')
