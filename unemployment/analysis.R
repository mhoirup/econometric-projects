source('~/Code/unemployment/imports.R')

specification <- function(response, regressors, lhs = NULL, rhs = NULL,
    polynomials = NULL, intercept = TRUE) {
    # This function is to ease the tinkering with variables in the model
    # estimation procedure. Since a string can be converted into an R
    # formula, manipulating the formula can be boiled down to string
    # manipulation.
    # Initiate the formula by concatenating the regressors with '+' and
    # 'response ~'.
    spec <- paste(response, '~', paste(regressors, collapse = '+'))
    # Create any interaction terms where terms are specified by lhs and rhs.
    if (!is.null(lhs) && !is.null(rhs)) {
        M <- expand.grid(lhs, rhs)
        idx <- sapply(seq_len(nrow(M)), function(i) M[i, 1] != M[i, 2])
        spec <- paste(spec, '+', paste(M[idx, 1], M[idx, 2], sep = ':',
            collapse = '+'))
    }
    # Add to the formula any polynomial terms of order 2.
    if (!is.null(polynomials)) {
        spec <- paste(spec, paste(paste0(polynomials, '^2'), collapse = '+'))
    }
    # Optional: remove intercept.
    if (intercept == FALSE) spec <- paste(spec, '-1')
    as.formula(spec)
}

spec <- function(x) specification('Surv(spell, censor4 == 0)', x)
regressors <- names(data)[!grepl('censor|spell', names(data))]
progress <- data.frame() # Bad practice to fill out a dataframe without
# specifying its size first, but we don't know how many variables
# are insignificant.

for (dist in c('weibull', 'exponential', 'lognormal', 'loglogistic', 'coxph')) {
    covariates <- regressors; reduced <- TRUE
    while (reduced) {
        # The 'coxph' isn't really a distribution, but here we simply specify
        # the Cox model instead if dist == 'coxph'.
        if (dist == 'coxph') unrestricted <- coxph(spec(covariates), data)
        else unrestricted <- survreg(spec(covariates), data, dist = dist)
        # Get the p-values and sort them highest to lowest. Idx gives the
        # indices in covariates corresponding to the sorted list of p-values.
        # Also keeps track of the current numbers of rows in progress.
        cfs <- coeftest(unrestricted); rows <- nrow(progress)
        pvalues <- cfs[!(rownames(cfs) %in% c('(Intercept)', 'Log(scale)')), 4]
        idx <- sort(pvalues, index.return = TRUE, decreasing = TRUE)$ix
        for (var in covariates[idx]) {
            # Estimate the model without the candiate variate and compute the
            # p-value for the likelihood ratio test.
            subset <- covariates[covariates != var]
            if (dist == 'coxph') restricted <- coxph(spec(subset), data)
            else restricted <- survreg(spec(subset), data, dist = dist)
            lr_pvalue <- lmtest::lrtest(unrestricted, restricted)[[5]][2]
            # Here we check for the percentage change in the remaining
            # coefficients, with an acceptable limit being at 15% in either
            # direction.
            new_params <- restricted$coefficients[-1]
            params <- unrestricted$coefficients[names(new_params)]
            differences <- abs(((new_params - params) / params)) * 100
            dmax <- c(names(which.max(differences)), max(differences))
            if (lr_pvalue > 0.05 && as.double(dmax[2]) < 15) {
                covariates <- subset
                progress <- rbind(progress, data.frame(
                    dist, var, lr_pvalue, delta_var = dmax[1], delta_p = dmax[2]
                ))
                break
            }
        }
        if (rows == nrow(progress)) break
    }
    # Finally we print a message which tells us (1) how many variables
    # are removed, (2) which were removed, and (3) the log-likelihood
    # of the final specification.
    nremoved <- length(regressors) - length(covariates)
    which_removed <- regressors[!(regressors %in% covariates)]
    message <- paste(dist, 'distribution: removed', nremoved, 'variable(s)',
        paste0('(', paste(which_removed, collapse = ', '), ').'),
        '\n negative log-likelihood of final model:',
        round(logLik(unrestricted), 2), '\n\n')
    cat(message)
}

regressors <- regressors[regressors != 'tenure']
