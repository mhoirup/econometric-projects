import numpy as np
import pandas as pd
from scipy.optimize import minimize
from scipy.stats import norm
from pandas_datareader import data

data = data.DataReader('DEXUSEU', 'fred')
data

class olsregr:
    def __init__(self, endo, exo, intercept=True):
        if not map(lambda z: isinstance(z, np.ndarray), [endo, exo]):
            endo, exo = np.array(endo), np.array(exo)
        if exo.ndim == 1:
            endodm, exodm = endo - np.mean(endo), exo - np.mean(exo)
            beta = np.sum(endodm * exodm) / np.sum(exodm ** 2)
            self.fitted = exo * beta
            if intercept:
                beta = np.array([np.mean(exo) - (beta * np.mean(exo)), beta])
                self.fitted += beta[0]
            self.beta = beta
        else:
            if intercept: exo = np.insert(exo, 0, np.ones(exo.shape[0]), 1)
            Q, R = np.linalg.qr(exo)
            self.beta = np.linalg.inv(R).dot(Q.T).dot(endo)
            self.fitted = exo.dot(self.beta)
        self.resids = exo - self.fitted


def dshift(data, p):
    is_dataframe = False
    if isinstance(data, pd.DataFrame):
        data, is_dataframe, names = np.array(data), True, data.columns.tolist()
    if not isinstance(data, np.ndarray):
        data = np.array(data)
    nrows, nvars = data.shape if data.ndim == 2 else [data.size, 1]
    matrix = np.zeros([nrows - p, (nvars * p) + nvars])
    for t in range(p, nrows):
        if data.ndim == 1: xt = np.flip(data[(t-p):(t+1)])
        else: xt = np.array([data[t-i] for i in range(p+1)])
        matrix[t-p] = xt.reshape(1, matrix.shape[1])
    if is_dataframe:
        new = [name + '_L' + str(i) for i in range(1, p+1) for name in names]
        names.extend(new)
        matrix = pd.DataFrame(matrix, columns=names)
    return matrix


def autocorr(x, maxlag=30):
    N, xdm = x.size, x - np.mean(x)
    rho = np.array([np.sum(xdm[i:] * xdm[:(N-i)]) for i in range(1, maxlag+1)])
    return rho / np.sum(xdm ** 2)


class Arima_process:
    def __init__(self, series, order=[0,0,0], fit_mean=True, method='CSS-ML'):
        if not isinstance(series, np.ndarray): series = np.array(series)
        self.series, self.order, self.fit_mean = series, order, fit_mean
        nrows, dof = series.size, np.sum(order) + fit_mean
        if order[2] > 0:
            for _ in range(1, order[2] + 1):
                series = np.diff(series)
        if order[3] == 0:
            X = dshift(series, self.order[0])
            initparams = olsregr(X[:,0], X[:,1:], intercept=False)
        else:
            m = int(np.round(nrows/3)) + dof
            Rm, rho = np.zeros([m, m]), autocorr(series, m)
            for i in range(m):
                Ri = np.insert(rho[:(m-(i+1))], 0, 1)
                Rm[i] = np.concatenate([np.flip(rho[:i]), Ri])
            phi, obs = np.linalg.inv(Rm).dot(rho), range(m, nrows)
            z = [series[i] - np.sum(series[(i-m):i] * phi) for i in obs]
            Z = dshift(np.array(z), self.order[1])
            Zy, Zx = Z[:,0], Z[:,1:]
            if self.order[2] != 0:
                X = dshift(series[m:], self.order[0])[:,1:]
                Xnrows, Znrows = X.shape[0], Zx.shape[0]
                if Xnrows != Znrows:
                    delta = np.abs(Xnrows - Znrows)
                    if Xnrows > Znrows: X = X[delta:]
                    else: Zy, Zx = Zy[delta:], Zx[delta:]
                Zx = np.concatenate([X, Zx], 1)
            initparams = olsregr(Zy, Zx, intercept=False).beta
        if method in ['CSS', 'CSS-ML']:
            CSS = lambda params: np.sum(_resids_(params) ** 2)
            self.optim = minimize(CSS, initparams, tol=1e-3)
        if method in ['ML', 'CSS-ML']:
            initparams = initparams if method == 'ML' else self.optim.x
            neg_loglike = lambda params: -_loglike_(params)
            self.optim = minimize(neg_loglike, initparams, tol=1e-3)
        parameters = self.optim.x
        resids = _resids_(parameters)
        dx = np.diag(np.repeat(1e-4, dof - fit_mean))
        self.sigma = np.sum(resids ** 2) / resids.size
        X = np.array([_resids_(parameters + dxi) - resids for dxi in dx]).T / 1e-4
        if fit_mean: X = np.insert(X, 0, np.ones(X.shape[0]), 1)
        self.pcov = np.linalg.inv(X.T.dot(X)) * self.sigma
        self.parameters, self.residuals = parameters, resids
        if fit_mean:
            self.parameters = np.insert(self.parameters, 0, np.mean(series))
        self.standard_errors = np.diag(self.pcov) ** .5
        self.z_scores = self.parameters / self.standard_errors
        self.p_values = 2 * (1 - norm.cdf(np.abs(self.z_scores)))
        if method in ['ML', 'CSS-ML']: self.loglike = -self.optim.fun
        else: self.loglike = _loglike_(self.optim.x)
        self.aic = -2 * self.loglike + 2 * dof
        self.bic = self.aic + (np.log(nrows) - 2) * dof
        self.aicc = self.aic + ((2 * dof ** 2 + 2 * dof) / (nrows - dof))

    def _resids_(self, params):
        z = self.series - np.mean(self.series) if self.fit_mean else self.series
        p, q = self.order[0], self.order[2]
        zp = dshift(np.insert(z, 0, np.zeros(p)), p)
        zp = zp[:,0] - np.sum(zp[:,1:] * params[:p], 1)
        theta, res = params[p:][::-1], np.zeros(q)
        for i in range(zp.size):
            aq = np.sum(res[-q:] * theta) if q > 0 else 0
            res = np.append(res, zp[i] - aq)
        return res[q:]

    def _loglike_(self, params):
        N2, rss = self.series.size/2, np.sum(_resids_(params) ** 2)
        sigma, lnpi = rss / (N2 * 2), np.log(2 * np.pi)
        return -N2 * lnpi - N2 * np.log(sigma) - (rss / (2 * sigma))

