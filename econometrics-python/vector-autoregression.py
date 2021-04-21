import numpy as np
# import pandas as pd
from scipy.stats import chi2

class Var_process:
    def __init__(self, series, order, intercept=True):
        if not isinstance(series, np.ndarray):
            series = np.array(series)
        self.intercept, self.order = intercept, order
        self.nrows, self.ncols = series.shape
        Z, X = series[order:], np.zeros(self.nrows - order, self.ncols * order)
        for t in range(order, self.nrows):
            xt = np.array([series[t-i] for i in range(1, order + 1)])
            X[t - order] += xt.reshape(1, self.ncols * order)
        if intercept: X = np.concatenate([np.ones(self.nrows - order), X])
        XX, XZ = np.linalg.inv(X.T.dot(X)), X.T.dot(Z)
        self.parameters = XX.dot(XZ)
        self.residuals = Z - X.dot(self.parameters)
        self.Cov = self.residuals.T.dot(self.residuals) / (self.nrows - order)
        self.ParamCov = np.kron(self.Cov, XX)
        self.standard_errors = np.diag(self.ParamCov) ** .5
        lndet = np.linalg.slogdet(self.Cov)[1]
        penalties = [
            (2 / self.nrows) * order * self.ncols ** 2,
            (np.log(self.nrows) / self.nrows) * order * self.ncols ** 2,
            (2 * np.log(np.log(self.nrows)) / self.nrows) * order * self.ncols ** 2
            ]
        info_crit = [lndet + penalty for penalty in penalties]
        self.aic, self.bic, self.hq = info_crit

    def check_stationarity(self, return_eigen=False):
        B = self.parameters[self.intercept:]
        Phi = np.hstack(B.reshape((self.p, self.k, self.k)))
        if self.order > 1:
            dim1 = self.ncols if self.order == 2 else self.ncols * (self.order - 1)
            I, O = np.identity(dim1), np.zeros([dim1, self.ncols])
            Phi = np.vstack([Phi, np.hstack([I, O])])
        eigen = np.abs(np.linalg.eig(Phi)[0])
        stationary, nonstationary = 'Process stationary', 'Process not stationary'
        print(stationary) if np.all(eigen < 1) else print(nonstationary)
        if return_eigen: return eigen

    def irf(self, orthogonalise=True, max_lag=12):
        Phi = self.parameters[self.intercept:]
        Phu = Phi.reshape([self.order, self.ncols, self.ncols])
        Psi = np.zeros(max_lag, self.ncols, self.ncols)
        Psi = np.concatenate([np.identity(self.ncols), Psi], 0)
        for i in range(1, max_lag + 1):
            m = np.min([i, self.order])
            Psi[i] += np.sum([Phi[j].dot(Psi[i - j]) for j in range(1, m + 1)])
        if orthogonalise:
            U = np.linalg.cholesky(self.Cov)
            for i in range(1, max_lag + 1):
                Psi[i] = Psi[i].dot(U.T)
        return Psi

    def autocorrelation(self, max_lag=24):
        R, A, C0 = list(), self.residuals, self.Cov
        D = np.linalg.inv(np.diag(np.diag(C0) ** .5))
        R.append(np.linalg.inv(D.dot(C0).dot(D)))
        correlations = np.zeros([max_lag + 1, 4])
        effective = self.nrows - self.order # Effective sample size
        for i in range(1, max_lag + 1):
            A1, A2 = A[i:(effective)], A[0:(effective - i)]
            Ci = A1.dot(A2) * (1 / effective)
            R.append(D.dot(Ci).dot(D))
            Qm, degr_free, pvalue = 0, (i - self.order) * self.ncols ** 2, 1
            for j in range(1, i + 1):
                traced = np.trace(R[j].T.dot(R[0]).dot(R[j]).dot(R[0]))
                Qm += traced* (1 / (self.nrows - j))
            Qm *= self.nrows ** 2
            if degr_free > self.order: pvalue -= chi2.cdf(Qm, degr_free)
            correlations[i] += np.array(i, Qm, degr_free, pvalue)
        return correlations
