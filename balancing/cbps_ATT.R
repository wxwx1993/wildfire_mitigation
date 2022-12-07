# Covariate Balancing Propensity Score
# This simple script uses Base R's `optim` to solve a variant of (7.10) from
# http://web.stanford.edu/~swager/stats361.pdf to target ATT weights.
# (see https://arxiv.org/abs/1601.05890 for details)
#
# Input:
#   X: nXp numeric covariate matrix
#   W: binary treatment assignment vector
#   intercept: whether to include an intercept in logistic model, default is TRUE.
#   theta.init: optional starting values for theta.
#   method: method argument passed to `optim`.
#   control: control argument passed to `optim`.
#   lambda: optional ridge penalty (remember to scale X's appropriately if used)
# Output:
#   theta.hat: estimated thetas
#   weights.0: IPW weights for control
#   weights.1: IPW weights for treated
#   convergence: optim's convergence status. 0=success.
#   balance condition: the LHS and RHS of the balance condition.
cbps_att = function(X, W, intercept = TRUE, theta.init = NULL, method = "BFGS", control = list(), lambda = NULL) {
  if (!all(W %in% c(0, 1))) {
    stop("W should be a binary vector.")
  }
  if (!is.numeric(X) || nrow(X) != length(W) || is.null(dim(X)) || anyNA(X)) {
    stop("X should be a numeric matrix with nrows = length(W).")
  }

  .Xtheta = NULL
  # ATT balance constraint is:
  # 1/n1 \sum_{Wi = 0} e(x)/(1-e(x)) Xi = 1/n1 \sum_{Wi=1} Xi,
  # which gives loss function
  # (1 - W)exp(theta * X) - W * theta * X
  .objective = function(theta, X, W0.idx, W1.idx, lambda) {
    .Xtheta <<- X %*% theta
    (sum(exp(.Xtheta[W0.idx, ])) - sum(.Xtheta[W1.idx, ])) / length(W1.idx) + sum(lambda * theta^2)
  }

  .objective.gradient = function(theta, X0, Xsum1, W0.idx, n, lambda) {
    (colSums(X0 * exp(.Xtheta[W0.idx, ])) - Xsum1) / n + 2 * lambda * theta
  }
  if (is.null(lambda)) {
    lambda = rep(0, ncol(X))
  }
  if (intercept) {
    X = cbind(1, X)
    lambda = c(0, lambda)
  }

  W1.idx = which(W == 1)
  W0.idx = which(W == 0)
  if (is.null(theta.init)) {
    # Use "naive" logistic starting values
    idx.small = c(W1.idx, sample(W0.idx, length(W1.idx)))
    glm = glm.fit(X[idx.small, ], W[idx.small], family = binomial())
    theta.init = glm$coefficients
    # update the intercept, (7) in https://gking.harvard.edu/files/0s.pdf
    if (intercept) {
      pi = mean(W)
      theta.init[1] = theta.init[1] - log((1 - pi) / pi) * length(idx.small) / sum(W)
    }
  }

  X0 = X[W0.idx, ]
  Xsum1 = colSums(X[W1.idx, ])
  res = optim(
    par = theta.init,
    fn = function(x) .objective(x, X, W0.idx, W1.idx, lambda),
    gr = function(x) .objective.gradient(x, X0, Xsum1, W0.idx, nrow(X), lambda),
    method = method,
    lower = -Inf,
    upper = Inf,
    control = control,
    hessian = FALSE
  )

  theta.hat = res$par
  weights.0 = exp(X %*% theta.hat)[,]
  LHS = colSums((1 - W) * X * weights.0) / sum(W == 1)
  RHS = colSums(W * X) / sum(W==1)

  sd.W1 = apply(X[W1.idx, ], 2, sd)
  sd.W1[sd.W1 == 0] = 1
  sd.W = apply(X, 2, sd)
  sd.W[sd.W == 0] = 1
  mean.diff = colMeans(X[W1.idx, ]) -
    apply(X[W0.idx, ], 2, function(x) weighted.mean(x, weights.0[W0.idx]))
  balance.std = mean.diff / sd.W1
  balance.std.pre = (colMeans(X[W1.idx, ]) - colMeans(X[W0.idx, ])) / sd.W1
  balance.std.all = mean.diff / sd.W
  balance.std.pre.all = (colMeans(X[W1.idx, ]) - colMeans(X[W0.idx, ])) / sd.W


  list(
    theta.hat = theta.hat,
    weights.0 = weights.0,
    weights.1 = rep(1, nrow(X)),
    convergence = res$convergence,
    balance.condition = cbind(LHS = LHS, RHS = RHS),
    balance.std = if (intercept) balance.std[-1] else balance.std,
    balance.std.pre = if (intercept) balance.std.pre[-1] else balance.std.pre,
    balance.std.all = if (intercept) balance.std.all[-1] else balance.std.all,
    balance.std.pre.all = if (intercept) balance.std.pre.all[-1] else balance.std.pre.all
  )
}


if (FALSE) {
  n = 15000
  p = 40
  X = matrix(rnorm(n*p), n, p)
  W = rbinom(n, 1, 1 / (1 + exp(2.5 - X[, 1])))

  system.time(res <- cbps_att(X, W))

  head(res$balance.condition)
  plot(res$balance.std)
  abline(h = 0)
}
