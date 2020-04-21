Recycle <- function (...) 
{
  lst <- list(...)
  maxdim <- max(lengths(lst))
  res <- lapply(lst, rep, length.out = maxdim)
  attr(res, "maxdim") <- maxdim
  return(res)
}

BinomCI <- function (x, n, conf.level = 0.95, sides = c("two.sided", 
                                             "left", "right"), method = c("wilson", 
                                                                          "wald", "agresti-coull", "jeffreys", "modified wilson", 
                                                                          "wilsoncc", "modified jeffreys", "clopper-pearson", 
                                                                          "arcsine", "logit", "witting", "pratt"), 
          rand = 123) 
{
  if (missing(method)) 
    method <- "wilson"
  if (missing(sides)) 
    sides <- "two.sided"
  iBinomCI <- function(x, n, conf.level = 0.95, sides = c("two.sided", 
                                                          "left", "right"), method = c("wilson", 
                                                                                       "wilsoncc", "wald", "agresti-coull", 
                                                                                       "jeffreys", "modified wilson", "modified jeffreys", 
                                                                                       "clopper-pearson", "arcsine", "logit", 
                                                                                       "witting", "pratt"), rand = 123) {
    if (length(x) != 1) 
      stop("'x' has to be of length 1 (number of successes)")
    if (length(n) != 1) 
      stop("'n' has to be of length 1 (number of trials)")
    if (length(conf.level) != 1) 
      stop("'conf.level' has to be of length 1 (confidence level)")
    if (conf.level < 0.5 | conf.level > 1) 
      stop("'conf.level' has to be in [0.5, 1]")
    sides <- match.arg(sides, choices = c("two.sided", 
                                          "left", "right"), several.ok = FALSE)
    if (sides != "two.sided") 
      conf.level <- 1 - 2 * (1 - conf.level)
    alpha <- 1 - conf.level
    kappa <- qnorm(1 - alpha/2)
    p.hat <- x/n
    q.hat <- 1 - p.hat
    switch(match.arg(arg = method, choices = c("wilson", 
                                               "wald", "wilsoncc", "agresti-coull", 
                                               "jeffreys", "modified wilson", "modified jeffreys", 
                                               "clopper-pearson", "arcsine", "logit", 
                                               "witting", "pratt")), wald = {
                                                 est <- p.hat
                                                 term2 <- kappa * sqrt(p.hat * q.hat)/sqrt(n)
                                                 CI.lower <- max(0, p.hat - term2)
                                                 CI.upper <- min(1, p.hat + term2)
                                               }, wilson = {
                                                 est <- p.hat
                                                 term1 <- (x + kappa^2/2)/(n + kappa^2)
                                                 term2 <- kappa * sqrt(n)/(n + kappa^2) * sqrt(p.hat * 
                                                                                                 q.hat + kappa^2/(4 * n))
                                                 CI.lower <- max(0, term1 - term2)
                                                 CI.upper <- min(1, term1 + term2)
                                               }, wilsoncc = {
                                                 est <- p.hat
                                                 lci <- (2 * x + kappa^2 - 1 - kappa * sqrt(kappa^2 - 
                                                                                              2 - 1/n + 4 * p.hat * (n * q.hat + 1)))/(2 * 
                                                                                                                                         (n + kappa^2))
                                                 uci <- (2 * x + kappa^2 + 1 + kappa * sqrt(kappa^2 + 
                                                                                              2 - 1/n + 4 * p.hat * (n * q.hat - 1)))/(2 * 
                                                                                                                                         (n + kappa^2))
                                                 CI.lower <- max(0, lci)
                                                 CI.upper <- min(1, uci)
                                               }, `agresti-coull` = {
                                                 x.tilde <- x + kappa^2/2
                                                 n.tilde <- n + kappa^2
                                                 p.tilde <- x.tilde/n.tilde
                                                 q.tilde <- 1 - p.tilde
                                                 est <- p.tilde
                                                 term2 <- kappa * sqrt(p.tilde * q.tilde)/sqrt(n.tilde)
                                                 CI.lower <- max(0, p.tilde - term2)
                                                 CI.upper <- min(1, p.tilde + term2)
                                               }, jeffreys = {
                                                 est <- p.hat
                                                 if (x == 0) CI.lower <- 0 else CI.lower <- qbeta(alpha/2, 
                                                                                                  x + 0.5, n - x + 0.5)
                                                 if (x == n) CI.upper <- 1 else CI.upper <- qbeta(1 - 
                                                                                                    alpha/2, x + 0.5, n - x + 0.5)
                                               }, `modified wilson` = {
                                                 est <- p.hat
                                                 term1 <- (x + kappa^2/2)/(n + kappa^2)
                                                 term2 <- kappa * sqrt(n)/(n + kappa^2) * sqrt(p.hat * 
                                                                                                 q.hat + kappa^2/(4 * n))
                                                 if ((n <= 50 & x %in% c(1, 2)) | (n >= 51 & x %in% 
                                                                                   c(1:3))) CI.lower <- 0.5 * qchisq(alpha, 2 * 
                                                                                                                       x)/n else CI.lower <- max(0, term1 - term2)
                                                 if ((n <= 50 & x %in% c(n - 1, n - 2)) | (n >= 51 & 
                                                                                           x %in% c(n - (1:3)))) CI.upper <- 1 - 0.5 * qchisq(alpha, 
                                                                                                                                              2 * (n - x))/n else CI.upper <- min(1, term1 + 
                                                                                                                                                                                    term2)
                                               }, `modified jeffreys` = {
                                                 est <- p.hat
                                                 if (x == n) CI.lower <- (alpha/2)^(1/n) else {
                                                   if (x <= 1) CI.lower <- 0 else CI.lower <- qbeta(alpha/2, 
                                                                                                    x + 0.5, n - x + 0.5)
                                                 }
                                                 if (x == 0) CI.upper <- 1 - (alpha/2)^(1/n) else {
                                                   if (x >= n - 1) CI.upper <- 1 else CI.upper <- qbeta(1 - 
                                                                                                          alpha/2, x + 0.5, n - x + 0.5)
                                                 }
                                               }, `clopper-pearson` = {
                                                 est <- p.hat
                                                 CI.lower <- qbeta(alpha/2, x, n - x + 1)
                                                 CI.upper <- qbeta(1 - alpha/2, x + 1, n - x)
                                               }, arcsine = {
                                                 p.tilde <- (x + 0.375)/(n + 0.75)
                                                 est <- p.tilde
                                                 CI.lower <- sin(asin(sqrt(p.tilde)) - 0.5 * kappa/sqrt(n))^2
                                                 CI.upper <- sin(asin(sqrt(p.tilde)) + 0.5 * kappa/sqrt(n))^2
                                               }, logit = {
                                                 est <- p.hat
                                                 lambda.hat <- log(x/(n - x))
                                                 V.hat <- n/(x * (n - x))
                                                 lambda.lower <- lambda.hat - kappa * sqrt(V.hat)
                                                 lambda.upper <- lambda.hat + kappa * sqrt(V.hat)
                                                 CI.lower <- exp(lambda.lower)/(1 + exp(lambda.lower))
                                                 CI.upper <- exp(lambda.upper)/(1 + exp(lambda.upper))
                                               }, witting = {
                                                 set.seed(rand)
                                                 x.tilde <- x + runif(1, min = 0, max = 1)
                                                 pbinom.abscont <- function(q, size, prob) {
                                                   v <- trunc(q)
                                                   term1 <- pbinom(v - 1, size = size, prob = prob)
                                                   term2 <- (q - v) * dbinom(v, size = size, prob = prob)
                                                   return(term1 + term2)
                                                 }
                                                 qbinom.abscont <- function(p, size, x) {
                                                   fun <- function(prob, size, x, p) {
                                                     pbinom.abscont(x, size, prob) - p
                                                   }
                                                   uniroot(fun, interval = c(0, 1), size = size, 
                                                           x = x, p = p)$root
                                                 }
                                                 est <- p.hat
                                                 CI.lower <- qbinom.abscont(1 - alpha, size = n, x = x.tilde)
                                                 CI.upper <- qbinom.abscont(alpha, size = n, x = x.tilde)
                                               }, pratt = {
                                                 est <- p.hat
                                                 if (x == 0) {
                                                   CI.lower <- 0
                                                   CI.upper <- 1 - alpha^(1/n)
                                                 } else if (x == 1) {
                                                   CI.lower <- 1 - (1 - alpha/2)^(1/n)
                                                   CI.upper <- 1 - (alpha/2)^(1/n)
                                                 } else if (x == (n - 1)) {
                                                   CI.lower <- (alpha/2)^(1/n)
                                                   CI.upper <- (1 - alpha/2)^(1/n)
                                                 } else if (x == n) {
                                                   CI.lower <- alpha^(1/n)
                                                   CI.upper <- 1
                                                 } else {
                                                   z <- qnorm(1 - alpha/2)
                                                   A <- ((x + 1)/(n - x))^2
                                                   B <- 81 * (x + 1) * (n - x) - 9 * n - 8
                                                   C <- (0 - 3) * z * sqrt(9 * (x + 1) * (n - x) * 
                                                                             (9 * n + 5 - z^2) + n + 1)
                                                   D <- 81 * (x + 1)^2 - 9 * (x + 1) * (2 + z^2) + 
                                                     1
                                                   E <- 1 + A * ((B + C)/D)^3
                                                   CI.upper <- 1/E
                                                   A <- (x/(n - x - 1))^2
                                                   B <- 81 * x * (n - x - 1) - 9 * n - 8
                                                   C <- 3 * z * sqrt(9 * x * (n - x - 1) * (9 * 
                                                                                              n + 5 - z^2) + n + 1)
                                                   D <- 81 * x^2 - 9 * x * (2 + z^2) + 1
                                                   E <- 1 + A * ((B + C)/D)^3
                                                   CI.lower <- 1/E
                                                 }
                                               })
    ci <- c(est = est, lwr.ci = max(0, CI.lower), upr.ci = min(1, 
                                                               CI.upper))
    if (sides == "left") 
      ci[3] <- 1
    else if (sides == "right") 
      ci[2] <- 0
    return(ci)
  }
  lst <- list(x = x, n = n, conf.level = conf.level, sides = sides, 
              method = method, rand = rand)
  maxdim <- max(unlist(lapply(lst, length)))
  lgp <- lapply(lst, rep, length.out = maxdim)
  lgn <- Recycle(x = if (is.null(names(x))) 
    paste("x", seq_along(x), sep = ".")
    else names(x), n = if (is.null(names(n))) 
      paste("n", seq_along(n), sep = ".")
    else names(n), conf.level = conf.level, sides = sides, method = method)
  xn <- apply(as.data.frame(lgn[sapply(lgn, function(x) length(unique(x)) != 
                                         1)]), 1, paste, collapse = ":")
  res <- t(sapply(1:maxdim, function(i) iBinomCI(x = lgp$x[i], 
                                                 n = lgp$n[i], conf.level = lgp$conf.level[i], sides = lgp$sides[i], 
                                                 method = lgp$method[i], rand = lgp$rand[i])))
  colnames(res)[1] <- c("est")
  rownames(res) <- xn
  return(res)
}

BinomDiffCI <- function (x1, n1, x2, n2, conf.level = 0.95, sides = c("two.sided", 
                                                       "left", "right"), method = c("wald", "waldcc", 
                                                                                    "ac", "score", "scorecc", "mn", "mee", 
                                                                                    "blj", "ha", "beal")) 
{
  if (missing(method)) 
    method <- "ac"
  if (missing(sides)) 
    sides <- "two.sided"
  iBinomDiffCI <- function(x1, n1, x2, n2, conf.level = 0.95, 
                           sides = c("two.sided", "left", "right"), 
                           method = c("wald", "waldcc", "ac", 
                                      "score", "scorecc", "mn", "mee", 
                                      "blj", "ha", "beal")) {
    method <- match.arg(arg = method, choices = c("wald", 
                                                  "waldcc", "ac", "score", "scorecc", 
                                                  "mn", "mee", "blj", "ha"))
    sides <- match.arg(sides, choices = c("two.sided", 
                                          "left", "right"), several.ok = FALSE)
    if (sides != "two.sided") 
      conf.level <- 1 - 2 * (1 - conf.level)
    alpha <- 1 - conf.level
    kappa <- qnorm(1 - alpha/2)
    p1.hat <- x1/n1
    p2.hat <- x2/n2
    est <- p1.hat - p2.hat
    switch(method, wald = {
      vd <- p1.hat * (1 - p1.hat)/n1 + p2.hat * (1 - p2.hat)/n2
      term2 <- kappa * sqrt(vd)
      CI.lower <- max(-1, est - term2)
      CI.upper <- min(1, est + term2)
    }, waldcc = {
      vd <- p1.hat * (1 - p1.hat)/n1 + p2.hat * (1 - p2.hat)/n2
      term2 <- kappa * sqrt(vd)
      term2 <- term2 + 0.5 * (1/n1 + 1/n2)
      CI.lower <- max(-1, est - term2)
      CI.upper <- min(1, est + term2)
    }, ac = {
      n1 <- n1 + 2
      n2 <- n2 + 2
      x1 <- x1 + 1
      x2 <- x2 + 1
      p1.hat <- x1/n1
      p2.hat <- x2/n2
      est1 <- p1.hat - p2.hat
      vd <- p1.hat * (1 - p1.hat)/n1 + p2.hat * (1 - p2.hat)/n2
      term2 <- kappa * sqrt(vd)
      CI.lower <- max(-1, est1 - term2)
      CI.upper <- min(1, est1 + term2)
    }, exact = {
      CI.lower <- NA
      CI.upper <- NA
    }, score = {
      w1 <- BinomCI(x = x1, n = n1, conf.level = conf.level, 
                    method = "wilson")
      w2 <- BinomCI(x = x2, n = n2, conf.level = conf.level, 
                    method = "wilson")
      l1 <- w1[2]
      u1 <- w1[3]
      l2 <- w2[2]
      u2 <- w2[3]
      CI.lower <- max(-1, est + kappa * sqrt(l1 * (1 - 
                                                     l1)/n1 + u2 * (1 - u2)/n2))
      CI.upper <- min(1, est - kappa * sqrt(u1 * (1 - u1)/n1 + 
                                              l2 * (1 - l2)/n2))
    }, scorecc = {
      w1 <- BinomCI(x = x1, n = n1, conf.level = conf.level, 
                    method = "wilsoncc")
      w2 <- BinomCI(x = x2, n = n2, conf.level = conf.level, 
                    method = "wilsoncc")
      l1 <- w1[2]
      u1 <- w1[3]
      l2 <- w2[2]
      u2 <- w2[3]
      CI.lower <- max(-1, est + sqrt((p1.hat - l1)^2 + 
                                       (u2 - p2.hat)^2))
      CI.upper <- min(1, est - sqrt((u1 - p1.hat)^2 + (p2.hat - 
                                                         l2)^2))
    }, mee = {
      .score <- function(p1, n1, p2, n2, dif) {
        diff <- p1 - p2 - dif
        if (abs(diff) == 0) {
          res <- 0
        } else {
          t <- n2/n1
          a <- 1 + t
          b <- -(1 + t + p1 + t * p2 + dif * (t + 2))
          c <- dif * dif + dif * (2 * p1 + t + 1) + p1 + 
            t * p2
          d <- -p1 * dif * (1 + dif)
          v <- (b/a/3)^3 - b * c/(6 * a * a) + d/a/2
          s <- sqrt((b/a/3)^2 - c/a/3)
          u <- ifelse(v > 0, 1, -1) * s
          w <- (3.141592654 + acos(v/u^3))/3
          p1d <- 2 * u * cos(w) - b/a/3
          p2d <- p1d - dif
          n <- n1 + n2
          res <- (p1d * (1 - p1d)/n1 + p2d * (1 - p2d)/n2)
        }
        return(sqrt(res))
      }
      pval <- function(delta) {
        z <- (est - delta)/.score(p1.hat, n1, p2.hat, 
                                  n2, delta)
        2 * min(pnorm(z), 1 - pnorm(z))
      }
      CI.lower <- uniroot(function(delta) pval(delta) - 
                            alpha, interval = c(-1 + 1e-06, est - 1e-06))$root
      CI.upper <- uniroot(function(delta) pval(delta) - 
                            alpha, interval = c(est + 1e-06, 1 - 1e-06))$root
    }, blj = {
      p1.dash <- (x1 + 0.5)/(n1 + 1)
      p2.dash <- (x2 + 0.5)/(n2 + 1)
      vd <- p1.dash * (1 - p1.dash)/n1 + p2.dash * (1 - 
                                                      p2.dash)/n2
      term2 <- kappa * sqrt(vd)
      est.dash <- p1.dash - p2.dash
      CI.lower <- max(-1, est.dash - term2)
      CI.upper <- min(1, est.dash + term2)
    }, ha = {
      term2 <- 1/(2 * min(n1, n2)) + kappa * sqrt(p1.hat * 
                                                    (1 - p1.hat)/(n1 - 1) + p2.hat * (1 - p2.hat)/(n2 - 
                                                                                                     1))
      CI.lower <- max(-1, est - term2)
      CI.upper <- min(1, est + term2)
    }, mn = {
      .conf <- function(x1, n1, x2, n2, z, lower = FALSE) {
        p1 <- x1/n1
        p2 <- x2/n2
        p.hat <- p1 - p2
        dp <- 1 + ifelse(lower, 1, -1) * p.hat
        i <- 1
        while (i <= 50) {
          dp <- 0.5 * dp
          y <- p.hat + ifelse(lower, -1, 1) * dp
          score <- .score(p1, n1, p2, n2, y)
          if (score < z) {
            p.hat <- y
          }
          if ((dp < 1e-07) || (abs(z - score) < 1e-06)) (break)() else i <- i + 
            1
        }
        return(y)
      }
      .score <- function(p1, n1, p2, n2, dif) {
        diff <- p1 - p2 - dif
        if (abs(diff) == 0) {
          res <- 0
        } else {
          t <- n2/n1
          a <- 1 + t
          b <- -(1 + t + p1 + t * p2 + dif * (t + 2))
          c <- dif * dif + dif * (2 * p1 + t + 1) + p1 + 
            t * p2
          d <- -p1 * dif * (1 + dif)
          v <- (b/a/3)^3 - b * c/(6 * a * a) + d/a/2
          s <- sqrt((b/a/3)^2 - c/a/3)
          u <- ifelse(v > 0, 1, -1) * s
          w <- (3.141592654 + acos(v/u^3))/3
          p1d <- 2 * u * cos(w) - b/a/3
          p2d <- p1d - dif
          n <- n1 + n2
          var <- (p1d * (1 - p1d)/n1 + p2d * (1 - p2d)/n2) * 
            n/(n - 1)
          res <- diff^2/var
        }
        return(res)
      }
      z = qchisq(conf.level, 1)
      CI.lower <- max(-1, .conf(x1, n1, x2, n2, z, TRUE))
      CI.upper <- min(1, .conf(x1, n1, x2, n2, z, FALSE))
    }, beal = {
      a <- p1.hat + p2.hat
      b <- p1.hat - p2.hat
      u <- ((1/n1) + (1/n2))/4
      v <- ((1/n1) - (1/n2))/4
      V <- u * ((2 - a) * a - b^2) + 2 * v * (1 - a) * 
        b
      z <- qchisq(p = 1 - alpha/2, df = 1)
      A <- sqrt(z * (V + z * u^2 * (2 - a) * a + z * v^2 * 
                       (1 - a)^2))
      B <- (b + z * v * (1 - a))/(1 + z * u)
      CI.lower <- max(-1, B - A/(1 + z * u))
      CI.upper <- min(1, B + A/(1 + z * u))
    })
    ci <- c(est = est, lwr.ci = min(CI.lower, CI.upper), 
            upr.ci = max(CI.lower, CI.upper))
    if (sides == "left") 
      ci[3] <- 1
    else if (sides == "right") 
      ci[2] <- -1
    return(ci)
  }
  lst <- Recycle(x1 = x1, n1 = n1, x2 = x2, n2 = n2, conf.level = conf.level, 
                 sides = sides, method = method)
  res <- t(sapply(1:attr(lst, "maxdim"), function(i) iBinomDiffCI(x1 = lst$x1[i], 
                                                                  n1 = lst$n1[i], x2 = lst$x2[i], n2 = lst$n2[i], conf.level = lst$conf.level[i], 
                                                                  sides = lst$sides[i], method = lst$method[i])))
  lgn <- Recycle(x1 = if (is.null(names(x1))) 
    paste("x1", seq_along(x1), sep = ".")
    else names(x1), n1 = if (is.null(names(n1))) 
      paste("n1", seq_along(n1), sep = ".")
    else names(n1), x2 = if (is.null(names(x2))) 
      paste("x2", seq_along(x2), sep = ".")
    else names(x2), n2 = if (is.null(names(n2))) 
      paste("n2", seq_along(n2), sep = ".")
    else names(n2), conf.level = conf.level, sides = sides, method = method)
  xn <- apply(as.data.frame(lgn[sapply(lgn, function(x) length(unique(x)) != 
                                         1)]), 1, paste, collapse = ":")
  rownames(res) <- xn
  return(res)
}