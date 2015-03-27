#
#	Rpatches.R
#Fri Nov 20 17:18:37 CET 2009

# geepack patch

anovageePrim2 = function (m1, m2, ...)
{
    mm1 <- model.matrix(m1)
    mm2 <- model.matrix(m2)
    P1 <- mm1 %*% solve(t(mm1) %*% mm1) %*% t(mm1)
    P2 <- mm2 %*% solve(t(mm2) %*% mm2) %*% t(mm2)
    e2 <- mm2 - P1 %*% mm2
    e1 <- mm1 - P2 %*% mm1
    m2inm1 <- all(apply(e2, 2, var) < 1e-10)
    m1inm2 <- all(apply(e1, 2, var) < 1e-10)
    if (!any(c(m2inm1, m1inm2)))
        cat("Models not nested\n")
    else if (all(c(m2inm1, m1inm2)))
        cat("Models are identical\n")
    else {
        if (m1inm2) {
            tmp <- m1
            m1 <- m2
            m2 <- tmp
        }
        mm1 <- model.matrix(m1)
        mm2 <- model.matrix(m2)
        mf1 <- paste(paste(formula(m1))[c(2, 1, 3)], collapse = " ")
        mf2 <- paste(paste(formula(m2))[c(2, 1, 3)], collapse = " ")
        mm <- cbind(mm2, mm1)
        qmm <- qr(mm)
        qmmq <- qr.Q(qmm)
        nymm1 <- as.data.frame(qmmq[, 1:qmm$rank])
        colnames(nymm1) <- paste("parm", 1:ncol(nymm1), sep = ".")
        nymm2 <- nymm1[, 1:ncol(mm2), drop = FALSE]
        formula1 <- formula(paste(formula(m1)[[2]], formula(m1)[[1]],
            paste(c("-1", colnames(nymm1)), collapse = "+"),
            collapse = ""))
        m1call <- m1$call
        nymm1[, paste(formula(m1)[[2]])] <- m1$y
        nymm1[, paste(m1call$id)] <- m1$id
        m1call$offset <- m1$offset
        m1call$weights <- m1$weights
        m1call$formula <- formula1
        m1call$data <- nymm1
        m1ny <- eval(m1call)
        beta <- coef(m1ny)
        vbeta <- summary(m1ny)$cov.unscaled
        df <- dim(mm1)[2] - dim(mm2)[2]
        rbeta <- rep(1, length(beta))
        rbeta[1:df] <- 0
        beta0 <- rev(rbeta)
        zeroidx <- beta0 == 0
        X2 <- t(beta[zeroidx]) %*% solve(vbeta[zeroidx, zeroidx,
            drop = FALSE]) %*% beta[zeroidx]
        topnote <- paste("Model 1", mf1, "\nModel 2", mf2)
        title <- "Analysis of 'Wald statistic' Table\n"
        table <- data.frame(Df = df, X2 = X2, p = 1 - pchisq(X2,
            df))
        dimnames(table) <- list("1", c("Df", "X2", "P(>|Chi|)"))
        val <- structure(table, heading = c(title, topnote),
            class = c("anova", "data.frame"))
        return(val)
    }
}
