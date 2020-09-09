
qchibarsq<-function (p, df = 1, mix = 0.5)
{
  n <- max(length(p), length(df), length(mix))
  df <- rep(df, length.out = n)
  mix <- rep(mix, length.out = n)
  p <- rep(p, length.out = n)
  tmpf2 <- function(p, df, mix) {
    if (df > 1) {
      tmpf <- function(x) {
        pchibarsq(x, df, mix) - p
      }
      uniroot(tmpf, lower = qchisq(p, df - 1), upper = qchisq(p,
                                                              df))$root
    }
    else {
      newq <- (p - mix)/(1 - mix)
      ifelse(newq < 0, 0, qchisq(newq, df = 1))
    }
  }
  mapply(tmpf2, p, df, mix)
}
