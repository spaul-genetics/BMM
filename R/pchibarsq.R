pchibarsq<-function (p, df = 1, mix = 0.5, lower.tail = TRUE, log.p = FALSE)
{
  df <- rep(df, length.out = length(p))
  mix <- rep(mix, length.out = length(p))
  c1 <- ifelse(df == 1, if (lower.tail)
    1
    else 0, pchisq(p, df - 1, lower.tail = lower.tail))
  c2 <- pchisq(p, df, lower.tail = lower.tail)
  r <- mix * c1 + (1 - mix) * c2
  if (log.p)
    log(r)
  else r
}
