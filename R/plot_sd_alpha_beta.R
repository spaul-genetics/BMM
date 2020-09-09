
plot_sd_alpha_beta <- function(dat) {
  betas <- unique(dat$true_beta)
  sd_alpha <-
    sapply(seq(0.5, 0.9, 0.1), function(alpha)
      sapply(betas, function(beta)
        sd(dat$Mix_param[dat$mix_param == alpha & dat$true_beta == beta])))
  sd_alpha <- as.data.frame(sd_alpha)
  names(sd_alpha) <- paste('Alpha', seq(0.5, 0.9, 0.1), sep = '')
  sd_alpha$beta <- betas

  sd_alpha_melt <- reshape2::melt(sd_alpha, id.vars = 'beta')
  alpha_vs_beta <- ggplot2::ggplot(data = sd_alpha_melt, aes(x = beta, y = value)) +
    geom_line(aes(colour = variable), size = 0.2) +
    xlab(expression(Effect ~ Size ~ group('(', beta, ')'))) +
    ylab(expression(SD ~ group('(', hat(alpha), ')'))) + theme_bw(base_size = 6) +
    theme(
      legend.key.size = unit(3, "mm"),
      legend.position = 'bottom',
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(-3, -3, -3, -3)
    ) +
    scale_colour_discrete(name = expression(paste(Mixing ~ Proportions, ", ", ~ alpha, ' : ')) ,
                          labels = c('0.5', '0.6', '0.7', '0.8', '0.9'))
  ## Variance of beta
  sd_beta <-
    sapply(seq(0.5, 0.9, 0.1), function(alpha)
      sapply(betas, function(beta)
        sd(dat$beta[dat$mix_param == alpha &
                      dat$true_beta == beta & dat$maf > 0.1])))
  sd_beta <- as.data.frame(sd_beta)
  names(sd_beta) <- paste('Alpha', seq(0.5, 0.9, 0.1), sep = '')
  sd_beta$beta <- betas

  sd_beta_melt <- reshape2::melt(sd_beta, id.vars = 'beta')
  beta_vs_alpha <- ggplot2::ggplot(data = sd_beta_melt, aes(x = beta, y = value)) +
    geom_line(aes(colour = variable), size = 0.2) +
    xlab(expression(Effect ~ Size ~ group('(', beta, ')'))) +
    ylab(expression(SD ~ group('(', hat(beta), ')'))) + theme_bw(base_size = 6) +
    theme(
      legend.key.size = unit(3, "mm"),
      legend.position = 'bottom',
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(-3, -3, -3, -3)
    ) +
    scale_colour_discrete(name = expression(paste(Mixing ~ Proportions, ", ", ~ alpha, ' : ')) ,
                          labels = c('0.5', '0.6', '0.7', '0.8', '0.9'))
  beta_vs_alpha

  g_legend <- function(a.gplot) {
    tmp <- ggplot2::ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x)
      x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
  }

  mylegend <- g_legend(alpha_vs_beta)

  sd_alpha_beta <-
    gridExtra::grid.arrange(
      arrangeGrob(
        beta_vs_alpha + theme(legend.position = "none") + labs(tag = '(A)'),
        alpha_vs_beta + theme(legend.position =
                                "none") + labs(tag = '(B)'),
        nrow = 1
      ),
      mylegend,
      nrow = 2,
      heights = c(10, 1)
    )
  return(sd_alpha_beta)
}
