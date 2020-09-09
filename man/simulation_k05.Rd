\name{simulation_k05}
\alias{simulation_k05}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
Simulation using disease prevalence, K = 0.05
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("simulation_k05")}
\format{
  A data frame with 2,310,000 observations on the following 17 variables.
  \describe{
    \item{\code{sim}}{Simulation index}
    \item{\code{n_cases}}{Number of cases in simulated data. For all the simulations 5000 cases and 20,000 were used.}
    \item{mix_param}{Proportion of associated group used for simulations}
    \item{maf}{Minor allele frequency used in simulation}
    \item{true_beta}{The effect size used in simulation}
    \item{Iteration}{Number of iteration needed to converge}
    \item{Mix_param}{Estimated mixing proportion}
    \item{mu1}{Estimated constant coefficient for non-associated group}
    \item{mu2}{Estimated constant coefficient for associated group. In this results mu1 and mu2 are equal.}
    \item{beta}{Effect size estimates}
    \item{likelihood}{Likelihood for the Binomial Mixture model}
    \item{var_beta}{Variance of effect size estimates}
    \item{lik_ratio_stat}{Likelihood ratio statistic for the compound hypothesis test that mixing proportion = 0 or beta =0}
    \item{non_mix_estimate}{Estimate of effect size using the logistic regression}
    \item{non_mix_pvalue}{P-value from logistic regression model}
    \item{lik_logit}{Likelihood of the data for logistic regression model}
    \item{lik_ratio_logit}{Likelihood ratio statistic for the logistic regression model}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(simulation_k05)
## maybe str(simulation_k05) ; plot(simulation_k05) ...
}
\keyword{datasets}
