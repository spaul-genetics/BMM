\name{bmm_eqin}
\alias{bmm_eqin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Applies EM algorithm to simulated data for Binomial Mixture Model
}
\description{
    As input it takes data with phenotype, genotype and ancestry cluster Apply Binomial Mixture model on the data and calculate likelihood, likelihood ratio statistics etc.
}
\usage{
bmm_eqin(data = dat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{A dataframe containing at least three columns: \code{Y}, \code{G}, and \code{Cluste}}
  \item{mu}{
  Initial value of the constant coefficient. Default 0
  }
  \item{max_it}{Maximum number of iteration for the EM algorithm. Default 50}
  \item{epsilon}{Tolerance, to be used as threshold for Aitken Stopping Criterion}
  \item{rep_em}{The EM algorithm will run with different starting value of mixing proportion if the best estimate is larger than rep_em}

}

\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The function returns a vector containing: Iterations, Mix_param, mu1, mu2, beta, likelihood, var_beta, lik_ratio_stat, non_mix_estimate, non_mix_pvalue, lik_logit, lik_ratio_logit
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Subrata Paul
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
em_alg_eqin
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
