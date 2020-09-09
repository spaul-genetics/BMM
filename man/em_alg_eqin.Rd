\name{em_alg_eqin}
\alias{em_alg_eqin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  EM algorithm for binomial mixture

}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
  Given data in the format of total number of samples and number of cases the function fits the Expectation Maximization algorithm to approximate MLE of the parameters in the Binomial Mixture Model

}
\usage{
em_alg_eqin(data, max_it,alpha,mu,beta,epsilon)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{The dataset that contains at least three columns named exactly as \code{Y.length}, \code{Y.sum} and \code{G}}
 \item{max_it}{Maximum number of iterations}
 \item{alpha}{The initial value for the mixing parameter. Default 0.5}
 \item{mu}{The initial value for the constant coefficient . Default -1}
 \item{beta}{The initial value for the effect size. Default 0}
 \item{epsilon}{The tolerance level. Default 1e-7}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The function returns a list with: it: number of iteration needed Estimated parameters: mu1, mu2, beta and their variances calculated from Fisher's Information matrix Likelihood
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Subrata Paul
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
