\name{t_test_bias}
\alias{t_test_bias}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
t_test_bias takes simulation result and calculates p-value using paired sample t-test
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Returns a dataframe with columns: \code{alpha}, \code{maf},\code{beta} and \code{pvalue}
}
\usage{
t_test_bias(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{Dataframe containing results from simulation. The dataframe must have \code{maf}, \code{true_beta}, \code{mix_param} columns.
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
