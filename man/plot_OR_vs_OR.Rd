\name{plot_OR_vs_OR}
\alias{plot_OR_vs_OR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Function to plot boxplot of estimated odds ratios
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
A ggplot that can be saved using ggsave function.
}
\usage{
plot_OR_vs_OR(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{The output data from simulation runs that contains columns with true effect size, mixture model estimates of effect size and effect size estimate using the logistic regression}
\item{mix}{The function will plot the data for simulation with mixing proportion equal to mix}
\item{pp}{The function will plot the data for simulations with maf equal to pp}
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

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
