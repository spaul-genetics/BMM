\name{lrts_qqplot}
\alias{lrts_qqplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Provides Q-Q plot of LRTS of the NULL samples and 1/2 chisq_1 + 1/2 chisq_2
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Helping function to plot a grid of figures that contains density and qqplots
}
\usage{
lrts_qqplot(data,p,title,size)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{The simulation results containing columns \code{mix_param}, \code{true_beta}, \code{maf}, \code{like_ratio_stat}}
  \item{title}{Title of the plot.}
  \item{size}{Base size of theme_bw of ggplot2}
  \item{p}{Minor allele frequency to be used to subset the data. If not provided the data will be collapsed over mafs.}
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
Subrata Paul
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
