\name{plot_lrts_density_qq}
\alias{plot_lrts_density_qq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Plots a two by three grid of density and Q-Q plots
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Produces a figure using \code{lrts_density} and \code{lrts_qqplot} functions. First row of the figure has three density plots and the second row has three qqplots. The null distribution of likelihood ratio statistic are compared to 1/2 chisq_1 + 1/2 chisq_2
}
\usage{
plot_lrts_density_qq(data,maf,size,text.size)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{The simulation results containing columns \code{mix_param}, \code{true_beta}, \code{maf}, \code{like_ratio_stat}}
 \item{maf}{A vector of three allele frequencies}
 \item{size}{base-size for theme_bw of ggplot2}
 \item{text.size}{text_size for title, axis titles with defaults 20 and 20 respectively}
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
