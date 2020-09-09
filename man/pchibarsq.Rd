\name{pchibarsq}
\alias{pchibarsq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Provides CDF for mixture of chi-quared distribution alpha chisq_(n-1) + (1-alpha) chisq_n
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
To calculate CDF for 1/2 chisq_1 + 1/2 chisq_2 use df = 2. In general df = n provides CDF for 1/2 chisq_(n-1) + 1/2 chisq_n. If mixing proportion is alpha then df =n gives CDF for alpha chisq_(n-1) + (1-alpha) chisq_n
}
\usage{
pchibarsq(P,df,mix,lower.tail,log.p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{Numeric value or vector}
 \item{df}{Degrees of freedom}
 \item{mix}{Mixing proportion. Default 0.5}
 \item{lower.tail}{Logical}
 \item{log.p}{Logical}
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
