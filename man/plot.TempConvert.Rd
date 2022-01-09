\name{plot.TempConvert}
\alias{plot.TempConvert}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Narysuj Wykres
}
\description{
Narysuj Wykres
}
\usage{
plot.TempConvert(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
ref
}
\author{
BK
}
\note{
note
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
see
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, ...) 
{
    data <- data.frame(name = c(x$Temperatura, x$Konwersja), 
        value = c(x$Temp, x$Konw))
    barplot(height = data$value, names = data$name, col = rgb(0.2, 
        0.4))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
