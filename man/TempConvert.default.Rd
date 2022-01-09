\name{TempConvert.default}
\alias{TempConvert.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Funkcja TempConvert
}
\description{
Konwersja Temperatury
}
\usage{
TempConvert.default(x, metoda = "ctok")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{metoda}{
%%     ~~Describe \code{metoda} here~~
}
}
\details{
det
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
function (x, metoda = "ctok") 
{
    stopifnot(is.numeric(x))
    metoda <- match.arg(metoda, c("ctok", "ftoc", "ctof", "ktoc", 
        "ftok", "ktof"))
    temp_K <- x + 273.15
    result <- list(Temperatura = paste(x, "°C"), Konwersja = paste(temp_K, 
        "°K"), Temp = x, Konw = temp_K)
    class(result) <- append(class(result), "TempConvert")
    return(result)
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
