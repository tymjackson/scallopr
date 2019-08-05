\name{tbl_sex}
\alias{tbl_sex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
tbl_sex(scal_awl, tows)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{scal_awl}{
%%     ~~Describe \code{scal_awl} here~~
}
  \item{tows}{
%%     ~~Describe \code{tows} here~~
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

## The function is currently defined as
function (scal_awl, tows)
{
    scal_awl \%>\%
    left_join(tows, by = "tow_id") \%>\%
    filter(!is.na(sex), size == "large") \%>\%
    group_by(Bed, sex) \%>\%
    summarise(n = n()) \%>\%
    mutate(freq = round(n/sum(n) * 100, digits = 1)) \%>\%
    group_by(Bed) \%>\%
    mutate(N = sum(n)) \%>\%
    spread(., sex, freq) \%>\%
    dplyr::select(-N, everything(), -n) \%>\%
    replace(is.na(.), 0) \%T>\%
    write_csv(here::here(paste0("output/", YEAR, "/sex_tbl.csv")))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
