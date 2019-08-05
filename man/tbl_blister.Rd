\name{tbl_blister}
\alias{tbl_blister}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
tbl_blister(scal_awl, tows)
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
    tbl_names <- c("Bed", "N", "0\%", "1-24\%", "25-49\%", "50-74\%",
        "75-100\%")

    x <- scal_awl \%>\%
        left_join(tows) \%>\%
        filter(!is.na(mud)) \%>\%
        count(Bed, mud) \%>\%
        group_by(Bed) \%>\%
        mutate(prop = round(n/sum(n) * 100, 1)) \%>\%
        spread(mud, prop) \%>\%
        replace(is.na(.), 0) \%>\%
        mutate(`4` = 0) \%>\%
        summarise(N = sum(n),
                  zero = round(max(`0`), 1),
                  one = round(max(`1`), 1),
                  two = round(max(`2`), 1),
                  three = round(max(`3`), 1),
                  four = round(max(`4`), 1)) \%>\%
        rename_at(names(.), function(x) tbl_names)
    write_csv(x, here::here(paste0("output/", YEAR, "/mud.csv")))
    x
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
