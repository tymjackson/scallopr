\name{meat_wt_est}
\alias{meat_wt_est}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
meat_wt_est(scal_awl, scal_catch, beds, Q, boot = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{scal_awl}{
%%     ~~Describe \code{scal_awl} here~~
}
  \item{scal_catch}{
%%     ~~Describe \code{scal_catch} here~~
}
  \item{beds}{
%%     ~~Describe \code{beds} here~~
}
  \item{Q}{
%%     ~~Describe \code{Q} here~~
}
  \item{boot}{
%%     ~~Describe \code{boot} here~~
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
scal_catch \%>\%
    filter(Size == "large") \%>\%
    add_count(Bed) \%>\%
    transmute(Bed,
              tow_id,
              count,
              area_swept) -> x

  # Make data frame suitable for bootstrapping
  scal_awl \%>\%
    filter(size == "large", mwt_lb > 0, mwt_lb < rwt_lb) \%>\%
    select(tow_id, mwt_lb) \%>\%
    add_count(tow_id) \%>\%
    left_join(x, by = "tow_id") \%>\%
    replace_na(list(mwt_lb = 0, n = 0)) \%>\%
    group_by(Bed) \%>\%
    mutate(count = ifelse(n > count, n, count),
           bed_avg_mwt = mean(mwt_lb[mwt_lb > 0]),
           bed_var_mwt = var(mwt_lb[mwt_lb > 0]),
           n_i = n) \%>\%
    dplyr::select(-n) \%>\%
    group_by(Bed, tow_id) \%>\%
    summarize(count = unique(count),
              n_i = unique(n_i),
              sum_mwt = sum(mwt_lb),
              area_swept = mean(area_swept),
              d_hat = ifelse(n_i > 0,
                             count / n_i * sum_mwt / (Q * area_swept),
                             count * mean(bed_avg_mwt) / (Q * area_swept)),
              s2_mwt = ifelse(n_i > 1,
                              var(mwt_lb),
                              mean(bed_var_mwt)),
              s2_x = s2_mwt / (Q * area_swept)^2,
              s2_x_w = ifelse(n_i > 0, count * (count - n_i) / n_i, 0),
              s2_x_w = ifelse(n_i == 0 & count > 0, count^2, s2_x_w)) \%>\%
    left_join(beds, by = "Bed") \%>\%
    ungroup() -> data

  # Estimation function, given 'data' (one-time)

  data \%>\%
    group_by(Bed) \%>\%
    summarize(tows = unique(tows),
              est = mean(area_nm2) * mean(d_hat),
              se = mean(area_nm2) * sqrt(var(d_hat) / tows +
                                           mean(s2_x_w * s2_x)),
              cv = se / est,
              l95 = est * exp(-1.96 * sqrt(log(1 + cv^2))),
              u95 = est * exp(1.96 * sqrt(log(1 + cv^2)))) \%>\%
    mutate_at(vars(est, se, l95, u95), round, 0) -> est

  boot_ci <- function(split){

    rsample::analysis(split) \%>\%
      summarize(tows = unique(tows),
                est = mean(area_nm2) * mean(d_hat))
  }


  # To boot or not to boot

  if(boot){

    data \%>\%
      nest(-Bed, -tow_id) \%>\%
      mutate(boot = map(data, ~rsample::bootstraps(., 1000))) \%>\%
      unnest(boot) \%>\%
      mutate(models = map(splits, ~boot_ci(.x))) \%>\%
      unnest(models) \%>\%
      group_by(Bed) \%>\%
      summarise(l95 = quantile(est, 0.25),
                u95 = quantile(est, 0.975)) \%>\%
      mutate_at(vars(l95, u95), round, 0) -> ci


    est \%>\%
      dplyr::select(-l95, -u95) \%>\%
      left_join(ci) -> est
    est

  } else {

    est
  }

  write_csv(est, here::here(paste0("output/", YEAR, "/mwt_biom_est.csv")))
  est
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
