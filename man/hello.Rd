\name{0-tow_df}
\alias{0-tow_df}
\title{0-tow_df}
\usage{
For each year a number of GLOBALS must be set.
These include:
    YEAR & Q.
Three or four primary datasets will be imported for each year

For years before 2009 there are:

events_data
catch_data
awl_data

For years after 2018 there is also:

shell_data

There are a number of functions that must be run in consecutive order to
clean the survey data.

0-tow_df()
1-bed_df()
2-catch_df()
3-awl_df()
}
\description{
Evaluate statewide scallop survey data.
}
\examples{
YEAR = 2019
Q = 0.83


}

