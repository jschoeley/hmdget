# Input -----------------------------------------------

#' HMD Country Codebook
#'
#' A dataset containing country codes and corresponing
#' labels as used in the HMD database and webpage. Use it to
#' specify the countries you want to download from the HMD
#' database. \code{\link{HMDget}} only allows for country codes
#' specified in this table in its \code{.country} argument.
#'
#' @format A data frame with 46 rows and 2 variables:
#'   \describe{ \item{Code}{Country code as used in Human
#'   Mortality Database} \item{Label}{Full country name in
#'   English} }
#' @source
#' \url{http://www.mortality.org/cgi-bin/hmd/hmd_download.php}
#'
"hmdcbook"

# Retrieval function ----------------------------------

#' Download HMD data from the web
#'
#' @export
#' @param .country: 3 letter country code [string, length >
#'   0]
#' @param .timeframe: "c" (cohort), "p" (period)  or "p+c"
#'   (period and cohort) [string, length == 1]
#' @param .measure: "Dx" (death counts) or "Nx" (exposures
#'   in person years) [string, length == 1]
#' @param .username: HMD username [string, length == 1]
#' @param .password: HMD password [string, length == 1]
#' @return Death counts or exposures by country, timeframe,
#'   year, age and sex in long format with numeric age
#'   categories [data frame].
#' @importFrom dplyr %>% group_by do
HMDget <- function (.country, .timeframe = "p+c", .measure,
                    .username, .password) {

  # argument parsing
  arg <- HMDargs(list(country = .country,
                      timeframe = .timeframe,
                      measure = .measure,
                      username = .username,
                      password = .password))

  # prepare dataframe to hold downloaded data
  hmd.design <- expand.grid(Country = arg$country,
                            Timeframe = arg$timeframe,
                            stringsAsFactors = FALSE)

  # get data from web
  hmd.design %>% group_by(Country, Timeframe) %>%
    do(HMDhttp(., .measure = arg$measure,
               .username = arg$username,
               .password = arg$password)) -> hmd

  # tidy data
  hmd <- HMDtidy(hmd, arg$measure)

  return(hmd)
}

# Argument parsing, sanitizing, error check -----------

#' Parse Arguments of \code{HMDget} Function
#'
#' @param .x \code{\link{HMDget}} arguments [list]
#' @return Parsed arguments [list]
#' @details This functions does error checking and
#'   translates the function arguments of \code{\link{HMDget}} into
#'   the needed format.
HMDargs <- function (.x) {

  # error check
  if (any(!(.x$country %in% hmdcbook$Code)))
    stop("Argument <.country> contains element not compliant with HMD country codes.")
  if (length(unique(.x$country)) < length(.x$country)) {
    .x$country <- unique(.x$country)
    warning("Removed duplicated elements from argument <.country>.")
  }
  if (length(.x$timeframe) > 1)
    stop("Argument <.timeframe> length > 1.")
  if (any(!(.x$timeframe %in% c("p", "c", "p+c"))))
    stop("Argument <.timeframe> not element of ('p', 'c', 'p+c').")
  if (any(!(.x$measure %in% c("Dx", "Nx"))))
    stop("Argument <.measure> not element of ('Dx', 'Nx').")

  # parse arguments
  if (identical(.x$timeframe, "p")) .x$timeframe <- "Period"
  if (identical(.x$timeframe, "c")) .x$timeframe <- "Cohort"
  if (identical(.x$timeframe, "p+c")) .x$timeframe <- c("Period", "Cohort")

  return(.x)
}

# Download from HMD server ----------------------------

#' Download HMD data from the web
#'
#' @param .x: Country + Timeframe design matrix [data frame]
#' @param .measure: "Dx" (death counts) or "Nx" (exposures
#'   in person years) [string, length == 1]
#' @param .username: HMD username [string, length == 1]
#' @param .password: HMD password [string, length == 1]
#' @return HMD web data [data frame]
#' @importFrom httr GET authenticate content http_status
#' @importFrom dplyr data_frame
HMDhttp <- function(.x, .measure, .username, .password) {

  if (identical(.measure, "Nx")) {
    file <- c(Period = "Exposures_1x1.txt", Cohort = "cExposures_1x1.txt")
  }
  if (identical(.measure, "Dx")) {
    file <- c(Period = "Deaths_1x1.txt", Cohort = "Deaths_lexis.txt")
  }

  # generate web adresses for data
  # dependent on country and timeframe
  if (identical(.x$Timeframe, "Period")) {
    path <- paste0("http://www.mortality.org/hmd/", .x$Country,
                   "/STATS/", file["Period"])
  }
  if (identical(.x$Timeframe, "Cohort")) {
    path <- paste0("http://www.mortality.org/hmd/", .x$Country,
                   "/STATS/", file["Cohort"])
  }

  # connect to hmd webpage
  hmd <- GET(path,
             authenticate(user = .username, password = .password))

  # if connection is successful, then
  if (identical(http_status(hmd)$category, "success")) {
    # read data to table
    hmd <- content(hmd, as = "text")
    hmd <- read.table(textConnection(hmd),
                      header = TRUE, skip = 2, na.strings = ".",
                      stringsAsFactors = FALSE)

  } else { # if connection not successful return NAs
    hmd <- data_frame(Year = NA, Age = NA,
                      Female = NA, Male = NA, Total = NA)
    warning(paste0("I was not able to download ",
                   tolower(.x$Timeframe), " ", .measure, " for country ",
                   .x$Country, ". NAs produced instead."))
  }

  # convert period-cohort-age death counts to cohort-age
  if (identical(.measure, "Dx") &&
      identical(.x$Timeframe, "Cohort")) {
    hmd <- HMDapc2ac(hmd)
  }

  return(hmd)
}

# Convert between timeframes --------------------------

#' Convert HMD Data Between Timeframes
#'
#' @param .x HMD data by Lexis triangles [data frame]
#' @return HMD data by cohort-age [data frame]
#' @details The Human Mortality Database provides cohort
#'   death counts in Lexis triangle format (measures by
#'   cohort, period and age). In order to use these counts
#'   with the cohort-age exposures provided by the HMD, the
#'   Lexis triangle counts have to be aggregated into
#'   cohort-age counts. This function sums up the two Lexis
#'   triangles for each cohort-period-age-group to get
#'   cohort death counts by age:
#'
#'   A person in cohort c and age x could have died in years
#'   t and t+1, therefore if only the cohort death counts by
#'   age are of interest, one must sum up the deaths of
#'   cohort c and age x across the years t and t+1.
#' @importFrom dplyr %>% group_by summarise rename
HMDapc2ac <- function (.x) {

  .x %>% group_by(Cohort, Age) %>%
    summarise(Female = sum(Female, na.rm = TRUE),
              Male = sum(Male, na.rm = TRUE),
              Total = sum(Total, na.rm = TRUE)) %>%
    rename(Year = Cohort) -> result

  return(result)
}

# Tidying ---------------------------------------------

#' Tidy HMD web data
#'
#' @param .x HMD web data [data frame]
#' @param .measure: "Dx" (death counts) or "Nx" (exposures
#'   in person years) [string, length == 1]
#' @return Tidied HMD data [data frame]
#' @details This function takes HMD data as downloaded from
#'   the web, reshapes it into long format (\code{Female},
#'   \code{Male}, \code{Total} columns to \code{Sex} +
#'   \code{Value} column) and converts the age variable to
#'   numeric (and in the process changing the age category
#'   \code{110+} to 110).
#' @importFrom dplyr %>% group_by do right_join select_ data_frame ungroup mutate
#' @importFrom tidyr gather_
HMDtidy <- function (.x, .measure) {

  # transform Age variable to integer (110+ becomes 110)
  .x$Age <- as.integer(gsub("\\+", "", as.character(.x$Age)))

  # convert to long format
  gather_(.x,
          key_col = "Sex",
          value_col = .measure,
          gather_cols = c("Female", "Male", "Total")) %>%
    # map data on 0-110 age grid
    group_by(Country, Timeframe, Sex, Year) %>%
    do(right_join(select_(., "Age", .measure),
                  data_frame(Age = 0:110),
                  by = "Age")) %>% ungroup %>%
    # data frame styling
    mutate(Country = as.factor(Country),
           Timeframe = as.factor(Timeframe),
           Sex = as.factor(Sex)) -> hmd

  return(hmd)
}
