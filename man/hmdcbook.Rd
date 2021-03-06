% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hmdget.R
\docType{data}
\name{hmdcbook}
\alias{hmdcbook}
\title{HMD Country Codebook}
\format{A data frame with 46 rows and 2 variables: \describe{
  \item{Code}{Country code as used in Human Mortality Database}
  \item{Label}{Full country name in English} }
\tabular{ll}{
 Code \tab Label \cr
 AUS \tab Australia \cr
 AUT \tab Austria \cr
 BLR \tab Belarus \cr
 BEL \tab Belgium \cr
 BGR \tab Bulgaria \cr
 CAN \tab Canada \cr
 CHL \tab Chile \cr
 CZE \tab Czech Republic \cr
 DNK \tab Denmark \cr
 EST \tab Estonia \cr
 FIN \tab Finland \cr
 FRATNP \tab France (total population) \cr
 FRACNP \tab France (civilian population) \cr
 DEUTNP \tab Germany (total population) \cr
 DEUTE \tab Germany (east) \cr
 DEUTW \tab Germany (west) \cr
 GRC \tab Greece \cr
 HUN \tab Hungary \cr
 ISL \tab Iceland \cr
 IRL \tab Ireland \cr
 ISR \tab Israel \cr
 ITA \tab Italy \cr
 JPN \tab Japan \cr
 LVA \tab Latvia \cr
 LTU \tab Lithuania \cr
 LUX \tab Luxembourg \cr
 NLD \tab Netherlands \cr
 NZL_NP \tab New Zealand (total population) \cr
 NZL_MA \tab New Zealand (Maori population) \cr
 NZL_NM \tab New Zealand (non-Maori population) \cr
 NOR \tab Norway \cr
 POL \tab Poland \cr
 PRT \tab Portugal \cr
 RUS \tab Russia \cr
 SVK \tab Slovakia \cr
 SVN \tab Slovenia \cr
 ESP \tab Spain \cr
 SWE \tab Sweden \cr
 CHE \tab Switzerland \cr
 TWN \tab Taiwan \cr
 GBR_NP \tab United Kingdom \cr
 GBRTENW \tab England & Wales (total population) \cr
 GBRCENW \tab England & Wales (civilian population) \cr
 GBR_SCO \tab Scotland \cr
 GBR_NIR \tab Northern Ireland \cr
 USA \tab U.S.A. \cr
 UKR \tab Ukraine
}}
\source{
\url{http://www.mortality.org/cgi-bin/hmd/hmd_download.php}
}
\usage{
hmdcbook
}
\description{
A dataset containing country codes and corresponing labels as used in the HMD
database and webpage. Use it to specify the countries you want to download
from the HMD database. \code{\link{HMDget}} only allows for country codes
specified in this table in its \code{.country} argument.
}
\keyword{datasets}

