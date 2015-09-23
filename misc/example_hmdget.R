# Init ------------------------------------------------

library(dplyr) # data verbs, operations on subsets of data
library(hmdget) # download data from human mortality database

# System ----------------------------------------------

# System info
sessionInfo()

# Defaults --------------------------------------------

# HMD login information
username <- "***"; password <- "***"

# Test ------------------------------------------------

# download period and cohort exposures for all HMD countries
Dx <- HMDget(hmdcbook$Code, .timeframe = "p+c", .measure = "Dx",
             .username = username, .password = password)
Nx <- HMDget(hmdcbook$Code, .timeframe = "p+c", .measure = "Nx",
             .username = username, .password = password)
mx <- HMDget(hmdcbook$Code, .timeframe = "p+c", .measure = "mx",
             .username = username, .password = password)


# check
Dx %>% filter(Country == "GBRCENW", Year == 1939, Age == 100)
mx %>% filter(Country == "SWE", Year == 1900, Age == 100)

# join Dx and Nx, remove rows with NAs
full_join(Dx, Nx) %>% na.omit -> hmd
