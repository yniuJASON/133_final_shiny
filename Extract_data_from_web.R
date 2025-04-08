library(rvest)
library(httr)
library(tidyverse)
library(jsonlite)


# Function to generate the URL
get_faa_advisory <- function(date) {
  base_url <- "https://www.fly.faa.gov/adv/adv_list.jsp?"
  params <- list(
    WhichAdvisories = "ATCSCC",
    AdvisoryCategory = "All",
    dates = format(date, "%A, %m-%d-%Y"),
    AirFlow = "AirFlow",
    Ctop = "Ctop",
    Gstop = "Gstop",
    Gdelay = "Gdelay",
    Route = "Route",
    Other = "Other"
  )
  
  # Manually encode ',' as '%2C' and ' ' (space) as '+'
  encode_value <- function(value) {
    value <- gsub(",", "%2C", value)  # Encode comma
    value <- gsub(" ", "+", value)    # Encode space
    return(value)
  }
  
  # Construct query string
  query_string <- paste(names(params), sapply(params, encode_value), sep="=", collapse="&")
  
  return(paste0(base_url, query_string))
}



