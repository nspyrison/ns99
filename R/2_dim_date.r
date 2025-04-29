#' Create a Date Dimension Table
#'
#' This function generates a date dimension table with various date attributes.
#'
#' @param start_date A character string representing the start date in "YYYY-MM-DD" format.
#' @param end_date A character string representing the end date in "YYYY-MM-DD" format.
#' @return A data frame containing the date dimension table with attributes such as year, quarter, month, month name, week, day, day name, day of year, and whether the day is a weekend.
#' @examples
#' dim_date <- create_dim_date("2025-01-01", "2025-12-31")
create_dim_date <- function(start_date, end_date) {
  # Generate a sequence of dates
  dates <- seq.Date(from = as.Date(start_date), to = as.Date(end_date), by = "day")
  
  # Create a data frame with date attributes
  dim_date <- data.frame(
    date = dates,
    year = format(dates, "%Y"),
    quarter = paste0("Q", ceiling(as.numeric(format(dates, "%m")) / 3)),
    month = format(dates, "%m"),
    month_name = format(dates, "%B"),
    week = format(dates, "%U"),
    day = format(dates, "%d"),
    day_name = format(dates, "%A"),
    day_of_year = format(dates, "%j"),
    is_weekend = ifelse(format(dates, "%u") %in% c("6", "7"), TRUE, FALSE)
  )
  
  return(dim_date)
}
