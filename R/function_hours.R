#' Convert minutes to HH:MM format
#'
#' Converts a duration expressed in minutes into a character string in
#' `"HH:MM"` format.
#'
#' @param minutes Numeric. Duration expressed in minutes.
#'
#' @return
#' A character string representing the duration in `"HH:MM"` format.
#'
#' @examples
#' function_hours(125)
#' # Returns "02:05"
#'
#' @export

function_hours <- function(minutes) {
  minutes <- round(minutes)

  hs_tot <- minutes / 60

  integer <- floor(hs_tot)

  decimal <- hs_tot - integer

  mins <- round(decimal * 60, 1)

  if (integer <= 9) {
    integer_2 <- paste("0", integer, sep = "")
  } else {
    integer_2 <- integer
  }

  if (mins <= 9) {
    mins_2 <- paste("0", mins, sep = "")
  } else {
    mins_2 <- mins
  }

  output <- paste(integer_2, mins_2, sep = ":")

  return(output)
}
