
#' Get the Month Name(s)
#'
#' @param i Month as a number
#'
#' @returns Three letter abbreviation for month
#' @export
#'
#' @examples get_month(2)
get_month = function(i){
  c("Jan", "Feb", "Mar", "Apr", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")[i]
}
