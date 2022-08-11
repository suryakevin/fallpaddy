#' Extract sampling times from tip labels
#'
#' @description This function extracts the sampling times from tip labels.
#'
#' @details Viral tip labels typically include collection or sampling dates in
#'   the ISO 8601 format (YYYY-MM-DD). Whenever the sampling date is only
#'   available in the year (YYYY) or month of the year format (YYYY-MM), this
#'   function fixes sampling time to the middle of the year or the month. This
#'   step introduces a bias but is necessary for conversion to the decimal year
#'   unit. The significance of this bias depends on the timescale of the tree.
#'   For example, within-year date errors are arguably negligible when the
#'   timescale spans decades.
#'
#' @param tree An object of class `phylo` with tip labels
#' @param block The location of the sampling time in the tip label; viral tip
#'   labels typically contain metadata separated by vertical bars "|"; for
#'   example, if the sampling time is in the third "block" (e.g.,
#'   taxon_name|accession_number|sampling_time|sampling_location), then set
#'   `block = 3`.
#'
#' @return This function returns a vector of sampling dates in the decimal year
#'   format.
#'
#' @author Kevin Surya
#'
#' @import lubridate stringr zoo
#'
#' @export
#'
get_sampling_time <- function(tree, block) {
  tip_label <- tree$tip.label
  date <- word(string = tip_label, start = block, end = block, sep = "\\|")
  date <- gsub(pattern = "-00", replacement = "-01", x = date)
  data_time <- data.frame(tip_label, date)
  colnames(data_time) <- c("taxon", "date")
  for (i in 1:length(tip_label)) {
    if (nchar(data_time$date[i]) == 4) {
      data_time$date_format[i] <- "y"
    } else if (nchar(data_time$date[i]) == 7) {
      data_time$date_format[i] <- "ym"
    } else if (nchar(data_time$date[i]) == 10) {
      data_time$date_format[i] <- "ymd"
    }
  }
  for (j in 1:length(tip_label)) {
    if (data_time$date_format[j] == "y") {
      data_time$dec_year[j] <- as.numeric(data_time$date[j]) + 0.5
        # middle of the year
    } else if (data_time$date_format[j] == "ym") {
      data_time$dec_year[j] <- decimal_date(
        date = ymd(
          as.Date(
            x = as.yearmon(data_time$date[j], "%Y-%m"), frac = 0.5
          )
        )  # middle of the month
      )
    } else {
      data_time$dec_year[j] <- decimal_date(ymd(data_time$date[j]))
    }
  }
  time <- data_time[, "dec_year"]
  names(time) <- tip_label
  return(time)
}
