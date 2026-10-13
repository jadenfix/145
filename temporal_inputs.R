# Validate the calendar contract used by the monthly lag models.  Treating a
# previous row as a previous month is only correct when every adjacent row is
# exactly one calendar month apart.
validate_monthly_dates <- function(dates) {
  dates <- as.Date(dates)
  if (anyNA(dates)) stop('Valid monthly dates required')
  if (is.unsorted(dates,strictly=TRUE)) stop('Unique increasing dates required')
  month_index <- as.integer(format(dates,'%Y'))*12L + as.integer(format(dates,'%m'))
  bad <- which(diff(month_index)!=1L)
  if (length(bad)) {
    first <- bad[[1L]]
    stop(sprintf('Consecutive monthly dates required; gap between %s and %s',dates[[first]],dates[[first+1L]]))
  }
  invisible(dates)
}

# Build lagged design rows using past observations only; no archived lead columns.
past_inputs <- function(price,dates,lag) {
  dates <- as.Date(dates)
  if (length(price)!=length(dates) || anyNA(dates) || any(!is.finite(price))) stop('Finite prices and valid dates required')
  validate_monthly_dates(dates)
  if (length(lag)!=1L || lag<1L || lag!=as.integer(lag) || lag>=length(price)) stop('Lag must be a positive integer shorter than the history')
  rows <- seq.int(lag+1L,length(price))
  aligned <- data.frame(date=dates[rows],predictor_date=dates[rows-lag],target=price[rows],past=price[rows-lag])
  stopifnot(all(aligned$predictor_date < aligned$date))
  aligned
}

alignment_summary <- function(price,dates,lags=1:4) {
  summaries <- lapply(lags,function(lag) {
    aligned <- past_inputs(price,dates,lag)
    data.frame(
      lag_months=lag,
      observations=nrow(aligned),
      target_start=min(aligned$date),
      target_end=max(aligned$date),
      predictor_start=min(aligned$predictor_date),
      predictor_end=max(aligned$predictor_date)
    )
  })
  do.call(rbind,summaries)
}
