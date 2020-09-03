#' Connect Probable Cases
#'
#' Probabilistically estimates a likely tranmission chain using EMR
#' derrived data.
#'
#' @param dat the dataframe of likely connect cases with a column named "date"
#'     indicating the onset or positive test date and "patient_id", a unique
#'     identifier for the record.
#' @param weights_in the weights to use for the serial interval if available
#' @param threshold integer, the threshold in days at which to discard a connected
#'     case (e.g. >30 days from previous case, then discard).
#' @param exposure_link string or column name, how these cases are connected
#'
#' @importFrom data.table .SD `:=`
#' @export

connect_probable_cases <- function(dat, weights_in = NULL,
                                   threshold = 30, exposure_link = NULL){

  # Check Names
  stopifnot(all(c("date", "patient_id") %in% names(dat)))

  # Check Threshold
  stopifnot(is.numeric(threshold))

  if(is.null(weights_in)){
    SI_param = epitrix::gamma_mucv2shapescale(4.7, 2.9/4.7)
    SI_distribution <- distcrete::distcrete("gamma", interval = 1,
                                            shape = SI_param$shape,
                                            scale = SI_param$scale, w = 0.5)

    w <- SI_distribution$d(1:21)
  }

  dat <- data.table::setDT(dat)

  dat <- dat[order(dat$date),]

  dat$id <- 1:nrow(dat)
  dates <- dat$date

  # If all the dates are equivalent, the make sure that
  if(length(unique(dates))==1){
    dates[1] <- dates[1]-1
  }

  cluster <- outbreaker2::outbreaker_data(dates = dates, w_dens = w)

  res <- outbreaker2::outbreaker(cluster)

  out <- summary(res)

  tree <- data.table::setDT(out$tree)

  # If non of the cases can be linked, will assign the oldest case as the index
  # case. This isn't super elegent, but it keeps the cluster together.
  if(sum(!is.na(tree$from))==0){
    tree$from[2:nrow(tree)] <-1L
  }


  tree <- tree[time<=threshold, .(from, to)]

  tree <- merge(tree, dat[ ,c("id", "patient_id", "date")],
                by.x = "from", by.y="id", all.x = TRUE)

  tree <-tree[,.(patient_id,to, date)]

  names(tree) <- c("from", "to", "from_date")

  tree <- merge(tree, dat[ ,c("id", "patient_id", "date")],
                by.x = "to", by.y="id", all.x = TRUE)

  tree <- tree[,.(from, patient_id, from_date, date)]

  names(tree) <- c("from", "to", "from_date", "to_date")

  tree <- tree[,diff_dt_onset := abs(as.numeric(to_date-from_date))]
  tree <- tree[!is.na(from), list(from,to, diff_dt_onset)]

  if(!is.null(exposure_link)){
    tree <- cbind(tree, exposure_link = exposure_link)
  }
  # Returning as DF to avoid conflicts with purrr
  as.data.frame(tree)

}
