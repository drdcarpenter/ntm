#' @title ntm
#' @description Calculate tranquility scores for sites in a dataframe
#' @param x a dataframe containing NAMM, PONS, Lrr and Lat columns
#' @return a tibble with probabilities for each tranquility score, a tranquility score and a tranquility description
#' @details Your dataframe must contain columns with the attributes NAMM, PONS, Lrr and Lat
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE
#'  df <- data(ntm)
#'  ntm(df)
#'  }
#' }
#' @seealso 
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{case_when}}
#' @rdname ntm
#' @export 
#' @importFrom dplyr mutate case_when
ntm <- function(x){
  
  # check for missing data
  if(!"NAMM" %in% colnames(x)) {
    cat("ERROR: NAMM column missing.\n");
  }
  
  if(!"PONS" %in% colnames(x)) {
    cat("ERROR: PONS column missing.\n");
  }
  
  if(!"Lrr" %in% colnames(x)) {
    cat("ERROR: Lrr column missing.\n");
  }
  
  if(!"Lat" %in% colnames(x)) {
    cat("ERROR: Lat column missing.\n");
  }
  
  # check data are numeric
  if(!is.numeric(x$NAMM)) {
    cat("ERROR: NAMM is not numeric.\n");
  }
  
  if(!is.numeric(x$PONS)) {
    cat("ERROR: PONS is not numeric.\n");
  }
  
  if(!is.numeric(x$Lrr)) {
    cat("ERROR: Lrr is not numeric.\n");
  }
  
  if(!is.numeric(x$Lat)) {
    cat("ERROR: Lat is not numeric.\n");
  }
  # calculate scores for each level
  t_scores <- x %>% 
    dplyr::mutate(ts_1 = 0,
           ts_2 = 74.17398 + (NAMM * 9.57158) + (PONS * 5.32434) + (Lrr * 0.08640) - (Lat * 1.21115),
           ts_3 = 114.46581 + (NAMM * 10.93007) + (PONS * 5.27272) + (Lrr * 0.08981) - (Lat * 1.85779),
           ts_4 = 129.58104 + (NAMM * 11.55970) + (PONS * 5.33385) + (Lrr * 0.13029) - (Lat * 2.17490),
           ts_5 = 133.98827 + (NAMM * 12.81092) + (PONS * 5.35484) + (Lrr * 0.12512) - (Lat * 2.32374),
           ts_6 = 136.05294 + (NAMM * 14.11910) + (PONS * 5.37543) + (Lrr * 0.11841) - (Lat * 2.46001),
           ts_7 = 132.75350 + (NAMM * 16.44831) + (PONS * 5.38689) + (Lrr * 0.05909) - (Lat * 2.56049),
           ts_8 = 116.06068 + (NAMM * 19.41205) + (PONS * 5.45928) + (Lrr * 0.08844) - (Lat * 2.57928))
  
  # set up columns to select max score
  cols_to_select <- c("ts_1", "ts_2", "ts_3", "ts_4", "ts_5", "ts_6", "ts_7", "ts_8")
  
  # select max score and assign tranquility level
  t_scores <- t_scores %>% 
    dplyr::mutate(tscore = max.col(t_scores[cols_to_select])) %>% 
    dplyr::mutate(tranquility = dplyr::case_when(
      tscore == 1 ~ "Chaotic, frantic, harsh",
      tscore == 2 ~ "Busy, noisy",
      tscore == 3 ~ "Unsettled, slightly busy",
      tscore == 4 ~ "Not quite tranquil",
      tscore == 5 ~ "Just tranquil",
      tscore == 6 ~ "Fairly tanquil",
      tscore == 7 ~ "Good tranquility",
      tscore == 8 ~ "Excellent tranquility"
    ))
  
  return (t_scores)
  
}


