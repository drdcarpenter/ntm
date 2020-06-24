#' @title ntm_score
#' @description calculates a tranquility score from four inputs
#' @param NAMM a NAMM score between 1-5
#' @param PONS a PONS score, the percentage of natural sound
#' @param Lrr a road and rail score in decibels, 15-90 dB
#' @param Lat a corrected overall level, in decibels, 15-90dB
#' @return a tranquility description as a string
#' @details each argument must be an integer
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE
#'  ntm_score(5, 97, 15, 48)
#'  }
#' }
#' @seealso 
#'  \code{\link[tibble]{as_tibble}}
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{case_when}}
#' @rdname ntm_score
#' @export 
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate case_when
ntm_score <- function(NAMM, PONS, Lrr, Lat){
  l <- list(NAMM = NAMM, PONS = PONS, Lrr = Lrr, Lat = Lat)
  df <- tibble::as_tibble(l)
  
  df <- df %>% 
    dplyr::mutate(
      ts1 = 0,
      ts2 = (74.17398 + (NAMM * 9.57158) + (PONS * 5.32) + (Lrr * 0.08640) - (Lat * 1.21115)),
      ts3 = (114.46581 + (NAMM * 10.93007) + (PONS * 5.27272) + (Lrr * 0.08981) - (Lat * 1.85779)),
      ts4 = (129.58104 + (NAMM * 11.55970) + (PONS * 5.33385) + (Lrr * 0.13029) - (Lat * 2.17490)),
      ts5 = (133.98827 + (NAMM * 12.81092) + (PONS * 5.35484) + (Lrr * 0.12512) - (Lat * 2.32374)),
      ts6 = (136.05294 + (NAMM * 14.11910) + (PONS * 5.37543) + (Lrr * 0.11841) - (Lat * 2.46001)),
      ts7 = (132.75350 + (NAMM * 16.44831) + (PONS * 5.38689) + (Lrr * 0.05909) - (Lat * 2.56049)),
      ts8 = (116.06068 + (NAMM * 19.41205) + (PONS * 5.45928) + (Lrr * 0.08844) - (Lat * 2.57928))
      )
  # set up columns to select max score
  cols_to_select <- c("ts1", "ts2", "ts3", "ts4", "ts5", "ts6", "ts7", "ts8")
  
  # select max score and assign tranquility level
  df <- df %>% 
    dplyr::mutate(tscore = max.col(df[cols_to_select])) %>% 
    dplyr::mutate(tranquility = dplyr::case_when(
      tscore == 1 ~ "Chaotic, frantic, harsh",
      tscore == 2 ~ "Busy, noisy",
      tscore == 3 ~ "Unsettled, slightly busy",
      tscore == 4 ~ "Not quite tranquil",
      tscore == 5 ~ "Just tranquil",
      tscore == 6 ~ "Fairly tranquil",
      tscore == 7 ~ "Good tranquility",
      tscore == 8 ~ "Excellent tranquility"
    ))
  
  tranquility <- df$tranquility
  
  return(tranquility)
  
}
