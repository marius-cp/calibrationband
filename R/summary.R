#' summarize isoband output
#' @param object object of class calibrationband
#' @param ... further arguments to be passed to or from methods;
#' @rdname  print.summary.calibrationband
#' @export

summary.calibrationband <- function(object, ...){

  sr <- object$cal  %>%
    dplyr::filter(out==1) %>%
    dplyr::arrange(dplyr::desc(range)) %>%
    dplyr::select(min_x,max_x)

  class(sr) <- c("summary.calibrationband", class(sr))
  return(sr)

}
