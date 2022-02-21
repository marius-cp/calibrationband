#' summarize isoband output
#' @param object object of class calibrationband
#' @param ... further arguments to be passed to or from methods;
#' @rdname  print.summary.calibrationband
#' @export

summary.calibrationband <- function(object, ...){

  sr <- object$cal  %>%
    filter(out==1) %>%
    arrange(desc(range)) %>%
    select(min_x,max_x)

  class(sr) <- c("summary.calibrationband", class(sr))
  return(sr)

}
