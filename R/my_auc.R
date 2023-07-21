# AUC function


my_auc_func <- function(x, y) {
  
  if(length(x) > 2) {
    result <- auc(x,
                  y,
                  from = min(0, na.rm = TRUE),
                  to = max(x, na.rm = TRUE),
                  type = c("spline"),
                  absolutearea = FALSE)
  }
  
  if(length(x) < 3) {
    result <- NA
  }
  result
}
