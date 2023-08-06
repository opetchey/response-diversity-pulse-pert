# AUC function


my_auc_func_spline <- function(x, y) {
  
  if(length(x) > 2) {
    result <- auc(x,
                  y,
                  from = min(0, na.rm = TRUE),
                  to = max(x, na.rm = TRUE),
                  type = c("spline"),
                  absolutearea = FALSE,
                  subdivisions = 1000)
  }
  
  if(length(x) < 3) {
    result <- NA
  }
  result
}


my_auc_func_raw <- function(x, y) {
  
  if(length(x) > 2) {
    result <- sum(y)
  }
  
  if(length(x) < 3) {
    result <- NA
  }
  result
}
