#' FindOutliers function
#' Finds the outliers in data
#' @param x data to analyse
#' @param resp Tukey coefficient
#' @export

findOutliers <- function(x, resp = TRUE) {
  ssm = quantile(x)
  lowerq = ssm[2]
  upperq = ssm[4]
  iqr = upperq - lowerq
  threshold.upper = 0
  threshold.lower = 0
  result <- c()
  if(resp){
    threshold.upper = (iqr * 3) + upperq
    threshold.lower = lowerq - (iqr * 3)
  } else {
    threshold.upper = (iqr * 1.5) + upperq
    threshold.lower = lowerq - (iqr * 1.5)
  }
  for(i in x){
    if(i < threshold.lower || i > threshold.upper){
      result[length(result) + 1] <- i
    }
  }
  return(result)
}
