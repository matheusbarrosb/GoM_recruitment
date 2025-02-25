get_temp_threshold = function(temp) {
  
  threshold = quantile(temp, probs = c(.75), na.rm = TRUE)
  
  return(threshold)
  
}
