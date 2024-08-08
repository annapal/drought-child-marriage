
calculate_te <- function(mod, data) {
  
  # Calculate main TE
  result <- slopes(
    mod,
    newdata = subset(data, drought2==1), # Only region-years exposed to drought
    variables = "drought2",
    by = "drought2",
    wts = "Denorm_Wt"
  )
  result$iso <- iso # Append iso code
  
  # Calculate TE by rural-urban status
  result2 <- slopes(
    mod, 
    newdata = subset(data, drought2==1), # Only region-years exposed to drought
    variables = "drought2",
    by = "rural",
    wts = "Denorm_Wt"
  )
  result2$iso <- iso # Append iso code
  
  # Calculate TE by years of consecutive exposure to drought
  if (nrow(subset(data, drought_3yr==1))>0) {
    result3 <- slopes(
      mod, 
      newdata = subset(data, drought_3yr==1), # Only region-years exposed to 3 years of drought
      variables = "drought2",
      by = "drought_yr",
      wts = "Denorm_Wt"
    )
    result3$iso <- iso # Append iso code
  } else {
    result3 <- NA
  }
  
  # Return results
  list(result, result2, result3)
}
