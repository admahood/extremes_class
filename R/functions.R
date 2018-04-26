

st_par <- function(sf_df, sf_func, n_cores, ...){
  
  # http://www.spatialanalytics.co.nz/post/2017/09/11/a-parallel-function-for-spatial-analysis-in-r/
  
  # Paralise any simple features analysis.
  
  # Create a vector to split the data set up by.
  
  split_vector <- rep(1:n_cores, each = nrow(sf_df) / n_cores, length.out = nrow(sf_df))
  
  
  # Perform GIS analysis
  
  split_results <- split(sf_df, split_vector) %>%
    
    mclapply(function(x) sf_func(x, ...), mc.cores = n_cores)
  
  
  # Combine results back together. Method of combining depends on the output from the function.
  
  if (class(split_results[[1]]) == 'list' ){
    
    result <- do.call(c, split_results)
    
    names(result) <- NULL
    
  } else {
    
    result <- do.call(rbind, split_results)
    
  }
  
  
  # Return result
  
  return(result)
  
}
