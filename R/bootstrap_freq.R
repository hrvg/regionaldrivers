#' Bootstrapping analysis of random occurence
#' @param df a `data.frame` that should include two columns with two groupings `class` (classification group that is held steady) and `testGrouping` (binned attribute that may explain class distribution)
#' @param bin_toggle `numeric`, optional, determines which direction to run the bootstrapping: `1` tests the likelihood from random that the class occurs preferentially or not within a testGrouping, default, likely the value to use; `2` tests the opposite direction from `1`
#' @return a list of 2: (1) The p-value of each class-testGrouping pair. If the value is less than 0.05, then the pair frequency is significantly different from random with a 95% confidence level. (2) The probability for how many testGroupings appear in each class. This should be compared with the observed number of testGroupings in each class. If the observed number is < 0.05, then the number of testGroupings in the class is significantly different from random.
#' @export
#' @keywords function
bootstrap_freq <- function(df, bin_toggle = 1) {
  names(df) <- c("class", "testGrouping")
  df$class <- as.factor(df$class)
  df$testGrouping <- as.factor(df$testGrouping)
  n_tg <- length(levels(df$testGrouping))
  n_cl <- length(levels(df$class))
  
  if (bin_toggle == 1) {
    
    in_toggle <- 2
    
    # Create final occurrences data frame
    in_occur <- data.frame(matrix(NA, nrow = n_tg, ncol = n_cl))
    in_type_num <- data.frame(matrix(NA, nrow = n_tg, ncol = n_cl))
    
    # Standardized values
    stand_val <- data.frame(matrix(NA, nrow = n_tg, ncol = n_cl))
    
  } else if (bin_toggle == 2) {
    
    in_toggle <- 1
    
    # Create final occurrences data frame
    in_occur <- data.frame(matrix(NA, nrow = n_cl, ncol = n_tg))
    in_type_num <- data.frame(matrix(NA, nrow = n_cl, ncol = n_tg))
    
    # Standardized values
    stand_val <- data.frame(matrix(NA, nrow = n_cl, ncol = n_tg))
    
  }
  
  # What are the values to sample from randomly?
  sample_vector <- as.character(df[,in_toggle])
  
  # How many random samples?
  r <- 1000
  
  b <- 4999
  # Number of samples to randomly predict (same as input)
  s <- nrow(df)
  
  row.names(in_occur) <- levels(df[,in_toggle])
  colnames(in_occur) <- levels(df[,bin_toggle])
  
  row.names(stand_val) <- levels(df[,in_toggle])
  colnames(stand_val) <- levels(df[,bin_toggle])
  
  p_val <- data.frame(stand_val)
  row.names(p_val) <- levels(df[,in_toggle])
  colnames(p_val) <- levels(df[,bin_toggle])
  
  row.names(in_type_num) <- c(1:length(levels(df[,in_toggle])))
  colnames(in_type_num) <- levels(df[,bin_toggle])
  
  # Loop through each bin (bin_toggle = 1 then class; bin_toggle = 2 then fg)
  for (i in levels(df[,bin_toggle])) {
    # Determine specific length of the class group of interest
    bin_length <- nrow(df[df[,bin_toggle]==i,])
    
    # Loop through 'r' random samples to create random data.frame
    for (j in 1:r) {
      j_rand <- sample(sample_vector, size = bin_length, replace = TRUE)
      
      if (j == 1) {
        bin_rand <- j_rand
      }  else {
        bin_rand <- cbind(bin_rand, j_rand)
      }
    }
    
    # Create data frame for each occurrences
    if (bin_toggle == 1) {
      bin_occur <- data.frame(matrix(NA, nrow = r, ncol = n_tg))
    } else if (bin_toggle == 2) {
      bin_occur <- data.frame(matrix(NA, nrow = r, ncol = n_cl))
    }
    row.names(bin_occur) <- paste0("R", c(1:r))
    colnames(bin_occur) <- levels(df[,in_toggle])
    
    # Loop through each within group class to find occurrences
    for (in_grp in levels(df[,in_toggle])){
      
      # Determine number of actual occurrences
      in_occur[in_grp,i] <- nrow(df[df[,in_toggle]==in_grp & 
                                           df[,bin_toggle]==i,])
      
      for (j in 1:r) {
        bin_occur[j,in_grp] <- length(bin_rand[bin_rand[,j]==in_grp,j])
      }
      
      # Scale the data.frame to a normal distribution around 0
      occur_scale <- as.data.frame(scale(bin_occur))
      
      # Calculate mean & SD of hydroclass
      mean_h_i <- mean(bin_occur[,in_grp])
      sd_h_i <- sd(bin_occur[,in_grp])
      
      # Standardized value calculation
      stand_val[in_grp,i] <- (in_occur[in_grp,i] - mean_h_i) / sd_h_i
      
      boot_yn <- vector(mode = "numeric", length = b)
      # Bootstrap random means
      for (k in 1:b) {
        
        boot_yn[k] <- length(which(abs(rnorm(occur_scale[,in_grp], n = s)) >=
                                     abs(stand_val[in_grp,i]))==TRUE)
      }
      
      mean_boot <- mean(boot_yn)
      
      p_val[in_grp,i] <- (mean_boot + 1) / (s + 1)
      
    }
    
    # What is the total number of testGroups in random samples?
    rand_hydro_num <- apply(bin_rand, 2, function(x) length(unique(x)))
    
    # Determine actual number of testGroups in class
    actual_hydro_num <- length(unique(
      df[df[,bin_toggle]==i,bin_toggle]))
    
    # What is the percentage of times the same number as actually occurrences?
    for (grp_num in as.numeric(row.names(in_type_num))) {
      in_type_num[grp_num, i] <- length(
        rand_hydro_num[rand_hydro_num==grp_num]) / 1000
    }
  }
  
  out_list <- list(p_val, in_type_num)
  
  return(out_list)
}
