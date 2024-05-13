# AHG

# Combinations of the ancestry components
combine <- function(raw_list){
  comb_list = data.frame(combn(raw_list, 3), stringsAsFactors = FALSE) 
  return(comb_list) # A data frame. Each Column is a combination.
}

# Correlation efficient
corr <- function(x, y, z){
    corval <- abs( cor( log10(x/y), log10(z) ) )
  return(corval)
}

# Sampling of individuals
sample_func <- function(sam_range, num, prop){
  if(is.numeric(prop)){
    samples <- sample(x = 1:sam_range, size = floor(sam_range * prop), replace = FALSE)
  }else{
    samples <- sample(x = 1:sam_range, size = num, replace = FALSE)
  }
}

# Inferring admixture topology
topoInfer <- function(ancestry, ances_tuple, times, num, prop){
  for(i in 1:3){
    assign(toupper(letters)[i], ances_tuple[i])
  }  # Assign the names "A""B""C" to the variables (combinations of ancestries) 
  cor_mat <- data.frame()
  for(i in 1:times){
    samples <- sample_func(nrow(ancestry), num, prop)
    sam_mat <- ancestry[samples,]
    x <- sam_mat[, A]
    y <- sam_mat[, B]
    z <- sam_mat[, C]
    cors <- data.frame("topo1" = corr(x,y,z),
                       "topo2" = corr(z,x,y),
                       "topo3" = corr(y,z,x))
    cor_mat <- rbind(cor_mat, cors)
  }

  hit <- colnames(cor_mat)[apply(cor_mat, MARGIN = 1, FUN = which.min)]
  topo_num <- table(hit)
  topo_df <- data.frame("topo1" = c(A, B, C), 
                        "topo2" = c(C, A, B), 
                        "topo3" = c(B, C, A), stringsAsFactors = FALSE)
  topo_df <- rbind(topo_df, topo_num[c("topo1","topo2", "topo3")])
  topo_df[is.na(topo_df)] <- 0    #Transform NA values to zero
  
  colnames(cor_mat) <- c(sprintf("(%s,%s:%s)", A, B, C),
                         sprintf("(%s,%s:%s)", C, A, B),
                         sprintf("(%s,%s:%s)", B, C, A))
  
  return(list(topo_df, cor_mat))
}

#' AHG
#'
#' This function infers the admixture topology of ancestry components with supporting counts.
#'
#' @param ancestry A numeric data frame of ancestry proportion (columns: ancestry component; rows: individual).
#' @param component A character vector containing the ancestry components of which the admixture topology is about to be inferred. By default (if this parameter is not specified), all the combinations of the input ancestry components will be used.
#' @param times Bootstrap number of admixture topology.
#' @param num The sampling size. This parameter will be ignored if the parameter "prop" is specified.
#' @param prop The number of sampled individuals.
#' @return A list containing 1) possible admixture topologies with supporting counts and 2) matrices of correlation efficient between ancestry components.
#' @export
ahg <- function(ancestry, component = NULL, times = 1000, num = 10, prop = NULL){
  #ancestry <- read.table(opt$file, sep = "", header = TRUE)
  if(is.null(component)){
    ances_list <- colnames(ancestry)
    }else{
    ances_list <- component
  }
  comb_list <- combine(ances_list)
  
  logdf <- data.frame()
  correlation_list <- list()
  for(comb in comb_list){
    for(i in 1:3){
      assign(toupper(letters)[i], comb[i])
    }  # Assign the names "A""B""C" to the variables (combinations of ancestries)
    result <- topoInfer(ancestry, comb, times, num, prop)
    topo_df <- result[[1]]
    correlation <- result[[2]]
    for(topo in colnames(topo_df)){
      s <- topo_df[,topo]
      logdf <- rbind(logdf, data.frame( t( c( sprintf("%s_%s_%s", A, B, C), sprintf("(%s,%s:%s)", s[1], s[2], s[3]), s[4]) ) ) )
      #The three ancestries: A, B, C; the combination: s[1], s[2], s[3]; the hit number: s[4]
    }
    correlation_list[[sprintf("%s_%s_%s", A, B, C)]] <- correlation
  }
  colnames(logdf) <- c("combination", "topology", "number")
  cat("Done\n")
  return(list(topology = logdf, correlation = correlation_list))
}


