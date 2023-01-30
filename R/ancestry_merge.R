#######################                match               #######################
compare_files <- function(ref_data, target_ancestry_file_name, K){
  col_match <- c() ## pairs of ancestry components
  ancref <- ref_data
  rownames(ancref) <- ancref$V1
  anctar <- read.table(target_ancestry_file_name, header = F)
  
  rownames(anctar) <- anctar$V1
  overlapsample <- intersect(rownames(ancref), rownames(anctar))
  
  ## correlation between th ancestry components
  for( col1 in colnames(ancref)[3:ncol(ancref)] ){
    largest_cor <- 0.0
    second_cor <- 0.0
    for( col2 in colnames(anctar)[3:ncol(ancref)]){
      corr <- cor(x = ancref[overlapsample,col1], y = anctar[overlapsample,col2], method = "pearson")
      if( corr > largest_cor ){
        second_cor <- largest_cor
        largest_cor <- corr
        col_match[col1] <- col2
      }else if( (corr <= largest_cor) && (corr > second_cor) ){
        second_cor <- corr
      }
    } 
    if ( (abs(largest_cor) - abs(second_cor)) < 0.5 ){
      col_match <- col_match[-which(names(col_match) == col1)]
    }    
  }

  if( length( unique(names(col_match)) ) > length( unique(col_match)) ){
    multi_col_match <- c()
    combs <- combn(names(col_match), 2)
    for( j in 1:ncol(combs) ){
      if( col_match[combs[1, j]] == col_match[combs[2, j]] ) multi_col_match <- c(multi_col_match, combs[1, j], combs[2, j])
    }
    multi_col_match <- unique(multi_col_match)
    col_match_new <- c()
    for( i in names(col_match)) if( !i %in% multi_col_match ) col_match_new[i] <- col_match[i]
    col_match <- col_match_new
    rm(col_match_new)
  }

  if( length(col_match) == K ){
    anctar <- anctar[, c( colnames(anctar)[1:2], col_match[ colnames(ancref)[3:ncol(ancref)] ] ) ] 
    colnames(anctar) <- colnames(ancref)
    consensus_filelist <<- c(consensus_filelist, target_ancestry_file_name)
    return(anctar)  # anctar is a data frame
  }else{
    conflict_filelist <<- c(conflict_filelist, target_ancestry_file_name)
    return(col_match)  # col_match is a vector
  }

}
 
#######################                 merge              #######################
merge_files <- function(ref_data, target_ancestry_file_list, K){
  n <- length(target_ancestry_file_list)
  result <- lapply(target_ancestry_file_list, FUN = compare_files, ref_data = ref_data, K = K) # result is a list
  
  ## estimate supporting ratio for each component
  matching <- list()
  for(x in result) if(is.vector(x)) matching <- append(matching, list(x)) 
  matching_names <- c()
  for(k in matching) matching_names <- c(matching_names, names(k))
  counting <- c(matching_names,
                rep(paste0("V", seq(3, K + 2, 1)), times = ( length(target_ancestry_file_list) - length(matching)) ) ) # the columns of the data frame are "V1" "V2" "V3" ..
  counting <- data.frame(table(counting)) 
  counting['ratio'] <- counting['Freq']*1.0 / length(target_ancestry_file_list)
  rownames(counting) <- counting$counting
  counting <- subset(counting, select = - counting)
  
  ## merge consensus ancestry file
  merging <- data.frame()
  for(y in result) if(is.data.frame(y)) merging <- rbind(merging, y)
  merging <- aggregate(merging[, 3:ncol(merging)], by = list(merging[,1], merging[,2]), FUN = mean)    
  return(list(counting, merging)) 
}

####################### sorting #######################
#' ancmerge
#'
#' This function draws a radiation plot to visualize the genetic distance
#'
#' @param tar_anc_filelist Character. Required. ancestry file names (recomended format: prefix.K.ancestry). The ancestry matrix file should be(2 + K) columns without header. The columns: 1st-ndividual ID; 2nd-Group ID. From the 3rd column, it indicates the ancestry proportion.
#' @param ref Character. Required. The reference ancestry matrix to be matched.
#' @param K Integer. Required. The number of the ancestry components.
#' @param poporder Character. (optional, input files) Population order list.
#' @return A list containing the merged ancestry matrix, the supporting ratio of ancestry components, and the input ancestry matrices that match/do not match the reference. 
#' @export
ancmerge <- function(tar_anc_filelist, ref, K, poporder = NULL){

  cat(c("Time:", as.character(Sys.time()), "Path:", getwd()), sep = "\n")
 
  ref_data <- read.table(ref, sep = "", header = FALSE)
  
  consensus_filelist <<- c()
  conflict_filelist <<- c()

  tmp_out <- merge_files(ref_data, tar_anc_filelist, K)
  counts <- tmp_out[[1]]; merging <- tmp_out[[2]]; rm(tmp_out)
  
  ## sorting populations
  if(is.null(poporder)){
    poplist <- unique(ref_data[,2])
  }else{
    poplist <- poporder
  }
  merging[,2] <- factor(merging[,2], levels = poplist)
  merging <- merging[order(merging[,2]), ]
  
  ## sorting individuals within each population
  remerging <- data.frame()
  for( pop in poplist){
    temp <- merging[merging[,2] == pop, ]
    mean <- as.numeric(apply(temp[, 3:ncol(temp)], MARGIN = 2, FUN = mean))
    sorting <- order(mean) + 2
    sorting <- sorting[length(sorting):1]
    temp <- temp[do.call("order", temp[, sorting]), ]
    remerging <- rbind(remerging, temp); rownames(remerging) <- NULL
  }
  merging <- remerging

  ## sorting components
  popmean <- aggregate(merging[, 3:ncol(merging)], by = list(merging[, 2]), FUN = mean);
  rownames(popmean) <- popmean[, "Group.1"]
  popmean <- subset(popmean, select = - Group.1)
  resorting <- data.frame()
  for(comp in colnames(popmean)){
    resorting[comp, 'index'] <- which(popmean[,comp] == max(popmean[, comp]))[1]   # Return to the index of the first maximal value
  }
  
  resorting$component <- rownames(resorting)
  resorting$pop <- rownames(popmean[resorting$index, ])
  resorting <- resorting[order(resorting[, 'index']), ]
  resortlist <- resorting[,'component']
  merging <- merging[, c(colnames(merging)[1:2], resortlist)]
  colnames(merging) <- c(c(1, 2), 3:(K + 2))
  
  ## update supporting ratio
  resorting$count <- counts[resorting$component, 'Freq']
  resorting$ratio <- counts[resorting$component, 'ratio']
  resorting$component <- paste0('comp', 1:K)
  resorting <- subset(resorting, select = - index)
  colnames(resorting) <- c('component','represent_pop','support_counts','support_ratio')
  rownames(resorting) <- NULL 
 
  ## finish
  cat('Done.\n')

  return(list(merged_ancestry = merging, supporting_ratio = resorting, consensus_filelist = consensus_filelist, conflict_filelist = conflict_filelist))
}

