################### I. define the sector function to draw sector ############################
Sector <- function(x0 = 0, y0 = 0, angle1, angle2, radius1, radius2, col, angleinc = 0.03){
  # radius1: inner circle; radius2: outer circle
  if (angle1 > angle2) {
    temp <- angle1
    angle1 <- angle2
    angle2 <- temp
  }
  if (radius1 > radius2) {
    temp <- radius1
    radius1 <- radius2
    radius2 <- temp
  }
  ###########use 4 points polygon to draw a sector########################
  angles <- seq(angle1, angle2, by = angleinc)
  angles[length(angles)] <- angle2
  angles <- angles*(pi/180)
  xpos <- c(cos(angles) * radius1, cos(rev(angles)) * radius2) + x0
  ypos <- c(sin(angles) * radius1, sin(rev(angles)) * radius2) + y0
  polygon(xpos, ypos, col = col, border = col)
}

###########################II. draw sectors#########################################
Paint_ind_in_pop <- function(data, rmin, rmax, amax, amin, prgap, popsize, npop, popcols){
  
  angle_df <- data.frame()
  
  rstart <- rmin
  rlen <- (rmax - rmin)*(1 - prgap)/Knum            ####### set the length unit
  angelperpop <- (amax - amin)/npop
  angelperInd <- angelperpop/popsize
  angpre <- amin
  for (i in 1:nrow(data)){                  ################ for each individual
    ang1 <- angpre
    ang2 <- ang1 + angelperInd[as.character(data[i, popcol])]
    rpre <- rstart

    angle_df <- rbind(angle_df, c(ang1, ang2))

    for (j in Kstart:ncol(data)){                  ################ for each K
      rpost <- rpre + rlen*data[i, j]
      Sector(angle1 = ang1, angle2 = ang2, radius1 = rpre, radius2 = rpost, col = popcols[j - Kstart + 1])###### draw sector for each K of each individual
      rpre <- rpost
    }
    angpre <- ang2
  }
  
  return(angle_df)

}

########################III. draw lines for each pop #################################
Draw_pop_line <- function(rmin, rmax, amin, amax, npop, prgap){
  #get the line length
  rstart <- rmin + (rmax-rmin)/Knum
  lstart <- rmin
  lend <- rmax - prgap*(rmax-rmin)/Knum
  #get pop_number angles
  angles <- seq(amin, amax, length=npop+1)
  for(tmp in angles){
    px <- c(lstart*cos(tmp*pi/180), lend*cos(tmp*pi/180))
    py <- c(lstart*sin(tmp*pi/180), lend*sin(tmp*pi/180))
    lines(px, py, col='black', cex = 0.5)
  }
}

#############################IV. Draw piechart #####################################
Target_Layout <- function(tar_ang1, tar_ang2, cendis, num){
  if(tar_ang1 > tar_ang2){
    temp <- tar_ang1
    tar_ang1 <- tar_ang2
    tar_ang2 <- temp
  }
  angleinc <- (tar_ang2 - tar_ang1) / num
  angles <- seq(tar_ang1, tar_ang2, angleinc)
  xpos <- cos(angles*pi/180) * cendis
  ypos <- sin(angles*pi/180) * cendis 
  return(list(xpos, ypos)) 
}


Draw_target_pie <- function(orig, rmin, target, popcols, cendis = 1, tar_ang1 = 0, tar_ang2 = 360, arrow = FALSE, angle_df = NULL){
  num <- length(target)
  tar_R <- 0.6 
  
  if(num == 1){
    cendis <- 0  # The single target lays at the center of the plot
  }

  positions <- Target_Layout(tar_ang1 = tar_ang1, tar_ang2 = tar_ang2, cendis = cendis, num = num)
  xpos <- positions[[1]]
  ypos <- positions[[2]]

  for(i in 1:num){
    tar <- target[i]
    tar_cen <- c(xpos[i], ypos[i])
    
    data_pop <- orig[orig[,popcol] == tar,]
    data_ind <- orig[orig[,indcol] == tar,]
    data <- rbind(data_pop, data_ind)
     
    apre <- -180
    for( j in Kstart:ncol(data) ){
      val <- mean(data[,j])
      apost <- apre + 360*val
      Sector(x0 = tar_cen[1], y0 = tar_cen[2], angle1 = apre, angle2 = apost, radius1 = 0, radius2 = tar_R, col = popcols[j - Kstart + 1])
      apre <- apost
    }
    # Label of target
    text(tar_cen[1], tar_cen[2] + tar_R*1.3, tar, cex = 6, font = 2, col = colors()[490])
    
    # Arrow to target
    if(arrow){
      angle_df_cut1 <- angle_df[which(orig[,popcol] == tar),]
      angle_df_cut2 <- angle_df[which(orig[,indcol] == tar),]
      angle_df_cut <- rbind(angle_df_cut1, angle_df_cut2)
      arrow_ang <-  median(as.matrix(angle_df_cut))*pi/180  
      
      arrow_end <- c(rmin*0.95*cos(arrow_ang), rmin*0.95*sin(arrow_ang))
      vec <- arrow_end - tar_cen
      extd_ratio <- sqrt(sum(vec**2))/(tar_R*1.05)
      arrow_start <- tar_cen + vec/extd_ratio

      arrows(x0 = arrow_start[1], y0 = arrow_start[2], x1 = arrow_end[1], y1 = arrow_end[2], length = 0.25, angle = 30, code = 2, col = "red", lwd = 2)
    }
  }

}

######################### V. Write all population name############################
Write_lab <- function(ang, xx, yy, text, cex_no, text_col){
  if(ang < -90){
        text(x = xx, y = yy, labels = text, srt = 180 + ang, adj = c(1.1, 0.5), cex = cex_no, font = 1, col = text_col)
    }else{
     text(x = xx, y = yy, labels = text, srt = ang, adj = c(-0.1, 0.5), cex = cex_no, font = 1, col = text_col)
  }
}

Write_pop_lab <- function(data, amax, amin, rmax, rmin, prgap, npop, poporder, popgroup){
  angelperpop <- (amax - amin)/npop
  angpre <- amin
  lend <- rmax - prgap*(rmax-rmin)/Knum
  for(i in 1:npop){
    ang <- angpre + angelperpop/2
    xx <- lend*cos(ang*pi/180)
    yy <- lend*sin(ang*pi/180)
    cex_no = 220/npop
    if(cex_no > 6){
      cex_no=6
      }else if(cex_no < 1.5){
      cex_no=1.5
    }
    if(length(popgroup) == 0){
      text_col <- "black"
    }else{
      text_col <- popgroup[i]
    }
    text <- poporder[i]
    Write_lab(ang, xx, yy, text, cex_no, text_col)
    angpre <- angpre + angelperpop
    }    
}

####### VI. Plot the whole picture ######
Ancestry_Paint <- function(Q, ind, fout, target = NULL, poporder = NULL, popgroup = NULL, popcols = NULL,
                           rmin = 2, rmax = 3.7, amin = -265, amax = 85, tar_ang1 = 0, tar_ang2 = 360, arrow = FALSE,
                           prgap = 0.2, noline = FALSE, png = FALSE){
  #bind individual information to ancestry composition in .Q file 
  ances_df <- cbind(ind[,c(2,1)],Q)

  #sort the population order
  if(length(poporder) > 0){
     cat("Use the specified population order...\n")
     inter_poporder <- poporder[poporder %in% unique(ances_df[,popcol])] #must check whether all populations in poporder file are in the .ind file
     if(length(inter_poporder) < length(poporder)){
       warning('Some populations in the .poporder file are not present in the .ind file.')
     }
     poporder <- inter_poporder
  }else{
    cat("No population order specified. Sort the population order...\n")
    if(sort_order){
      subti_df <- aggregate(ances_df[, 3:ncol(ances_df)], by = list(ances_df[,popcol]), FUN = mean)
      rownames(subti_df) <- subti_df[, "Group.1"]
      subti_df["Group.1"] <- NULL
      pops <- unique(ances_df[,popcol])
      major_ances <- c()
      for(pop in pops){
          seq <- sort(as.numeric(subti_df[pop, ]), decreasing = T, index.return = T)$ix
          major_ances <- c(major_ances, seq[1])  # the number of major ancestry type = the number of K
      }
      names(major_ances) <- pops
      auto_pop_order <- c()
      auto_ind_order <- c()
      for(i in 1:ncol(subti_df)){
        subgroup <- names(major_ances[major_ances == i])
        # to get population order
        subgroup_df <- subti_df[subgroup,]
        subgroup_order <- rownames(subgroup_df[order(subgroup_df[, i], decreasing = T),])
        auto_pop_order <- c(auto_pop_order, subgroup_order)
        # to get individual order
        for(pop in subgroup_order){
          temp_pop_df <- ances_df[ances_df[,popcol] == pop,]
          auto_ind_order <- c(auto_ind_order, temp_pop_df[order(temp_pop_df[, i + 2], decreasing = T), indcol])
        }
      }
      poporder <- auto_pop_order
      ind_order <- auto_ind_order[auto_ind_order %in% ind[,2]]
      write.table(poporder, paste0(fout, ".auto_sort.poporder"), quote = F, sep = "\t", row.names = F, col.names = F)
      write.table(ind_order, paste0(fout, ".auto_sort.indorder"), quote = F, sep = "\t", row.names = F, col.names = F)
    }else{
      poporder <- ances_df[!duplicated(ances_df[,popcol]),][,popcol]
    }
  }
  #print(poporder) #to debug
  #print(length(poporder))
  ances_df[,popcol] <- factor(ances_df[,popcol], levels = poporder)
  ances_df <- ances_df[order(ances_df[,popcol]),]
  ances_df <- ances_df[ances_df[,popcol] %in% poporder,] #must check whether all populations are in poporder file

  
  if(sort_order){
    rownames(ances_df) <- ances_df[,indcol]
    ances_df <- ances_df[ind_order,]
    #cat("Hi!\n")
    rownames(ances_df) <- NULL
  }
  

  #Get the population size
  popsize <- table(ances_df[, popcol])
  npop <- length(popsize)
  
  #Set the colors of populations
  if(length(popcols) == 0){
    popcols <- colors()[sample(1:657, npop, replace = F)] #The length of colors is 657
  }
  
  ###Plot###
  if(drawpng){
    png(filename = paste(fout, "png",sep="."), width = 1500, height = 1500)
  }else{
    pdf(paste(fout, "pdf", sep = "."), 45, 45)  
  } #beginning of the plot
    #set the canvas
    par('oma'=c(10, 10, 10, 10))
    par('mar'=c(20, 20, 20, 20))
    par('xpd'=TRUE)
    plot(0, 0, xlim=c(-rmax, rmax), ylim=c(-rmax, rmax), axes=F, ann=F, type='n')
    #Paint ancestry of each individual in each population
    angle_df <- Paint_ind_in_pop(data = ances_df, rmin = rmin, rmax = rmax, 
                                  amax = amax, amin = amin, prgap = prgap, popsize = popsize, npop = npop,
                                  popcols = popcols)
    #Write all population labels
    Write_pop_lab(data = ances_df, amax = amax, amin = amin, 
                  rmax = rmax, rmin = rmin, prgap = prgap,
                  npop = npop, poporder = poporder, popgroup = popgroup)
    #Draw the lines
    if(!noline){
      Draw_pop_line(rmin = rmin, rmax = rmax, amin = amin, amax = amax, npop = npop, prgap = prgap)
    }
    #Draw the pie chart of the target population
    if(length(target) > 0){
      Draw_target_pie(orig = ances_df, rmin = rmin, target = target, tar_ang1 = tar_ang1, tar_ang2 = tar_ang2, popcols = popcols, arrow = arrow, angle_df = angle_df)
    }
    
  dev.off() #The end of the plot
}

########Parameters###########
library("optparse")

option_list <- list(
  make_option(c("-q", "--qfile"), type = "character",  action = "store", 
              help = "(required, input file) Define the individual Q-matrix file. 
              Check ./example/data.Q for input format."),
  make_option(c("-i", "--indfile"), type = "character",  action = "store", 
              help = "(required, input file) Define the population information of each individual. Check ./example/data.ind for input format."),
  make_option(c("-t", "--target"), type = "character", default = NULL,action = "store", 
              help = "(optional, population name) Define the only target population to be plotted as a pie chart in the center of the circle figure. 
              The population must be included in both of the required files."),
  make_option(c("-p","--poporder"), type = "character", default = NULL, action = "store", 
              help = "(optional, input files) Can be one or two columns. Define the populations (1st column) and their groups (2nd column) to be included in the figure, 
              and the display order of the populations in the figure. Check ./example/pop.order for input format."),
  make_option(c("-c","--color"), type = "character",  default = NULL, action = "store", 
              help = "(optional, input files) Define the colors of each ancestry in the figure. 
              Check ./example/color.file for input format."),
  make_option(c("--arrow"), type = "character",  default = FALSE, action = "store_true",
              help = "(optional) Whether to draw the arrows to the target pies."),
  make_option("--tar_ang1", type = "integer", default = 0, action = "store", 
              help = "(optional) The start angle of the target leyout."),
  make_option("--tar_ang2", type = "integer", default = 360, action = "store", 
              help = "(optional) The end angle of the target leyout."),
  make_option(c("--noline"), type = "logical", default = FALSE, action = "store_true", 
              help = "(optional) remove the black lines between populations. Default is keeping black lines"),
  make_option(c("--sort_order"), type = "logical", default = FALSE, action = "store_true",
              help = '(optional) automatically sort the order of the populations. useless when "--poporder" is specified'),
  make_option(c("-f","--png"), type = "logical", default = FALSE, action = "store_true", 
              help = "(optional) output png figure instead of pdf figure. Default is pdf figure."),
  make_option(c("-o", "--out"), type = "character", default = "out", action = "store", 
              help = "(optional) The prefix of output file.")
)

opt = parse_args(OptionParser(option_list = option_list, usage = "R script for Fst radiation plot."))

##########Data_input###########
Q <- read.table(opt$qfile, sep = '', header = F, stringsAsFactors = F)
ind <- read.table(opt$indfile, sep = '', header = F, stringsAsFactors = F)
target <- opt$target
if(length(target) > 0) target <- strsplit(opt$target,",")[[1]]

if(length(opt$poporder) > 0){
  poporder <- read.table(opt$poporder, stringsAsFactors = F,sep = '\t', header = F)$V1
  popgroup <- read.table(opt$poporder, stringsAsFactors = F,sep = '\t', header = F)$V2
}else{
  poporder <- opt$poporder
  popgroup <- opt$poporder
}

if(length(opt$color) > 0){
  popcols <- read.table(opt$color, sep = '\t', stringsAsFactors = F, header = F, comment.char = "")$V1
}else{
  popcols <- opt$color
}

fout <- opt$out
noline <- opt$noline
drawpng <- opt$png
sort_order <- opt$sort_order
arrow <- opt$arrow
tar_ang1 <- opt$tar_ang1
tar_ang2 <- opt$tar_ang2

######### Fixed parameters ########
indcol <- 1
popcol <- 2
Kstart <- 3
Knum <- 1

######### Plot #########
Ancestry_Paint(Q = Q, ind = ind, fout = fout, target = target, poporder = poporder, popgroup = popgroup, popcols = popcols,
               rmin = 2, rmax = 3.7, amin = -265, amax = 85, prgap = 0.2, tar_ang1 = tar_ang1, tar_ang2 = tar_ang2,  
               noline = noline, png = drawpng, arrow = arrow)
