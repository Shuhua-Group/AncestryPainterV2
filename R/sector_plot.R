######### Fixed parameters ########
indcol <- 1
popcol <- 2
Kstart <- 3
Knum <- 1

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
  graphics::polygon(xpos, ypos, col = col, border = col)
}

############ II. Coerse the length of attributes to match target items###################
coercion <- function(x, len){
  #x: the vector to be coerced; len: the length of the target item.
  len_x <- length(x)
  if(len_x < len){
    if(len %% len_x == 0){
      z <- rep(x, times = len %/% len_x)
    }else{
      z <- c(rep(x, times = len %/% len_x), x[1:(len %% len_x)])
    }
    #warnings("")
  }else if(len_x == len){
    z <- x
  }else{ 
    z <- x[1:len]
    #warnings("")
  }
  return(z)  
}

###########################III. draw sectors#########################################
Paint_ind_in_pop <- function(data, rmin, rmax, amax, amin, prgap, popsize, npop, ancescols){
  
  angle_df <- data.frame()
  
  rstart <- rmin
  rlen <- (rmax - rmin) * (1 - prgap)/Knum            ####### set the length unit
  angelperpop <- (amax - amin)/npop
  angelperInd <- angelperpop/popsize
  angpre <- amin
  for (i in 1:nrow(data)){                  ################ for each individual
    ang1 <- angpre
    ang2 <- ang1 + angelperInd[as.character(data[i, popcol])]
    rpre <- rstart

    angle_df <- rbind(angle_df, c(ang1, ang2))

    for (j in Kstart:ncol(data)){                  ################ for each K
      rpost <- rpre + rlen * data[i, j]
      Sector(angle1 = ang1, angle2 = ang2, radius1 = rpre, radius2 = rpost, col = ancescols[j - Kstart + 1])###### draw sector for each K of each individual
      rpre <- rpost
    }
    angpre <- ang2
  }
  
  return(angle_df)

}

########################IV. draw lines for each pop #################################
Draw_pop_line <- function(rmin, rmax, amin, amax, npop, prgap){
  #get the line length
  rstart <- rmin + (rmax-rmin)/Knum
  lstart <- rmin
  lend <- rmax - prgap*(rmax-rmin)/Knum
  #get pop_number angles
  angles <- seq(amin, amax, length = npop+1)
  for(tmp in angles){
    px <- c(lstart*cos(tmp*pi/180), lend*cos(tmp*pi/180))
    py <- c(lstart*sin(tmp*pi/180), lend*sin(tmp*pi/180))
    graphics::lines(px, py, col='black', cex = 0.5)
  }
}

#############################V. Draw piechart #####################################
Target_Layout <- function(tarang1, tarang2, cendis, num){
  if(tarang1 > tarang2){
    temp <- tarang1
    tarang1 <- tarang2
    tarang2 <- temp
  }
  if(num > 1){
    angleinc <- (tarang2 - tarang1) / (num - 1)
    angles <- seq(tarang1, tarang2, angleinc)
    xpos <- cos(angles * pi/180) * cendis
    ypos <- sin(angles * pi/180) * cendis
  }
  else{
    angles <- 0
    xpos <- cos(angles * pi/180) * cendis
    ypos <- sin(angles * pi/180) * cendis
  }
  return(list(xpos, ypos)) 
}

Target_arrow <- function(orig, rmin, tar.r, angle_df, tar_cen, tar, arrow.col = "red", arrow.lwd = 2){
  angle_df_cut1 <- angle_df[which(orig[, popcol] == tar),]
  angle_df_cut2 <- angle_df[which(orig[, indcol] == tar),]
  angle_df_cut <- rbind(angle_df_cut1, angle_df_cut2)
  arrow_ang <-  stats::median(as.matrix(angle_df_cut))*pi/180

  arrow_end <- c(rmin * 0.95 * cos(arrow_ang), rmin * 0.95 * sin(arrow_ang))
  vec <- arrow_end - tar_cen
  extd_ratio <- sqrt(sum(vec ** 2))/(tar.r * 1.05)
  arrow_start <- tar_cen + vec/extd_ratio

  graphics::arrows(x0 = arrow_start[1], y0 = arrow_start[2], x1 = arrow_end[1], y1 = arrow_end[2], length = 0.25, angle = 30, code = 2, col = arrow.col, lwd = arrow.lwd)
  # Done.
}


Draw_target_pie <- function(orig, rmin, target, ancescols, tar.r = 0.6, tarangs = NULL, cendis = 1, tarang1 = 0, tarang2 = 180, arrow = FALSE, angle_df = NULL, tar.lab.cex = 6, tar.lab.col = "navy", arrow.col = "red", arrow.lwd = 2){
  num <- length(target)

  tar.r <- coercion(tar.r, num) # The vector of target radius is coerced to match the target number.
  cendis <- coercion(cendis, num)

  if(sum(cendis + tar.r >= rmin) > 0){
    warnings("Your target pie charts may exceed the inner ring.")
  }
  
  if(is.null(tarangs)){
    positions <- Target_Layout(tarang1 = tarang1, tarang2 = tarang2, cendis = cendis, num = num)
    xpos <- positions[[1]]
    ypos <- positions[[2]]
  }else{
    tarangs <- coercion(tarangs, num)
    xpos <- cendis * cos(tarangs * pi/180)
    ypos <- cendis * sin(tarangs * pi/180)
  }
  
  for(i in 1:num){
    tar <- target[i]
    tar_cen <- c(xpos[i], ypos[i])

    data_pop <- orig[orig[, popcol] == tar, ]
    data_ind <- orig[orig[, indcol] == tar, ]
    data <- rbind(data_pop, data_ind)

    apre <- -180
    for( j in Kstart:ncol(data) ){
      val <- mean(data[,j])
      apost <- apre + 360*val
      Sector(x0 = tar_cen[1], y0 = tar_cen[2], angle1 = apre, angle2 = apost, radius1 = 0, radius2 = tar.r[i], col = ancescols[j - Kstart + 1])
      apre <- apost
    }

    # Label of target
    graphics::text(tar_cen[1], tar_cen[2] + tar.r[i]*1.3, tar, cex = tar.lab.cex, font = 2, col = tar.lab.col)

    # Arrow to target
    if(arrow){
      Target_arrow(orig = orig, rmin = rmin, tar.r = tar.r[i], angle_df = angle_df, tar = tar, tar_cen = tar_cen, arrow.col = arrow.col, arrow.lwd = arrow.lwd)
    }

  }
  # Done.
}

######################### VI. Write all population name############################
Write_lab <- function(ang, xx, yy, text, cex_no, text_col, text_font){
  if(ang < -90){
     graphics::text(x = xx, y = yy, labels = text, srt = 180 + ang, adj = c(1.1, 0.5), cex = cex_no, font = text_font, col = text_col)
    }else{
     graphics::text(x = xx, y = yy, labels = text, srt = ang, adj = c(-0.1, 0.5), cex = cex_no, font = text_font, col = text_col)
  }
}

Write_pop_lab <- function(amax, amin, rmax, rmin, prgap, npop, poporder, pop.lab.col, pop.lab.font, pop.lab.cex){
  angelperpop <- (amax - amin)/npop
  angpre <- amin
  lend <- rmax - prgap * (rmax - rmin)/Knum

  # Text size
  if(is.null(pop.lab.cex)){
    # To get a proper text size if it is not specified.
    cex_no <- 220/npop
    if(cex_no >= 6){
      cex_no <- rep(6, npop)
    }else if(cex_no <= 1.5){
      cex_no <- rep(1.5, npop)
    }else{
      cex_no <- rep(cex_no, npop)
    }
  }else{
    cex_no <- coercion(pop.lab.cex, npop)
  }
  
  # Text color
  text_col <- coercion(pop.lab.col, npop)

  # Text font
  text_font <- coercion(pop.lab.font, npop)
  
  for(i in 1:npop){
    ang <- angpre + angelperpop/2
    xx <- lend*cos(ang * pi/180)
    yy <- lend*sin(ang * pi/180)
    
    text <- poporder[i]
    Write_lab(ang, xx, yy, text, cex_no = cex_no[i], text_col = text_col[i], text_font = text_font[i])
    angpre <- angpre + angelperpop
  }
  # Done.    
}


####### VI. Draw legend ########
Draw_ances_legend <- function(ancescols, ancesnames, legend.pos = "topright"){
  if(is.null(ancesnames)){
    ancesnames <- paste("Ancestry", 1:length(ancescols), sep = "_")
  }
  graphics::legend(legend.pos, legend = ancesnames, lwd = 5, col = ancescols, cex = 2.00, bty = 'n')
  # Done.
}

####### VII. Plot the whole picture ######

#' Sector plot
#'
#' This function draws a sector plot to visualize ancestry proportion.
#'
#' @param Q A numeric data frame of ancestry proportion (columns: ancestry component; rows: individual). e.g., an output ".Q" file of the software ADMIXTURE.
#' @param ind A two-column data frame (1: population; 2: individual).
#' @param target Character. The target populations to be plotted as a pie chart in the center of the circle figure. The population must be included in input "ind" and "Q" data frame.
#' @param poporder Character. The populations to be included in the figure, also the display order of the populations in the figure.
#' @param ancescols The color code of each ancestry component in the figure.
#' @param sorting A logical value to define whether to sort the order of the populations, which will be masked if "poporder" is specified.
#' @param rmin The radium of the inner ring. Default is 2.
#' @param rmax The radium of the outer ring. Default is 3.7.
#' @param tar.r A numeric vector. The radius of the target pie chart. Default: 0.6.
#' @param cendis A numeric vector. The distance from the center of a target pie chart to the center of the sectorplot. Default: 1.
#' @param amin The angle at which the ring is initiated. Default is -265.
#' @param amax The angle at which the ring is ended. Default is 85.
#' @param tarangs A numeric vector. The angles of the target pie charts.
#' @param tarang1 The start angle of the target layout. Default is 0.
#' @param tarang2 The end angle of the target layout. Default is 360.
#' @param arrow Logical. Whether to draw the arrows to the target pies.
#' @param legend_mode Logical. Whether to draw the legend of ancestry components.
#' @param ancesnames Character. To specify the names of ancestry components. If not specified, would be shown as "Ancestry_1" "Ancestry_2" and so on.
#' @param prgap A numeric value for gap length. Default is 0.2.
#' @param noline Logical. Whether to remove the black lines between populations. Default is FALSE.
#' @param pop.lab.cex The cex of the population labels.
#' @param pop.lab.col The color of population labels. The length of this parameter can be 1 or equal to the number of the populations.Default: "black".
#' @param pop.lab.font The font of the population labels.
#' @param tar.lab.cex The cex of the target label.
#' @param tar.lab.col The color of the target label. Default: "navy".
#' @param arrow.col The color of the arrow(s). Default: "red".
#' @param arrow.lwd The line width of the arrow(s). Default: 2.
#' @param legend.pos The position of the legend. Can be set as "top", "topleft", "topright", "bottom", "bottomleft", "bottomright", "left", and "right".
#' @return NULL
#' @export
sectorplot <- function(Q, ind, target = NULL, poporder = NULL, ancescols = NULL, sorting = FALSE,
                       rmin = 2, rmax = 3.7, tar.r = 0.6, tarangs = NULL, cendis = 1, amin = -265, amax = 85, tarang1 = 0, tarang2 = 180, 
                       arrow = FALSE, legend_mode = FALSE, ancesnames = NULL, prgap = 0.2, noline = FALSE, 
                       pop.lab.cex = NULL, pop.lab.col = "black", pop.lab.font = 1,
                       tar.lab.cex = 6, tar.lab.col = "navy", arrow.col = "red", arrow.lwd = 2,
                       legend.pos = "topright"){

  #bind individual information to ancestry composition in .Q file 
  ances_df <- cbind(ind[,c(2,1)], Q)

  #sort the population order
  if(length(poporder) > 0){
     cat("Use the specified population order...\n")
     inter_poporder <- poporder[poporder %in% unique(ances_df[, popcol])] #must check whether all populations in poporder file are in the .ind file
     if(length(inter_poporder) < length(poporder)){
       warning('Some populations in the .poporder file are not present in the .ind file.')
     }
     poporder <- inter_poporder
  }else{
    cat("No population order specified.\n")
    if(sorting){
      cat("Sort the population order...\n")
      subti_df <- stats::aggregate(ances_df[, 3:ncol(ances_df)], by = list(ances_df[,popcol]), FUN = mean)
      rownames(subti_df) <- subti_df[, "Group.1"]
      subti_df["Group.1"] <- NULL
      pops <- unique(ances_df[,popcol])
      major_ances <- c()
      for(pop in pops){
          seq <- sort(as.numeric(subti_df[pop, ]), decreasing = T, index.return = T)$ix
          major_ances <- c(major_ances, seq[1])  # the number of major ancestries = the number of K
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
    }else{
      poporder <- ances_df[!duplicated(ances_df[, popcol]),][, popcol]
    }
  }
  #print(poporder)
  
  ances_df[,popcol] <- factor(ances_df[,popcol], levels = poporder)
  ances_df <- ances_df[order(ances_df[,popcol]),]
  ances_df <- ances_df[ances_df[,popcol] %in% poporder,] #must check whether all populations are in poporder file
  
  if(sorting){
    rownames(ances_df) <- ances_df[,indcol]
    ances_df <- ances_df[ind_order,]
    rownames(ances_df) <- NULL
  }
  
  # Get the population size
  popsize <- table(ances_df[, popcol])
  npop <- length(popsize)
  
  # Set the colors of ancestry components
  if(length(ancescols) == 0){
    ancescols <- grDevices::colors()[sample(1:657, npop, replace = FALSE)] #The length of colors is 657
  }

  # Set the attributes of the population label
  pre_poporder <- ind[, 1][!duplicated(ind[, 1])]
  
  if(length(pop.lab.col) > 1){
    names(pop.lab.col) <- pre_poporder[1:length(pop.lab.col)]
    pop.lab.col <- pop.lab.col[poporder] 
 }
 
  if(length(pop.lab.cex) > 1){
    names(pop.lab.cex) <- pre_poporder[1:length(pop.lab.cex)]
    pop.lab.cex <- pop.lab.cex[poporder]
  }

  if(length(pop.lab.font) > 1){
    names(pop.lab.font) <- pre_poporder[1:length(pop.lab.font)]
    pop.lab.font <- pop.lab.font[poporder]
  }
  
  ###Plot###
  #beginning of the plot
  #set the canvas
  graphics::par('oma'=c(10, 10, 10, 10))
  graphics::par('mar'=c(20, 20, 20, 20))
  graphics::par('xpd'=TRUE)
  graphics::plot(0, 0, xlim=c(-rmax, rmax), ylim=c(-rmax, rmax), axes=F, ann=F, type='n')

  #Paint ancestry of each individual in each population
  angle_df <- Paint_ind_in_pop(data = ances_df, rmin = rmin, rmax = rmax, 
                               amax = amax, amin = amin, prgap = prgap, popsize = popsize, npop = npop,
                               ancescols = ancescols)

  #Write all population labels
  Write_pop_lab(amax = amax, amin = amin, rmax = rmax, rmin = rmin, prgap = prgap,
                npop = npop, poporder = poporder, pop.lab.col = pop.lab.col, pop.lab.cex = pop.lab.cex, pop.lab.font = pop.lab.font)

  #Draw the lines
  if(!noline){
    Draw_pop_line(rmin = rmin, rmax = rmax, amin = amin, amax = amax, npop = npop, prgap = prgap)
  }

  #Draw the pie chart of the target population
  if(length(target) > 0){
    Draw_target_pie(orig = ances_df, rmin = rmin, target = target, tar.r = tar.r, tarangs = tarangs, cendis = cendis, tarang1 = tarang1, tarang2 = tarang2, ancescols = ancescols, arrow = arrow, angle_df = angle_df, tar.lab.cex = tar.lab.cex, tar.lab.col = tar.lab.col, arrow.col = arrow.col, arrow.lwd = arrow.lwd)
  }

  #Draw the legend showing the names of ancestry components 
  if(legend_mode){
    Draw_ances_legend(ancescols = ancescols, ancesnames = ancesnames, legend.pos = legend.pos)
  }

  #The end of the plot
  invisible(ances_df)
}


