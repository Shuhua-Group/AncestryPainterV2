###This script is for radiation plot of genetic difference.

####Fixed parameters####
#Columns
pop_col <- 1     #The column of populations
reg_col <- 2     #The column of regions
val_col <- 3     #The column of fst values
col_col <- 4     #The column of colors

######Plotting Functions######
#The basic function of plotting radiation bars (sectors).
sector <- function(x0, y0, angle1, angle2, radius1, radius2, col, angleinc = 0.03)
{
  if (angle1 > angle2) {
    temp <- angle1
    angle1 <- angle2
    angle2 <- temp
  }
  if (radius1 > radius2) {
    temp <- radius1
    radius1 <- radius2
    radius2 <- temp
  }     ###to ensure angle1 < angle2, and radius1 < radius2
  angles <- seq(angle1, angle2, by = angleinc)
  angles[length(angles)] <- angle2
  angles <- angles*(pi/180)
  xpos <- c(cos(angles) * radius1, cos(rev(angles)) * radius2) + x0
  ypos <- c(sin(angles) * radius1, sin(rev(angles)) * radius2) + y0
  polygon(xpos, ypos, col = col, border = col)
  xmean <- cos(mean(angles)) * radius2
  ymean <- sin(mean(angles)) * radius2
  invisible(c(xmean, ymean, mean(angles)*180/pi))
}

#core of the radiation plot, inside the core the name of the target population is written.
core <- function(x0,y0,target,rstart){
  text(x0, y0, labels = target, adj = c(0.5, 0.5), cex = 1.1);
  sector(x0, y0, angle1 = -270, angle2 = 90, radius1 = rstart * 0.98, radius2 = rstart * 0.99, col = 'black')
}

#Draw marker rings to show the values
ring <- function(x0, y0, layers, rstart, flat){
  for (val in layers){
    text(x0, y0 + val * flat + rstart, labels = val, adj = c(0.5, 1.2), cex = 1.0)  #rstart=rmin: mininum radius
    sector(x0, y0, angle1 = -270, angle2 = 90, radius1 = rstart + val * flat, radius2 = rstart + val * flat, col = 'gray')
    # flat: to enlarge the difference between values
  }
}

#Draw population labels
plot_values <- function(x0, y0, data, amax, amin, rstart, show_label){
  #amin: minimum angle ( the angle of first population ) 
  #amax: maximum angle ( the angle of last population )
  npop <- nrow(data)  #total number of populations
  angelperpop <- (amax - amin)/npop  #divide the whole circle into sectors with the number equalling population 
  angpre <- amin
  offset <- angelperpop*0.4
  for (i in 1:nrow(data)){
    ang1 <- angpre + offset
    ang2 <- ang1 + angelperpop - offset
    rpre <- rstart
    rpost <- rpre + data[i,val_col]
    pmean <- sector(x0, y0, angle1 = ang1, angle2 = ang2, radius1 = rpre, radius2 = rpost, col = data[i,col_col])
    # Labels
    if(show_label){
      if(pmean[3] < -90){
         text(x0 + pmean[1], y0 + pmean[2], data[i, pop_col], srt = 180 + pmean[3], adj = c(1.1, 0.5), cex = 0.7, font = 1, col = colors()[490])
        }else{
         text(x0 + pmean[1], y0 + pmean[2], data[i, pop_col], srt = pmean[3], adj = c(-0.1, 0.5), cex = 0.7, font = 1, col = colors()[490])
      }
    }
    angpre <- ang2
  }
}

# Draw legend
plot_legend <- function(data){
  regioncolors <- unique(data[,c(reg_col, col_col)])
  legend('topright', legend = as.character(regioncolors[,1]), lwd = 5, 
         col = regioncolors[,2], cex = 1.00, bty = 'n')
}

# The main function.

#' Radiation plot
#'
#' This function draws a radiation plot to visualize the genetic distance
#'
#' @param data A four-column data frame. 1: population label (character); 2: region label (character); 3: genetic difference (numeric); 4: color code.
#' @param target A character value to pass name of the target population. Default is "target".
#' @param cenvals The coordinates of the center of the circle. Default is c(0.5, 0.5)
#' @param layers Numeric values of layers. This parameter will musk "num" if specified.
#' @param num An integer. Default is 4. Layer number.
#' @param amax Numeric. Maximal angle of radiation bars. Default is -250.
#' @param amin Numeric. Minimal angle of radiation bars. Default is 70.
#' @param border A numeric value of border. Default is 0.3.
#' @param rstart A numeric value of radius. Default is 0.02.
#' @param flat A numeric value. Flattening coefficient of radiation bars. Default is 1.2.
#' @param show_legend Logical. Whether to draw the legend of region information. Default is FALSE.
#' @param show_label Logical. Whether to print the population labels. Default is TRUE.
#' @param sorting Logical. Whether to sort population order according to their genetic difference. Default is FALSE.
#' @return NULL
#' @export
radiationplot <- function(data, target = "target", cenvals = c(0.5, 0.5), layers = NULL, num = 4, amax = -250, amin = 70, border = 0.3, rstart = 0.02, flat = 1.2, show_legend = FALSE, show_label = TRUE, sorting = FALSE){

  vals <- data[,val_col]
  if(is.null(layers)){
    layers <- round(max(vals)*seq(1/num, 1, 1/num), 2) # the layers of Fst
  }

  centre <- c(2 * cenvals[1] * border-border, 2 * cenvals[2] * border - border)
  if(sorting){
    data <- data[order(data[,val_col]), ]
  }
  data[, val_col] <- flat * data[, val_col]
  x0 = centre[1]
  y0 = centre[2]
  
  # Put a canvas
  par('oma' = c(0,0,0,0))
  par('mar' = c(2,2,2,2))
  par('xpd' = TRUE)
  plot(0, 0, xlim = c(-border, border), ylim = c(-border, border), axes = F, ann = F, type = 'n')
    
  # Draw the core showing the target
  core(x0, y0, target, rstart)
  # Draw the rings as markers of fst values
  ring(x0, y0, layers, rstart, flat)
  # Draw the sectors showing fst values
  plot_values(x0, y0, data, amax, amin, rstart, show_label)
  # Draw the legend
  if(show_legend){
    plot_legend(data = data)
  }
  # Done
}


