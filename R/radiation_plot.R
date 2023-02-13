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
  graphics::polygon(xpos, ypos, col = col, border = col)
  xmean <- cos(mean(angles)) * radius2
  ymean <- sin(mean(angles)) * radius2
  invisible(c(xmean, ymean, mean(angles)*180/pi))
}

#core of the radiation plot, inside the core the name of the target population is written.
core <- function(x0, y0, target, rstart, tar.lab.col = "black", tar.lab.cex = 1.1, tar.lab.font = 1, core.line.col = 'black'){
  graphics::text(x0, y0, labels = target, adj = c(0.5, 0.5), cex = tar.lab.cex, col = tar.lab.col, font = tar.lab.font);
  sector(x0, y0, angle1 = -270, angle2 = 90, radius1 = rstart * 0.98, radius2 = rstart * 0.99, col = core.line.col)
}

#Draw marker rings to show the values
ring <- function(x0, y0, layers, rstart, flat, ring.text.col = "black", ring.text.cex = 1.0, ring.text.font = 1, ring.line.col = "gray", digits = 2){
  for (val in layers){
    fmt <- paste0("%0.", digits, "f")
    graphics::text(x0, y0 + val * flat + rstart, labels = sprintf(fmt, val), adj = c(0.5, 1.2), col = ring.text.col, cex = ring.text.cex, font = ring.text.font)  #rstart = rmin: mininum radius
    sector(x0, y0, angle1 = -270, angle2 = 90, radius1 = rstart + val * flat, radius2 = rstart + val * flat, col = ring.line.col)
    # flat: to enlarge the difference between values
  }
}

#Draw population labels
plot_values <- function(x0, y0, data, amax, amin, rstart, label_mode, pop.lab.cex = 0.7, pop.lab.font = 1, pop.lab.col = "navy"){
  #amin: minimum angle ( the angle of first population ) 
  #amax: maximum angle ( the angle of last population )
  
  npop <- nrow(data)  #total number of populations
  angelperpop <- (amax - amin) / npop  #divide the whole circle into sectors with the number equalling population 
  angpre <- amin
  offset <- angelperpop * 0.4
  for (i in 1:nrow(data)){
    ang1 <- angpre + offset
    ang2 <- ang1 + angelperpop - offset
    rpre <- rstart
    rpost <- rpre + data[i,val_col]
    pmean <- sector(x0, y0, angle1 = ang1, angle2 = ang2, radius1 = rpre, radius2 = rpost, col = data[i,col_col])
    
    # Labels
    if(label_mode){
      if(pmean[3] < -90){
         graphics::text(x0 + pmean[1], y0 + pmean[2], data[i, pop_col], srt = 180 + pmean[3], adj = c(1.1, 0.5), cex = pop.lab.cex, font = pop.lab.font, col = pop.lab.col)
        }else{
         graphics::text(x0 + pmean[1], y0 + pmean[2], data[i, pop_col], srt = pmean[3], adj = c(-0.1, 0.5), cex = pop.lab.cex, font = pop.lab.font, col = pop.lab.col)
      }
    }
    angpre <- ang2
  }
}

# Draw legend
plot_legend <- function(data, legend.pos = "topright", legend.lwd = 5, legend.text.cex = 1.00){
  graphics::legend(legend.pos, legend = as.character(data[, 1]), lwd = legend.lwd, col = data[,2], cex = legend.text.cex, bty = 'n')
}

# The main function.

#' Radiation plot
#'
#' This function draws a radiation plot to visualize the genetic distance
#'
#' @param data A four-column data frame. 1: population label (character); 2: region label (character); 3: genetic difference (numeric); 4: color code.
#' @param target A character value to pass name of the target population. Default is "target".
#' @param sorting Logical. Whether to sort population order according to their genetic difference. Default is FALSE.
#' @param layers Numeric values of layers. This parameter will mask "num" if specified.
#' @param num An integer. Default is 4. Layer number.
#' @param cenvals The coordinates of the center of the circle. Default is c(0.5, 0.5).
#' @param border A numeric value. The limits of the plot is (-border, border). Default is 0.3.
#' @param digits A float number. The decimal space that the genetic distance should be rounded to. Default is 2.
#' @param amax Numeric. Maximal angle of radiation bars. Default is -250.
#' @param amin Numeric. Minimal angle of radiation bars. Default is 70.
#' @param rstart A numeric value of radius. Default is 0.02.
#' @param flat A numeric value. Flattening coefficient of radiation bars. Default is 1.2.
#' @param label_mode Logical. Whether to print the population labels. Default is TRUE.
#' @param pop.lab.cex The cex of population labels. Default: 0.7.
#' @param pop.lab.font The font of population labels. Default: 1.
#' @param pop.lab.col The color of population labels. Default: "navy".
#' @param legend_mode Logical. Whether to draw the legend of region information. Default is FALSE.
#' @param legend.pos The position of the legend. Can be set as "top", "topleft", "topright", "bottom", "bottomleft", "bottomright", "left", and "right". Default: "topright".
#' @param legend.lwd The line width of the legend colors.
#' @param legend.text.cex The text size of the legend.
#' @param ring.text.col The color of the text indicating the genetic distance. Default: "black".
#' @param ring.text.cex The size of the text indicating the genetic distance. Default: 1.
#' @param ring.text.font The font of the text indicating the genetic distance. Default: 1.
#' @param ring.line.col The color of the outer rings. Default: "gray". 
#' @param tar.lab.col The color of the target label. Default: "black".
#' @param tar.lab.cex The size of the target label. Default: 1.1.
#' @param tar.lab.font The font of the target label. Default: 1.
#' @param core.line.col The.color of the core edge.
#' @return NULL
#' @export
radiationplot <- function(data, target = "target", sorting = FALSE, layers = NULL, num = 4, digits = 2,
                          cenvals = c(0.5, 0.5), border = 0.3, amax = -250, amin = 70, rstart = 0.02, flat = 1.2, 
                          label_mode = TRUE, pop.lab.cex = 0.7, pop.lab.font = 1, pop.lab.col = "navy",
                          legend_mode = FALSE, legend.pos = "topright", legend.lwd = 5, legend.text.cex = 1.00,
                          ring.text.col = "black", ring.text.cex = 1, ring.text.font = 1, ring.line.col = "gray",
                          tar.lab.col = "black", tar.lab.cex = 1.1, tar.lab.font = 1, core.line.col = 'black'){

  vals <- data[,val_col]
  if(is.null(layers)){
    layers <- round(max(vals)*seq(1/num, 1, 1/num), digits = digits) # the layers of genetic distance
  }

  centre <- c(2 * cenvals[1] * border-border, 2 * cenvals[2] * border - border)
  if(sorting){
    data <- data[order(data[,val_col]), ]
  }
  data[, val_col] <- flat * data[, val_col]
  x0 = centre[1]
  y0 = centre[2]
  
  # Put a canvas
  graphics::par('oma' = c(0,0,0,0))
  graphics::par('mar' = c(2,2,2,2))
  graphics::par('xpd' = TRUE)
  graphics::plot(0, 0, xlim = c(-border, border), ylim = c(-border, border), axes = F, ann = F, type = 'n')
    
  # Draw the rings as markers of genetic distance
  ring(x0, y0, layers, rstart, flat, ring.text.col = ring.text.col, ring.text.cex = ring.text.cex, ring.text.font = ring.text.font, ring.line.col = ring.line.col, digits = digits)
  
  # Draw the sectors showing fst values
  plot_values(x0, y0, data, amax, amin, rstart, label_mode, pop.lab.cex = pop.lab.cex, pop.lab.font = pop.lab.font, pop.lab.col = pop.lab.col)
  
  # Draw the core showing the target
  core(x0, y0, target, rstart, tar.lab.col = tar.lab.col, tar.lab.cex = tar.lab.cex, tar.lab.font = tar.lab.font, core.line.col = core.line.col)
  
  # Draw the legend
  regioncolors <- unique(data[,c(reg_col, col_col)])
  if(legend_mode){
    plot_legend(data = regioncolors, legend.pos = legend.pos, legend.lwd = legend.lwd, legend.text.cex = legend.text.cex)
  }

  # Done
}

