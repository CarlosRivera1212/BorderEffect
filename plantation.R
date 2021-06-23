plantation <- function(n_plant = 48, arr = 2, width = 250, heigh = 210,
                       nx = 6, ny = 8, plot = T){
  # n_plant: number of plants
  # arr: 1=sqare, 2=tres bolillos
  # width: width total of rows
  # heigh: heigh total of plant-by-row
  # nx: number of rows
  # ny: nomber of plant-by-row
  
  
  if ((nx*ny) != n_plant) {
    warning('width and heigh are different of numer of plants')
  }
  
  else {
    xy = expand.grid(x=seq(0,width,l=nx), y=seq(0,heigh,l=ny))
    if(arr==2){xy$x[xy$y %in% unique(xy$y)[seq(2,ny,2)]] = xy$x[xy$y %in% unique(xy$y)[seq(2,ny,2)]] + width/(2*(nx-1))}
    
    xy$br1 = 0
    xy$br1[with(xy, which(x==min(x) | x==max(x) | y==min(y) | y==max(y)))] = 1
    
    xy$br2 = 0
    xy$br2[with(xy, which((x==min(x[br1!=1]) | x==max(x[br1!=1]) | y==min(y[br1!=1]) | y==max(y[br1!=1])) & br1!=1))] = 1
    
    
    xy$br_1_d = 0
    xy$br_1_d[with(xy, which(y==min(y)))] = 1
    xy$br_1_l = 0
    xy$br_1_l[with(xy, which(x==min(x)))] = 1
    xy$br_1_u = 0
    xy$br_1_u[with(xy, which(y==max(y)))] = 1
    xy$br_1_r = 0
    xy$br_1_r[with(xy, which(x==max(x)))] = 1
    
    
    xy$br_2_dl = 0
    xy$br_2_dl[with(xy, which(y==min(y) | x==min(x)))] = 1
    xy$br_2_du = 0
    xy$br_2_du[with(xy, which(y==min(y) | y==max(y)))] = 1
    xy$br_2_dr = 0
    xy$br_2_dr[with(xy, which(y==min(y) | x==max(x)))] = 1
    xy$br_2_lu = 0
    xy$br_2_lu[with(xy, which(x==min(x) | y==max(y)))] = 1
    xy$br_2_lr = 0
    xy$br_2_lr[with(xy, which(x==min(x) | x==max(x)))] = 1
    xy$br_2_ur = 0
    xy$br_2_ur[with(xy, which(x==max(x) | y==max(y)))] = 1
    
    
    
    xy$br_3_dlu = 0
    xy$br_3_dlu[with(xy, which(y==min(y) | x==min(x) | y==max(y)))] = 1
    xy$br_3_dlr = 0
    xy$br_3_dlr[with(xy, which(y==min(y) | x==min(x) | x==max(x)))] = 1
    xy$br_3_dur = 0
    xy$br_3_dur[with(xy, which(y==min(y) | y==max(y) | x==min(x)))] = 1
    xy$br_3_lur = 0
    xy$br_3_lur[with(xy, which(x==min(x) | y==max(y) | x==max(x)))] = 1
    
    
    xy$br_1_h = 0
    xy$br_1_h[with(xy, which(y==y[(round(ny/2)+1)*ny]))] = 1
    
    
    tbr = expand.grid(rep = factor(paste0('R',1:(n_plant/(2*3)))),
                      blk = factor(paste0('B',1:2)),
                      trt = factor(paste0('T',1:3)))
    
    tbr$int=rep(1,n_plant)
    tbr$posy = 1:n_plant
    
    tbr$tau1 = ifelse(tbr$trt=='T1',1,0)
    tbr$tau2 = ifelse(tbr$trt=='T2',1,0)
    tbr$tau3 = ifelse(tbr$trt=='T3',1,0)
    
    tbr$bk1 = ifelse(tbr$blk=='B1',1,0)
    tbr$bk2 = ifelse(tbr$blk=='B2',1,0)
    
    
    # set.seed(1)
    # set.seed(2026)
    tbr = tbr[sample(1:n_plant,n_plant,F),]
    xy = data.frame(xy, tbr)
    
    
    if (plot) {
      plot(xy$x, xy$y, col = tbr$trt, pch = 8, 
           xlab='Plants by row (spacing in cm)', ylab='Rows (spacing in cm)')
      text(xy$x, xy$y, 1:n_plant, pos=3, cex=0.6)
      text(xy$x, xy$y, xy$posy, pos=4, cex=0.6)
      text(xy$x, xy$y, with(xy, interaction(trt,blk,rep)), pos=1, cex=0.6)
    }
    
    return(xy)
  }
}
