library(writexl)
library(stringr)
library(ggplot2)
library(ape)

set.seed(3000)

iter = 3000
variables = 96

# CROP PLOT
df = plantation(n_plant = variables, arr = 2, 
                width = 550, heigh = 210, 
                nx = 12, ny = 8,
                plot = F)

REND.d=as.matrix(dist(df[,1:2], diag=T, upper=T))
REND.d.inv <-as.matrix(1/REND.d)
diag(REND.d.inv) <- 0
W=as.matrix(REND.d.inv)
We=W/sum(W)

y_wo_border = replicate(iter, 
                        rnorm(variables, 2.21, 0.06))

y_1c_border = replicate(iter,
                     rnorm(variables,
                           ifelse(df$br1==1,2.21,1.74),
                           ifelse(df$br1==1,0.06,0.035)))
y_2c_border = replicate(iter, 
                      rnorm(variables, 
                            ifelse(df$br2 | df$br1,
                                   2.21,1.74),
                            ifelse(df$br2 | df$br1,
                                   0.06,0.035)))

# 1 BORDE - 1 SIDE
y_br_1_d = replicate(iter,
                     rnorm(variables,
                           ifelse(df$br_1_d,2.21,1.74),
                           ifelse(df$br_1_d,0.06,0.035)))

y_br_1_l = replicate(iter,
                     rnorm(variables,
                           ifelse(df$br_1_l,2.21,1.74),
                           ifelse(df$br_1_l,0.06,0.035)))

y_br_1_u = replicate(iter,
                       rnorm(variables,
                             ifelse(df$br_1_u,2.21,1.74),
                             ifelse(df$br_1_u,0.06,0.035)))

y_br_1_r = replicate(iter,
                       rnorm(variables,
                             ifelse(df$br_1_r,2.21,1.74),
                             ifelse(df$br_1_r,0.06,0.035)))

# 1 BORDE - 2 SIDE 
y_br_2_dl = replicate(iter,
                        rnorm(variables,
                              ifelse(df$br_2_dl,2.21,1.74),
                              ifelse(df$br_2_dl,0.06,0.035)))
y_br_2_du = replicate(iter,
                        rnorm(variables,
                              ifelse(df$br_2_du,2.21,1.74),
                              ifelse(df$br_2_du,0.06,0.035)))
y_br_2_dr = replicate(iter,
                        rnorm(variables,
                              ifelse(df$br_2_dr,2.21,1.74),
                              ifelse(df$br_2_dr,0.06,0.035)))

y_br_2_lu = replicate(iter,
                        rnorm(variables,
                              ifelse(df$br_2_lu,2.21,1.74),
                              ifelse(df$br_2_lu,0.06,0.035)))
y_br_2_lr = replicate(iter,
                        rnorm(variables,
                              ifelse(df$br_2_lr,2.21,1.74),
                              ifelse(df$br_2_lr,0.06,0.035)))
y_br_2_ur = replicate(iter,
                        rnorm(variables,
                              ifelse(df$br_2_ur,2.21,1.74),
                              ifelse(df$br_2_ur,0.06,0.035)))


# 1 BORDE - 3 SIDE 
y_br_3_dlu = replicate(iter,
                         rnorm(variables,
                               ifelse(df$br_3_dlu,2.21,1.74),
                               ifelse(df$br_3_dlu,0.06,0.035)))
y_br_3_dlr = replicate(iter,
                         rnorm(variables,
                               ifelse(df$br_3_dlr,2.21,1.74),
                               ifelse(df$br_3_dlr,0.06,0.035)))
y_br_3_dur = replicate(iter,
                         rnorm(variables,
                               ifelse(df$br_3_dur,2.21,1.74),
                               ifelse(df$br_3_dur,0.06,0.035)))

y_br_3_lur = replicate(iter,
                         rnorm(variables,
                               ifelse(df$br_3_lur,2.21,1.74),
                               ifelse(df$br_3_lur,0.06,0.035)))


# HORIZONTAL LINE CENTER
y_br_1_h = replicate(iter,
                       rnorm(variables,
                             ifelse(df$br_1_h,2.21,1.74),
                             ifelse(df$br_1_h,0.06,0.035)))

data=data.frame(y_wo_border, y_1c_border, y_2c_border,
                y_br_1_d,y_br_1_l,y_br_1_u,y_br_1_r,
                
                y_br_2_dl,y_br_2_du,y_br_2_dr,y_br_2_lu,y_br_2_lr,y_br_2_ur,
                
                y_br_3_dlu,y_br_3_dlr,y_br_3_dur,y_br_3_lur,
                
                y_br_1_h)
dim(data)
colnames(data)=c(paste0("y_",seq(iter)),
                 paste0("yb_",seq(iter)),
                 paste0("yb2_",seq(iter)),
                 
                 paste0("yb_1_d",seq(iter)),
                 paste0("yb_1_l",seq(iter)),
                 paste0("yb_1_u",seq(iter)),
                 paste0("yb_1_r",seq(iter)),
                 
                 paste0("yb_2_dl",seq(iter)),
                 paste0("yb_2_du",seq(iter)),
                 paste0("yb_2_dr",seq(iter)),
                 paste0("yb_2_lu",seq(iter)),
                 paste0("yb_2_lr",seq(iter)),
                 paste0("yb_2_ur",seq(iter)),
                 
                 paste0("yb_3_dlu",seq(iter)),
                 paste0("yb_3_dlr",seq(iter)),
                 paste0("yb_3_dur",seq(iter)),
                 paste0("yb_3_lur",seq(iter)),
                 
                 paste0("yb_1_h",seq(iter)))


dff=data.frame(df,data)

X=with(df,cbind(int,tau1,tau2,tau3,bk1,bk2))
n=nrow(X)
In = diag(n)

# MATRIX OF TRANSFORMATION
Z=matrix(c(1,0,0,0,0,0,0,-1,0,1,1,-1,-1,1,-2,1,0,0,0,-2,1,1,-1,1),nrow=4,byrow=T)
P=X%*%t(Z)
# ORTOGONAL PROJECTION MATRIX
Mp=P%*%(solve(t(P)%*%P))%*%t(P)

# CONVENTION
# y        = without border
# yb       = one closed border
# yb2      = two closed border
# yb_1_d   = one-sided lower border
# yb_1_l   = one-sided left border
# yb_1_u   = one-sided upper border
# yb_1_r   = one-sided right border
# yb_2_dl  = two-sided lower-left border
# yb_2_du  = two-sided lower-upper border
# yb_2_dr  = two-sided lower-right border
# yb_2_lu  = two-sided left-upper border
# yb_2_lr  = two-sided left-right border
# yb_2_ur  = two-sided upper-right border
# yb_3_dlu = three-sided lower-left-upper border
# yb_3_dlr = three-sided lower-left-right border
# yb_3_dur = three-sided lower-upper-right border 
# yb_3_lur = three-sided left-upper-right border
# yb_1_h   = central-line

K_1 = sapply(data, function(Vi){
  (t(Vi)%*%(In-Mp)%*%We%*%(Vi))/((t(Vi)%*%We%*%(In-Mp)%*%We%*%(Vi)))
})
df_kap = data.frame(
  kap_y_We = K_1[1:iter],
  kap_yb_We = K_1[(1:iter)+iter],
  kap_yb2_We = K_1[(1:iter)+2*iter],
  
  kap_yb_1_d_We = K_1[(1:iter)+3*iter],
  kap_yb_1_l_We = K_1[(1:iter)+4*iter],
  kap_yb_1_u_We = K_1[(1:iter)+5*iter],
  kap_yb_1_r_We = K_1[(1:iter)+6*iter],
  
  kap_yb_2_dl_We = K_1[(1:iter)+7*iter],
  kap_yb_2_du_We = K_1[(1:iter)+8*iter],
  kap_yb_2_dr_We = K_1[(1:iter)+9*iter],
  kap_yb_2_lu_We = K_1[(1:iter)+10*iter],
  kap_yb_2_lr_We = K_1[(1:iter)+11*iter],
  kap_yb_2_ur_We = K_1[(1:iter)+12*iter],
  
  kap_yb_3_dlu_We = K_1[(1:iter)+13*iter],
  kap_yb_3_dlr_We = K_1[(1:iter)+14*iter],
  kap_yb_3_dur_We = K_1[(1:iter)+15*iter],
  kap_yb_3_lur_We = K_1[(1:iter)+16*iter],
  
  kap_yb_1_h_We = K_1[(1:iter)+17*iter]
)

lapply(df_kap[1:3], shapiro.test)

# ANOVAS Y F-values ####
anovas = lapply(data, function(Vi){
  summary(aov(Vi~dff$trt + dff$blk))
})

df_F_aov = data.frame(
  name = factor(rep(c('without border', 
                      'one closed border',
                      'two closed border'),
                    each = iter),
                levels = c('without border', 
                           'one closed border',
                           'two closed border'),
                ordered = T),
  F.value = sapply(anovas[1:(3*iter)],function(m){m[[1]][1,4]})
)

# Kolmogorov-Smirnov Tests for F-distribution
df_ks = aggregate(
  list(s = df_F_aov$F.value), 
  list(name = df_F_aov$name), 
  function(x){
    ks = ks.test(x, 'pf', df1 = 2, df2 = 92)
    paste0('D = ',round(ks$statistic,2),
           ' (p-value = ',round(ks$p.value,4),')')
  })
df_ks

# MORAN INDEX
pvIM = sapply(data, function(Yi){
  Moran.I(aov(Yi~dff$trt + dff$blk)$residuals, We)$p.value})

df_pvIM = data.frame(
  name = gl(3, iter, 3*iter, 
            c('without border', 
              'one closed border',
              'two closed border'), 
            ordered = T),
  pv = pvIM[1:(3*iter)])

tapply(df_pvIM$pv, df_pvIM$name, function(x)sum(x>=0.05))
