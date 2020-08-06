#________________________
# IS - LM Model
# Author: Ronny M. Condor
#________________________
library(ggplot2)
library(ggthemes)
library(tidyverse)

# 1. IS equation
# r = (A/b)- ((1-c(1-t))/b)*Y
IS.curve <- function(c,t,A,b,Y){
  r = (A/b)- ((1-c(1-t))/b)*Y
  return(r)
}


# 2. LM equation
# Y=M/k - h/k * r
LM.curve <-function(M,k,h,Y){
  r=(k/h)*Y-(M/h)
  return(r)
}

# 3. Parameters
c <-0.75
t <-0.28
A <-100
b <-50
M <-80
k <-0.6
h <-80
# Range of Y 
Y<- sample(1:500,100,replace = F)

# 4. Solution
B <- matrix(c(1-c(1-t),b,k,-h), nrow =2,ncol=2,byrow = T)
d <- rbind(A,M)

equilibrium <- solve(B,d)
equilibrium <- t(equilibrium)
equilibrium<- as.data.frame(equilibrium)
equilibrium <- rename(equilibrium, Y_eq = V1, r_eq = V2)


r <- IS.curve(c,t,A,b,Y)
IS <- cbind(r,Y)
IS <- as.data.frame(IS)

r <- LM.curve(M,k,h,Y)
LM <- cbind(r,Y)
LM <- as.data.frame(LM)


ggplot(mapping=aes(Y,r))+
  geom_path(data = IS, aes(Y,r, color="a"))+
  geom_path(data=LM,aes(Y,r, color="b"))+
  labs( title = "IS-LM Model", x="Gross Product", y="Interest rate", 
        caption = "Source: Own elaboration",
        color="Legend")+
  scale_color_manual( values = c("a"="#288C80","b"="#FDB805"), 
                     labels=c("IS curve","LM curve"))+
  geom_segment(data = equilibrium, 
               aes(x = Y_eq, y = 0, xend = Y_eq, yend = r_eq), lty = "dotted") +
  geom_segment(data = equilibrium, 
               aes(x = 0, y = r_eq, xend = Y_eq, yend = r_eq), lty = "dotted")+
  geom_point(aes(x=equilibrium$Y_eq, y=equilibrium$r_eq), size = 3)+
  coord_cartesian(xlim=c(0,max(LM$Y)), ylim = c(0,max(IS$r)))+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))

# 5. Comparative Statics
# Example 1: A monetary expansive policy

M_new <- 120
r_new <- LM.curve(M_new,k,h,Y)
LM_new <- cbind(r_new,Y)
LM_new <- as.data.frame(LM_new)

ggplot()+
  geom_path(data = IS, aes(Y,r))+
  geom_path(data=LM, aes(Y,r))+
  geom_path(data=LM_new, aes(Y,r_new))+
  annotate("segment", x = 255, xend = 278, y = 0.85, yend = 0.60,
           arrow = arrow(length = unit(0.60, "lines")), colour = "grey50")
