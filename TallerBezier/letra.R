require(bezier)
require(gridBezier)
t <- seq(0, 5, length=100)

## BEZIER CURVES ##
p <- matrix(c(12,20,1, 
              10,22,1,
              8,20,1, 
              8,12,1,
              10,12,1,
              12,12,1,
              12,4,1,
              10,4,1,
              8,4,1,
              8,5,1), nrow=10, ncol=3, byrow=TRUE)
bezier_points <- bezier(t=t, p=p, deg = 3)
plot(bezier_points)

X1= bezier_points[,1]
Y= bezier_points[,2]
X2= bezier_points[,3]

attach(ex1)                
names(ex1)
dim(ex1)

#----------------------------------------------------------
require(scatterplot3d)
fig = scatterplot3d(X1,Y,X2, box=F, type='p', lwd=1, pch=19, 
                    xlim=c(0,30), ylim=c(0,30), zlim=c(0,30))
plano = lm(Y~X1+X2)
