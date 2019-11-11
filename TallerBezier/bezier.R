require(bezier)
require(gridBezier)
t <- seq(0, 1, length=100)

## BEZIER CURVES ##
p <- matrix(c(2,2, 4,6, 8,6, 10,2), nrow=4, ncol=2, byrow=TRUE)
bezier_points <- bezier(t=t, p=p, deg = NULL)
plot(bezier_points)


## BEZIER SPLINES ##
t <- seq(0, 2, length=100)
p <- matrix(c(2,2,1, 
              4,6,1, 
              8,6,1, 
              10,2,1, 
              14,6,1, 
              18,6,1, 
              20,1,1), nrow=7, ncol=3, byrow=TRUE)
p
bezier_points <- bezier(t=t, p=p, deg = 3)
bezier_points
plot(bezier_points)
