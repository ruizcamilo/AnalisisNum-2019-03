---
title: "Taller Interpolación"
author: "Sebastian Roberts, Camilo Ruiz, Alex Barreto"
date: "06/09/2019"
output:
  html_document:
    df_print: paged
  pdf_document: default
---

#2.  Construya un polinomio de grado tres que pase por:(0, 10),(1, 15),(2, 5) y que la tangente sea igual a 1 en x0
    
require(pracma) 
x = c(0,1,2)
y= c(10,15,5)
xs <- seq(0, 2, by = 0.1)
pp <- cubicspline(x, y)
ppfun <- function(xs) ppval(pp, xs)
curve(ppfun(x),from=0,to=3,xlab = "x", ylab = "f(x)", main = "Grafica Segundo Punto")
