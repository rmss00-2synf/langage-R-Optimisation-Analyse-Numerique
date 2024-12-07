library('pracma')
library('geometry')
library('ggplot2')
library('plotly')
f = function(x,y){
  x^2/2 + (7*y^2)/2
}
gradian = function(x,y){
  c( x,7*y)
}
phi = function(x,y,t){
  (x - t*x)^2/2 + (7*(y - 7*t*y)^2)/2
}
phi_derive = function(x,y,t){
  - x*(x - t*x) - 49*y*(y - 7*t*y)
}
prod_scal_grad = function(x,y){
  dot(gradian(x,y),gradian(x,y))
}

Pas = function(w1,w2,alpha,beta,x0,y0,max_i){
  ro = 1
  i=1
  x = c(x0)
  y = c(y0)
  print(prod_scal_grad(x[i],y[i]))
  while (i<=max_i & prod_scal_grad(x[i],y[i])>1e-5) {
    j = 0
    
    while ((j<=max_i-90) & ((phi(x[i],y[i],ro)>(f(x[i],y[i])-ro*w1*prod_scal_grad(x[i],y[i]))) | (phi_derive(x[i],y[i],ro)>w2*prod_scal_grad(x[i],y[i])))) {
      if(phi(x[i],y[i],ro)>f(x[i],y[i])-ro*w1*prod_scal_grad(x[i],y[i])){
        ro = alpha*ro;
        j = j+1
      }
      else if(phi_derive(x[i],y[i],ro)>w2*prod_scal_grad(x[i],y[i])){
        ro = beta*ro;
        j = j+1
      }
      
    }
    print(ro);
    x =c(x,x[i]-ro*gradian(x[i],y[i])[1])
    y =c(y,y[i]-ro*gradian(x[i],y[i])[2])
    i=i + 1
    
  }
  return(list(x,y))
}

w1 = 0.1
w2=0.5
alpha=0.1
beta=1.1
x0 =5 
y0=5
max_i = 100

print(Pas(w1,w2,alpha,beta,x0,y0,max_i))
rm(list = ls())