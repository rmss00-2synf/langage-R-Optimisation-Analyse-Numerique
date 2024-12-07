library('pracma')
library('geometry')
library('ggplot2')
library('geometry')
library('plotly')

f = function(x,y){
  exp(x - 3*y - 1/10) + exp(x + 3*y - 1/10) + exp(- x - 1/10)
  
}

gradian=function(x,y){
  c(exp(x - 3*y - 1/10) + exp(x + 3*y - 1/10) - exp(- x - 1/10),3*exp(x + 3*y - 1/10) - 3*exp(x - 3*y - 1/10))
}

phi = function(x,y,t){
  exp(- x - t*(exp(x - 3*y + 3*t*(3*exp(x - 3*y - 1/10) - 3*exp(x + 3*y - 1/10)) - 1/10) + exp(x + 3*y - 3*t*(3*exp(x - 3*y - 1/10) - 3*exp(x + 3*y - 1/10)) - 1/10) - exp(- x - 1/10)) - 1/10) + exp(x - 3*y + 3*t*(3*exp(x - 3*y - 1/10) - 3*exp(x + 3*y - 1/10)) + t*(exp(x - 3*y + 3*t*(3*exp(x - 3*y - 1/10) - 3*exp(x + 3*y - 1/10)) - 1/10) + exp(x + 3*y - 3*t*(3*exp(x - 3*y - 1/10) - 3*exp(x + 3*y - 1/10)) - 1/10) - exp(- x - 1/10)) - 1/10) + exp(x + 3*y - 3*t*(3*exp(x - 3*y - 1/10) - 3*exp(x + 3*y - 1/10)) + t*(exp(x - 3*y + 3*t*(3*exp(x - 3*y - 1/10) - 3*exp(x + 3*y - 1/10)) - 1/10) + exp(x + 3*y - 3*t*(3*exp(x - 3*y - 1/10) - 3*exp(x + 3*y - 1/10)) - 1/10) - exp(- x - 1/10)) - 1/10)
  
}

phi_derive = function(x,y,t){
  exp(x + 3*y + 3*t*(3*exp(x - 3*y - 1/10) - 3*exp(x + 3*y - 1/10)) - t*(exp(x - 3*y - 3*t*(3*exp(x - 3*y - 1/10) - 3*exp(x + 3*y - 1/10)) - 1/10) + exp(x + 3*y + 3*t*(3*exp(x - 3*y - 1/10) - 3*exp(x + 3*y - 1/10)) - 1/10) - exp(- x - 1/10)) - 1/10)*(9*exp(x - 3*y - 1/10) - 9*exp(x + 3*y - 1/10) - exp(x - 3*y - 3*t*(3*exp(x - 3*y - 1/10) - 3*exp(x + 3*y - 1/10)) - 1/10) - exp(x + 3*y + 3*t*(3*exp(x - 3*y - 1/10) - 3*exp(x + 3*y - 1/10)) - 1/10) + exp(- x - 1/10) + t*(exp(x - 3*y - 3*t*(3*exp(x - 3*y - 1/10) - 3*exp(x + 3*y - 1/10)) - 1/10)*(9*exp(x - 3*y - 1/10) - 9*exp(x + 3*y - 1/10)) - exp(x + 3*y + 3*t*(3*exp(x - 3*y - 1/10) - 3*exp(x + 3*y - 1/10)) - 1/10)*(9*exp(x - 3*y - 1/10) - 9*exp(x + 3*y - 1/10)))) + exp(t*(exp(x - 3*y - 3*t*(3*exp(x - 3*y - 1/10) - 3*exp(x + 3*y - 1/10)) - 1/10) + exp(x + 3*y + 3*t*(3*exp(x - 3*y - 1/10) - 3*exp(x + 3*y - 1/10)) - 1/10) - exp(- x - 1/10)) - x - 1/10)*(exp(x - 3*y - 3*t*(3*exp(x - 3*y - 1/10) - 3*exp(x + 3*y - 1/10)) - 1/10) + exp(x + 3*y + 3*t*(3*exp(x - 3*y - 1/10) - 3*exp(x + 3*y - 1/10)) - 1/10) - exp(- x - 1/10) - t*(exp(x - 3*y - 3*t*(3*exp(x - 3*y - 1/10) - 3*exp(x + 3*y - 1/10)) - 1/10)*(9*exp(x - 3*y - 1/10) - 9*exp(x + 3*y - 1/10)) - exp(x + 3*y + 3*t*(3*exp(x - 3*y - 1/10) - 3*exp(x + 3*y - 1/10)) - 1/10)*(9*exp(x - 3*y - 1/10) - 9*exp(x + 3*y - 1/10)))) - exp(x - 3*y - 3*t*(3*exp(x - 3*y - 1/10) - 3*exp(x + 3*y - 1/10)) - t*(exp(x - 3*y - 3*t*(3*exp(x - 3*y - 1/10) - 3*exp(x + 3*y - 1/10)) - 1/10) + exp(x + 3*y + 3*t*(3*exp(x - 3*y - 1/10) - 3*exp(x + 3*y - 1/10)) - 1/10) - exp(- x - 1/10)) - 1/10)*(9*exp(x - 3*y - 1/10) - 9*exp(x + 3*y - 1/10) + exp(x - 3*y - 3*t*(3*exp(x - 3*y - 1/10) - 3*exp(x + 3*y - 1/10)) - 1/10) + exp(x + 3*y + 3*t*(3*exp(x - 3*y - 1/10) - 3*exp(x + 3*y - 1/10)) - 1/10) - exp(- x - 1/10) - t*(exp(x - 3*y - 3*t*(3*exp(x - 3*y - 1/10) - 3*exp(x + 3*y - 1/10)) - 1/10)*(9*exp(x - 3*y - 1/10) - 9*exp(x + 3*y - 1/10)) - exp(x + 3*y + 3*t*(3*exp(x - 3*y - 1/10) - 3*exp(x + 3*y - 1/10)) - 1/10)*(9*exp(x - 3*y - 1/10) - 9*exp(x + 3*y - 1/10))))
}

prod_scal_grad = function(x,y){
  dot(gradian(x,y),gradian(x,y),d=TRUE)
}

Pas = function(w1,w2,alpha,beta,x0,y0,max_i){
  i=1
  x = c(x0)
  y = c(y0)
  # print(prod_scal_grad(x[i],y[i]))
  while (i<=max_i & norm(gradian(x[i],y[i]),'2')>1e-15) {
    j = 0
    ro = 1
    while ((j<=max_i) & (phi(x[i],y[i],ro)>(f(x[i],y[i])-ro*w1*prod_scal_grad(x[i],y[i])))) {
      cat(phi(x[i],y[i],ro)," : ",(f(x[i],y[i])-ro*w1*prod_scal_grad(x[i],y[i])),"\n");
      if(phi(x[i],y[i],ro)>f(x[i],y[i])-ro*w1*prod_scal_grad(x[i],y[i])){
        ro = alpha*ro;
        j = j+1
      }
    }
    # print(ro);
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
x0 =1
y0=1
max_i = 10000

print(Pas(w1,w2,alpha,beta,x0,y0,max_i))


rm(list = ls())
