
f1 = function(t,y){
  -150*y+49-150*t
}

f_app = function(t,y0,h){
  y = numeric(length(t))
  y[1]=y0;
  for(i in 2:length(t)){
    y[i] = y[i-1]+h/2*(f1(t[i-1],y[i-1])+f1(t[i],y[i-1]+h*f1(t[i-1],y[i-1])))
  }
  return(y)
}

#Pour epsilon = 0

f_ext_eps0 = function(t){
  return(1/3-t)
}

n = 100
h=1/n
t = seq(0,1,by=h)
y0 = 1/3 + 0.01
f_app_L = f_app(t,y0,h)
f_ext_eps0_L = mapply(f_ext_eps0,t=t)

emax = max(abs(f_ext_eps0_L-f_app_L))
cat(emax)