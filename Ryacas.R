library(Ryacas)

x = ysym('x')
y = ysym('y')

f = x^4 - 2*x^3 + 1/6*y^2 - 3*x - x*y
curve(y_eval(f, x=x, y=2, as.r=TRUE), xlim=c(0,10))
print(solve(deriv(f,x),deriv(f,y) ,c("x", "y")))
# print(deriv(f,x))
# print(deriv(f,y))
Hes = Hessian(f,c("x", "y"))
Hesr = as_r(Hes)
print(typeof(Hesr))



# library(Ryacas)
# 
# yac_str("HessianMatrix(x^2 * (y/4) - a*(3*x + 3*y/2 - 45), {x,y})")
# # "{{y/2,x/2},{x/2,0}}"
# 
# yac_str("PrettyForm(HessianMatrix(x^2 * (y/4) - a*(3*x + 3*y/2 - 45), {x,y}))") %>% cat


# library(Ryacas)
# # p = c(0.1, 0.01 , 0.001)
# # tol = 10^(-5)
# # x0=0
# # y0=0
# x = ysym('x')
# y = ysym('y')
# f = x^2 +y^2 +3*y*x -y
# H_f= Hessian(f,c("x","y"))
# print(H_f)