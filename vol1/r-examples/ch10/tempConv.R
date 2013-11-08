x = matrix(c(0,100,1,1),nrow=2,ncol=2)

y = matrix(c(32,212),nrow=2,ncol=1)

q <- qr.solve(x,y, tol = 1e-10)

qr.coef(q,y)