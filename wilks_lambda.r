# X - Design (predictors) matrix (n x m) with observations on the rows 
# Y - Outcome matrix (n x k) with fitted observation on the rows 
# B1- Betas of the multivariate regression
wilks_lambda <- function(B1, X, Y)
{
ModelSS = t(B1) %*% t(X) %*% Y;
ErrorSS = t(Y) %*% Y  - t(B1) %*% t(X) %*% Y
WilksLambda=prod(1/(1+eigen(solve(ErrorSS)*ModelSS)$values))
p=dim(X)[2]
q=dim(Y)[2]
N=dim(Y)[1]
a=1
Rao=WilksLambda^(N/2)
Raoapprox=-(N-p-1-0.5*(q-p+a+1))*log(Rao)
return(list(WilksLambda=WilksLambda,p.val=1-pchisq(Raoapprox, p-1)));
}



