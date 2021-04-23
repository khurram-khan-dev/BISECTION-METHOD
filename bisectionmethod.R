##bisection_method

bisection <- function(x1,x2)
{
  x <- 0
  ## f(x1)
  c = 5*x^3 - 5*x^2 + 6*x - 2
  x3 <- vector()
  for (i in 1:20)
  {
    x3[i] <- (x1+x2)/2
    ##f(x3)
    y <- 5*x3[i]^3 - 5*x3[i]^2 + 6*x3[i] - 2
    
    if(c*y < 0)
    {
      x2 <- x3[i]
    }
    else if (c*y > 0)
    {
      x1 <- x3[i]
    }
    ## error
    if (i==1)
    {
      next
    }
    else
    {
    absolutevalue <- abs(x3[i]-x3[i-1])
    }
    
    ea <- (absolutevalue/x3[i])*100
    print(ea)
    
    ## stopping criteria
    if(ea<10)
    {
      break
    }
  }
}
bisection(0,1)
