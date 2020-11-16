#ccc computes Lin's concordance correlation coefficient for the two vectors input
#returns a dataframe containing several parameters
#[1] ccc
#[2] mean-shift
#[3] var shift
#[4] pearson's correlation 
ccc <- function(x,y)
  {
    #check vectors are same length
    if(length(x)!=length(y)) stop("Vectors must be same length")
    if(sum(c(x,y), na.rm=T)==0)
      {
        ccc <- 1
        u <- 0
        v <- 0
        p <- 1
      }
    else
      {
        m1 <- mean(x, na.rm=T)
        m2 <- mean(y, na.rm=T)
        v1 <- var(x, na.rm=T)
        v2 <- var(y, na.rm=T)
        sd1 <- sqrt(v1)
        sd2 <- sqrt(v2)
        
        u <- (m1-m2)/sqrt(sd1*sd2)
        v <- sd1/sd2
        Cb <- 1/((v+(1/v)+u^2)/2)
        
        p <- cor(x,y, use="complete.obs",method="pearson")

        B1 <- v*p
        B0 <- m1-(B1*m2)
        a <-(2*B1*v2)
        b <- v1+v2+(B0+(B1-1)*m2)^2
        ccc <- a/b
      }
    return(list(ccc=ccc, u=u, v=v, pearson=p))#, a=a, b=b, v1=v1, v2=v2, b0=B0, b1=B1, m2=m2))
  }

