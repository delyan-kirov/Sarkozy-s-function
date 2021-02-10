f1 = function(k)
{
  k1 = length(k)
  for (i in 1:(k1-1))
  {
    for (j in (i+1):k1)
    {
      x = as.integer(k[i]-k[j])
      if(x<0)
      {
        x = (-1)*x
      }
      x1 = sqrt(x)%%1
      
      if (x1 == 0)
      {
        return(0)
      }
    }
  }
  return(k1)
}

f2 = function(z)
{
  z1 = 1:z
  for (i in 1:z)
  {
    z1[i] = factorial(z)/((factorial(i-1)*(factorial(z-(i-1)))))
  }
  return(z1)
}


f3 = function(k)
{
  k1 = length(k)
  p = f2(k1)
  a = 0
  for (i in 2:k1)
  {
    x = seq(1:i)
    s = combn(1:k1, i, simplify = TRUE)
    s1 = as.vector(s)
    p1 = p[k1-i+1]
    
    for (n in 1:p1)
    {
      for(m in 1:i)
      {
        a = a+1
        x[m] = s1[a]
      }
      if (f1(x) != 0)
      {
        sark = f1(x)
      }
    }
    a = 0
  }
  print(sark)
}