f5 = function(k) #checks for prime - 1
{
  k1 = length(k)
  for (i in 1:(k1-1)) #goes tru the elements
  {
    for (j in (i+1):k1)
    {
      x = as.integer(k[i]-k[j])
      if(x<0)
      {
        x = (-1)*x
      }
      
      if ((x == 1) | (x==2) ) #case when x = 1 or 2
      {
        print("x=1")
        return(0)
      }
      
      if (k1-1 == 1) #case when k1 is 2
      {
        for(q in 2:(x-1))
        {
          x1 = (x+1) %% q
          if (x1 == 0)
          {
            print ("k1=2")
            print (q)
            return(0)
          }
          else
          {
            print("finished1")
            return(k1)
          }
        }
      }
      
      for (q in 2:(x-1)) #other cases
      {
        x1 = x+1 %% q
        
        if (x1 == 0)
        {
          print("got here")
          return (q)
        }
      }
      
    }
  }
  print("finished")
  return(k1)
}