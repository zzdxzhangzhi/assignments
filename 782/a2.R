fib = function(n) {
  s = numeric(n)
  
  if (n <= 1) s[n] = 0 
  else {
    s[1:(n - 1)] = fib(n - 1)
    if (n == 2) s[n] = 1
    else s[n] = s[n - 1] + s[n - 2]
  }
  
  s
}

fib(1)
fib(2)
fib(3)
fib(10)

clusters.medians = function(x, c) {
  lenx = length(x)
  lenc = length(c)
  d = outer(c, x, function(cj, xi) abs(xi - cj))
  print(d)
  d.min = which.min(d[,1:lenx])
  print("\n")
  print(d.min)
  con = outer(1:lenc, d.min, function(num, minnum) num == minnum)
  print("\n")
  print(con)
  median(x[con[1:lenc,]])
}

x = faithful$eruptions
clusters.medians(x, c(2,4))
