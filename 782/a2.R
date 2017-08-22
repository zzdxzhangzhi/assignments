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
  lenc = length(c)
  
  d = outer(c, x, function(cj, xi) abs(xi - cj))
  d.minnum = apply(d, 2, which.min)
  
  con = outer(1:lenc, d.minnum, function(num, minnum) num == minnum)
  
  xv = unlist(apply(con, 1, function(t) median(x[t])))
  xv
}

find.clusters.medians = function(x, c) {
  ctmp1 = c
  repeat {
    ctmp2 = clusters.medians(x, ctmp1)
    if (all(abs(ctmp1 - ctmp2) < 1e-07)) break
    else ctmp1 = ctmp2
  }
  ctmp1
}

x = faithful$eruptions
find.clusters.medians(x, c(2,4))
find.clusters.medians(x, c(2,3,4))
find.clusters.medians(x, c(2,3,4,5))

x = 8 * runif(207)
find.clusters.medians(x, c(2,3,4,5,6))
find.clusters.medians(x, c(2,3,4,5,6,7))
