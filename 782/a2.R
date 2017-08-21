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