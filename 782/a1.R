seq(1, 28, by = 3)
paste(rep(letters[-1:-23], 3:1), rep(letters[1:3], 2), sep = "")
rep(1:3 < 2, 2) 
cumsum(10^(seq(0, 8, by = 2)))
1:4 + rep(0:3, rep(4, 4))

URL = "https://raw.githubusercontent.com/zzdxzhangzhi/assignments/adb70032ccc89e683055814da4c87727fcaf332a/782/eurocities.csv"
eurocities.df = read.csv(URL, stringsAsFactors = FALSE)
str(eurocities.df)
head(eurocities.df)

distance.mean = mean(eurocities.df$X3313)
distance.mean
maxd = max(eurocities.df$X3313)
con = (eurocities.df$X3313 == maxd)
maxdnum = rownames(eurocities.df)[con]
eurocities.df[maxdnum, 1:2]
maxdnum = which.max(eurocities.df$X3313)
maxpair = c(eurocities.df$Athens[maxdnum], eurocities.df$Barcelona[maxdnum])
maxpair

paris_300k = ((eurocities.df$Athens == "Paris" 
               | eurocities.df$Barcelona == "Paris") 
              & eurocities.df$X3313 <= 300)
sum(paris_300k)

length(which((eurocities.df$Athens == "Paris" 
       | eurocities.df$Barcelona == "Paris") 
      & eurocities.df$X3313 <= 300))

num = which(eurocities.df$Athens == "Copenhagen" 
            | eurocities.df$Barcelona == "Copenhagen")

copenhargen.df = data.frame(eurocities.df[num,])
d_sort = sort(copenhargen.df$X3313)
d_min3 = d_sort[1:3]
d_min3

cities3 = character(3)
for (i in 1:3) {
  min_No = which(copenhargen.df$X3313 == d_min3[i])
  cities3[i] = copenhargen.df$Barcelona[min_No]
  if (cities3[i] == "Copenhagen")
    cities3[i] = copenhargen.df$Athens[min_No]
}
cities3

num_start = which((eurocities.df$Athens == "Rome"
                  | eurocities.df$Athens == "Stockholm")
                  & (eurocities.df$Barcelona != "Rome"
                     & eurocities.df$Barcelona != "Stockholm"))
num_end = which((eurocities.df$Barcelona == "Rome"
                | eurocities.df$Barcelona == "Stockholm")
                & (eurocities.df$Athens != "Rome"
                   & eurocities.df$Athens != "Stockholm"))

start.df = data.frame(eurocities.df[num_start,])
end.df = data.frame(eurocities.df[num_end,])
start.df
end.df

## col is the column that contain the other same cities to the 2 cities
findcity.min = function(df, col, cityvec) {
  rows = rownames(df)
  rownum = nrow(df)
  colnum = ncol(df)
  distances = numeric(rownum %/% 2)
  cityvec.sort = sort(cityvec)
  for (i in seq(2, rownum, by = 2)) {
    # find the row number of two distances from 1 same city
    twodt.No = rows[df[, col] == cityvec.sort[i]]
    # add the two distance from the same sity
    distances[i %/% 2] = df[twodt.No[1], colnum] + df[twodt.No[2], colnum]
  }
  
  mindt = min(distances)
  distance_num = 1:length(distances)
  min_num = distance_num[distances == mindt]

  min_city = cityvec.sort[min_num * 2]
  list(city = min_city, min = mindt)
}

lst1 = findcity.min(start.df, 2, start.df$Barcelona)
lst1
lst2 = findcity.min(end.df, 1, end.df$Athens)
lst2

if (lst1$min < lst2$min) lst1$city else lst2$city

hemite1 = function(xseq, n) {
  for (x in xseq) {
    res = 0
    for (m in 0:floor(n / 2)) {
      res = res + (-1)^m / (factorial(m) * factorial(n - 2 * m)) * x^(n - 2 * m) / 2^m
    }
    print(factorial(n) * res)
  }
}

hemite1(seq(-2, 2, by = .2), 5)

hemite2 = function(xseq, n) {
  for (x in xseq) {
    upval = floor(n / 2)
    mrange = 0:upval
    res = factorial(n) * sum((-1)^mrange 
                             / (factorial(mrange) * factorial(n - 2 * mrange))
                             * x^(n - 2 * mrange) / 2^mrange)
    print(res)
  }
}

hemite2(seq(-2, 2, by = .2), 5)

hemite3.1 = function(x, m, n) {
  (-1)^m / (factorial(m) * factorial(n - 2 * m)) * x^(n - 2 * m) / 2^m
}

hemite3.2 = function(x, n) {
  factorial(n) * sum(x)
}

hemite.seq = function(x, n) {
  m = 0:floor(n / 2)
  hemite3.3 = function(x, m) hemite3.1(x, m, n)
  hemite3.4 = function(x) hemite3.2(x, n)
  
  o = outer(x, m, hemite3.3)
  apply(o, 1, hemite3.4)
}
x = seq(-2, 2, by = .2)
hemite.seq(x, 5)

hemite.coef = function(n) {
  if (n > 0) {
    x = seq(length = n + 1, from = 1)
    
    # contruct the value matrix
    A = outer(x, 0:n, "^")
    H = hemite.seq(x, n)
    
    # using round when x are not intergers
    round(solve(A, H))
  }
  else 1
}

hemite = function(n) {
  coef.matrix = matrix(0, nrow = n + 1, ncol = n + 1,
                       dimnames = list(paste("H", 0:n, sep = ""), 
                                       paste("x^", 0:n, sep = "")))
  for (i in 0:n) {
    coef.matrix[i + 1,] = c(hemite.coef(i), rep(0, n - i))
  }
  coef.matrix
}

hemite(0)
hemite(1)
hemite(3)
hemite(10)

f = function(x) sin(x) + exp((-1) * x / 10)

root = function(f, interval) {
  a = interval[1]
  b = interval[2]
  
  i = 1
  while ((f(a) * f(b)) < 0 
         && abs(f(a) - f(b)) > 1e-08
         && i < 1000) {
    c = (a + b) / 2
    tmp = f(c)
    if(sign(tmp) == sign(f(a)))
      a = c
    else
      b = c
    
    i = i + 1
  }
  
  if (i < 1000) {
    if ((f(a) * f(b)) < 0) a
    else {
      print("couldn't find root within this interval!")
      NA
    }
  }
  else {
    print("no root can be found!")
    NA
  }
}

root(f, c(0, 5))
root(f, c(5, 9))

allroot = function(f, interval) {
  a = interval[1]
  b = interval[2]
  points = seq(a, b, length.out = 1000)
  con = f(points) >= 0
  positives = sort(points[con])
  negatives = sort(points[!con])
  
  #print(positives)
  #print(negatives)
  
  if (positives[1] < negatives[1]) {
    low = positives[1]
    high = negatives[1]
    con = TRUE
  }
  else {
    low = negatives[1]
    high = positives[1]
    con = FALSE
  }
  
  roots = numeric(1000)
 
  i = 1
  j = 1
  up1 = length(positives)
  up2 = length(negatives)
  while(low < high) {
    if (con) {
      lows = positives[i:up1]
    }
    else {
      lows = negatives[i:up2]
    }
    
    nums = which(lows < high)
    len = length(nums)
    low = lows[nums[len]]
    #print(c(low, high))
    
    # save each root
    roots[j] = root(f, c(low, high))
    j = j + 1
    
    low = high
    if (nums[len] == length(lows)) 
      high = lows[nums[len]]
    else
      high = lows[nums[len] + 1]
    
    if (con) 
      i = which(negatives == low)
    else 
      i = which(positives == low)
    
    con = !con
  }
  
  roots[1:(j - 1)]
}

allroot(f, c(0, 50))
allroot(f, c(0, 100))
