seq(1, 28, by = 3)
paste(rep(letters[-1:-23], 3:1), rep(letters[1:3], 2), sep = "")
rep(1:3 < 2, 2) 
cumsum(10^(seq(0, 8, by = 2)))
1:4 + rep(0:3, rep(4, 4))

setwd("E:/My Documents/UoA Study/assignments/782")
csvfile = paste(getwd(), "/eurocities.csv", sep = "")
eurocities.df = read.csv(csvfile, stringsAsFactors = FALSE)
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

findcity.min = function(df, col, cityvec) {
  rows = rownames(df)
  rownum = nrow(df)
  colnum = ncol(df)
  distances = numeric(rownum %/% 2)
  cityvec.sort = sort(cityvec)
  for (i in 2:rownum) {
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
    for (m in 1:floor(n / 2)) {
      res = res + (-1)^m / (factorial(m) * factorial(n - 2 * m)) * x^(n - 2 * m) / 2^m
      print(res)
    }
    print(factorial(n) * res)
  }
}

hemite1(seq(-2, 2, by = .2), 5)

hemite2 = function(xseq, n) {
  for (x in xseq) {
    upval = floor(n / 2)
    mrange = 1:upval
    res = factorial(n) * sum((-1)^mrange 
                             / (factorial(mrange) * factorial(n - 2 * mrange))
                             * x^(n - 2 * mrange) / 2^mrange)
    print(res)
  }
}

hemite2(seq(-2, 2, by = .2), 5)

hemite3.1 = function(x, n) {
  m = floor(n / 2)
  factorial(n) * ((-1)^m / (factorial(m) * factorial(n - 2 * m)) * x^(n - 2 * m) / 2^m)
}

hemite3.1(seq(-2, 2, by = .2), 1)

hemite3 = function(x, n) {
  upval = floor(n / 2)
  mrange = 1:upval

  res = factorial(n) * sum((-1)^mrange 
                           / (factorial(mrange) * factorial(n - 2 * mrange))
                           * x^(n - 2 * mrange) / 2^mrange)
  res
}

x = as.matrix(seq(-2, 2, by = .2))
x

apply(x, 2, hemite3(x, 5))


