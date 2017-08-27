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
fib(50)

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

sign.matrix = function(x) outer(x, x, function(x1, x2) sign(x1 - x2))

conc = function(x, y) {
  conc.mtx = sign.matrix(x)
  conc.mty = sign.matrix(y)
  conc.z = conc.mtx + conc.mty
  c = length(which(conc.z < 0 | conc.z > 0))
  n = length(x)
  c / (n * (n - 1)) 
}

conc(x = 1:5, y = c(3, 1, 4, 5, 2))

set.seed(782)
x = round(rnorm(1000))
y = x + round(rnorm(1000))
conc(x, y)

nba.df = read.csv("https://raw.githubusercontent.com/zzdxzhangzhi/assignments/master/782/NBA2016-2017.csv",
stringsAsFactors = FALSE)
names(nba.df) = c("team1", "team2", "wins")
head(nba.df)



likelihood.r = function(r, times) {
  mtx = outer(r, r, function(ri, rj) ri / (ri + rj))
  rankv = c(mtx[which(row(mtx) != col(mtx))]) ^ times
  log(prod(rankv))
}

log.likelihood.r = function(r, times, s) {
  rn = s - sum(r)
  rr = c(r, rn)
  
  if (all(rr > 0)) {
    mtx = outer(rr, rr, function(ri, rj) ri / (ri + rj))
    rankv = log(c(mtx[which(row(mtx) != col(mtx))]))
    sum(times * rankv)
  } else {
    -Inf
  }
}

s = 1000
Q = function(r) {
  -log.likelihood.r(r, nba.df$wins, s)
}

result = optim(rep(33, 29), Q, method = "BFGS")
result

ratio = 100 / result$par[which.max(result$par)]
r.value = result$par * ratio
rr.value = c (r.value, (s - sum(result$par)) * ratio)
rr.value

nba.names = nba.df$team1[seq(1, 870, length = 30)]
nba.names

rank.table = cbind(data.frame(nba.names), rr.value, stringsAsFactors = FALSE)
ordered.rank = rank.table[order(rank.table$rr.value, decreasing = TRUE),]
colnames(ordered.rank) = c("name", "rank")
rownames(ordered.rank) = 1:30
ordered.rank









