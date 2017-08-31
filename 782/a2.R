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
  c = length(which(conc.z != 0))
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


team.num = 30
nba.names = nba.df$team1[seq(1, length(nba.df$team1), length = team.num)]
nba.names

# likelihood.r = function(r, times) {
#   mtx = outer(r, r, function(ri, rj) ri / (ri + rj))
#   rankv = c(mtx[which(row(mtx) != col(mtx))]) ^ times
#   log(prod(rankv))
# }

log.likelihood.r = function(r, times, s) {
  rn = s - sum(r)
  rr = c(r, rn)
  
  if (all(rr > 0)) {
    mtx = outer(rr, rr, function(ri, rj) log(ri / (ri + rj)))
    rankv = c(t(mtx)[which(row(mtx) != col(mtx))])
    sum(times * rankv)
  } else {
    -Inf
  }
}

s = 1000
Q = function(r) {
  -log.likelihood.r(r, nba.df$wins, s)
}

result = optim(seq(1, team.num - 1, length = team.num - 1), 
               Q, method = "BFGS", 
               control = list(maxit = 200))
result

ratio = 100 / max(result$par)
r.value = result$par * ratio
rr.value = c (r.value, (s - sum(result$par)) * ratio)
rr.value

rank.table = data.frame(nba.names, rr.value, stringsAsFactors = FALSE)
ordered.rank = rank.table[order(rank.table$rr.value, decreasing = TRUE),]
colnames(ordered.rank) = c("name", "rank")
rownames(ordered.rank) = 1:team.num
ordered.rank

log.likelihood.r.deriv = function(r, times1, times2, s = 1000) {
  rlen = length(r)
  rr = c(r, s - sum(r))
  
  mtx1 = outer(rr, rr, function(ri, rj) rj / ri * (ri + rj))
  mtx2 = outer(rr, rr, function(ri, rj) 1 / (ri + rj))
  #deriv1 = c(t(mtx1),
  #           1 / r[i]) * times1[(rlen * (i - 1) + 1):(rlen * i)]
  deriv1 = c(t(mtx1)[which(row(mtx1) != col(mtx1))]) * times1
  dim(deriv1) = c(rlen + 1, rlen)
  #deriv2 = c(t(mtx2),
  #           1 / (s - sum(r))) * times2[(rlen * (i - 1) + 1):(rlen * i)]
  deriv2 = c(t(mtx2)[which(row(mtx2) != col(mtx2))]) * times2
  dim(deriv2) = c(rlen + 1, rlen)
  
  gradients = rowSums(deriv1[-(rlen + 1), ]) - sum(deriv1[rlen + 1, ]) - 
    rowSums(deriv2[-(rlen + 1), ]) + sum(deriv2[rlen + 1, ])
  print(gradients)
  gradients
}

Q.derivs = function(r) {
  order.team2 = order(nba.df$team2)
  wins.team2 = nba.df$wins[order.team2]
  #rlen = length(r)
  #gradients = numeric(rlen)
  #for (i in 1:rlen) {
  #  gradients[i] = log.likelihood.r.deriv(r, i, nba.df$wins, wins.team2)
  #}
  log.likelihood.r.deriv(r, nba.df$wins, wins.team2)
  #gradients
}

result.deriv = optim(seq(1, 29, length = 29), Q, Q.derivs,
                     method = "BFGS",
                     control = list(reltol = 1e-10))
result.deriv

ranks = c(result$par, s - sum(result$par))
ranks.sort = sort(ranks, decreasing = TRUE)
first2 = c(which(round(ranks) == round(ranks.sort[1])), 
           which(round(ranks) == round(ranks.sort[2])))
first2

Q2 = function(r1, r2) {
  m = max(length(r1), length(r2))
  if (length(r1) < m)
    r1 = rep(r1, length = m)
  if (length(r2) < m)
    r2 = rep(r1, length = m)
  
  ans = numeric(m)
  for (i in 1:m) {
    ranks[first2] = c(r1[i], r2[i])
    ans[i] = -log.likelihood.r(ranks[-length(ranks)], 
                               nba.df$wins, s)
  }
  
  ans
}

r1 = ranks[first2[1]] + seq(-10, 10, length = 101)
r2 = ranks[first2[2]] + seq(-10, 10, length = 101)
z = outer(r1, r2, Q2)
contour(r1, r2, z,
        xlab = paste("rank of", nba.names[first2[1]]),
        ylab = paste("rank of", nba.names[first2[2]]),
        levels = result$value + 2^(1:10) / 1000, 
        main = "Profile Log-likelihood")
points(ranks[first2[1]],ranks[first2[2]], col = "blue", pch = 1, cex = 1.5)
points(ranks[first2[1]],ranks[first2[2]], col = "red", pch = 19)

r1 = seq(40, 145, length = 1001)
r2 = seq(20, 100, length = 6)
z = outer(r1, r2, Q2)
par(mfrow = c(2, 3))
ran = range(z)
for(j in 1:length(r2)) {
  plot(r1, z[, j], ylim = ran, type = "l",
       main = paste("rank of", 
                    nba.names[first2[2]], 
                    "=", 
                    r2[j]),
       xlab = paste("rank of", nba.names[first2[1]]),
       ylab = "Q")
}



