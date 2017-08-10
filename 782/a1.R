seq(1, 28, by = 3)
paste(rep(letters[-1:-23], 3:1), rep(letters[1:3], 2), sep = "")
rep(1:3 < 2, 2) 
cumsum(10^(seq(0, 8, by = 2)))
1:4 + rep(0:3, rep(4, 4))

setwd("E:/My Documents/UoA Study/assignments/782")
csvfile = paste(getwd(), "/eurocities.csv", sep = "")
eurocities.df = read.csv(csvfile)
str(eurocities.df)
head(eurocities.df)

distance.mean = mean(eurocities.df[,3])
distance.mean
di
