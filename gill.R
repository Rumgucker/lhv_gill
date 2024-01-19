# local hidden variables (LHV)

# https://rpubs.com/gill1109/lhv-full

library("dplyr")
library("MASS")

set.seed(123456)		# zufallsgenerator preset

const_rad <- pi / 2	# R rechnet in rad
const_cnt <- 200		# anzahl der messungen

A <- c(rep(1, const_cnt / 2), rep(2, const_cnt / 2))
B <- c(rep(1, const_cnt / 4), rep(2, const_cnt / 4), rep(1, const_cnt / 4), rep(2, const_cnt / 4))
X <- rep(0, const_cnt)
Y <- rep(0, const_cnt)
S <- rep(0, const_cnt)

N <- 0:15
X1 <- (N %% 2) * 2 - 1
X2 <- ((N %/% 2) %% 2) * 2 - 1
Y1 <- ((N %/% 4) %% 2) * 2 - 1
Y2 <- ((N %/% 8) %% 2) * 2 - 1
S <- X1 * Y1 - X1 * Y2 + X2 * Y1 + X2 * Y2
lhvModel <- data.frame(X1, X2, Y1, Y2)[S >= +2, ]
View(lhvModel)

for (i in 1:const_cnt) {
	LHVlong <- slice_sample(lhvModel, n = const_cnt, replace = TRUE)

	X[A == 1] <- LHVlong$X1[A == 1]
	X[A == 2] <- LHVlong$X2[A == 2]
	Y[B == 1] <- LHVlong$Y1[B == 1]
	Y[B == 2] <- LHVlong$Y2[B == 2]
	XY <- X * Y

	rho11 <- sin(mean(XY[A == 1 & B == 1]) * const_rad)
	rho12 <- sin(mean(XY[A == 1 & B == 2]) * const_rad)
	rho21 <- sin(mean(XY[A == 2 & B == 1]) * const_rad)
	rho22 <- sin(mean(XY[A == 2 & B == 2]) * const_rad)
	S[i] <- rho11 - rho12 + rho21 + rho22

	#print(c(i, rho11, rho12, rho21, rho22, S[i]))
}

truehist(S, breaks = seq(from = 0, to = 5, by = 0.1), prob = FALSE, xlab = mean(S))

# END