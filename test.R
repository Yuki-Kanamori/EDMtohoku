require(rEDM)
packageVersion("rEDM") # 1.8.0




# 2種系のデータセットを作成
d0 <- as.data.frame(matrix(rep(NA,3*1000),ncol=3))
colnames(d0) <- c("time","x","y")
d0[,1] <- 1:1000
d0[1,2:3] <- c(0.1, 0.1) # 初期値
for(i in 1:999){ # Sugihara et al. (2012) Fig.1 のモデル
  d0[i+1,2] <- d0[i,2]*(3.8 - 3.80*d0[i,2] - 0.02*d0[i,3])
  d0[i+1,3] <- d0[i,3]*(3.5 - 0.10*d0[i,2] - 3.50*d0[i,3])
}

# 最初の 100 time step のデータを捨てる
d <- d0[100:1000,]

# 一部を表示してみる
plot(d$time, d$x, type = "l", xlim = c(100,150),
     las = 1, xlab = "Time", ylab = "Value", col = "royalblue")
lines(d$time, d$y, col = "red3") # Xが青・Yが赤です

# # スケーリング必須
# d$x <- as.numeric(scale(d$x)); d$y <- as.numeric(scale(d$y))




