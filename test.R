require(rEDM)
packageVersion("rEDM") # 1.8.0, iMacは1.8.1




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




# simplex projection ------------------------------------------------------
# 埋め込み次元と時間遅れを決める

# Yの時間遅れ時系列を作る(make_block(); rEDM)
# エラーが出る; 2:ncol(block) でエラー:  引数の長さが 0 です 
# y_embed <- make_block(d$y, max_lag = 2) # 暫定的に Y(t), Y(t-1)まで作成
y_embed = data.frame(time = rep(1:nrow(d)), col1 = d$y, col1_1 = c(NA, d[1:(nrow(d)-1), "y"]))
head(y_embed)

plot(y_embed$col1_1, y_embed$col1, col = rgb(0,0,0,0.5),
     xlab = "Y(t-1)", ylab = "Y(t)", las = 1)


#===== 再構成した動態y_embedの中から，近未来を予測したい点を設定する =====#
t_target = 240
y_embed[t_target,]
plot(y_embed$col1_1, y_embed$col1, col = rgb(0,0,0,0.5),
     xlab = "Y(t-1)", ylab = "Y(t)", las = 1)
points(y_embed[t_target,3:2], col = "black", bg = "red3", pch = 21, cex = 2)
# ターゲットを拡大
plot(y_embed$col1_1, y_embed$col1, col = rgb(0,0,0,0.5),
     xlab = "Y(t-1)", ylab = "Y(t)", las = 1,
     xlim = c(y_embed[t_target,3] - 0.005, y_embed[t_target,3] + 0.005),
     ylim = c(y_embed[t_target,2] - 0.005, y_embed[t_target,2] + 0.005))
points(y_embed[t_target,3:2], col = "black", bg = "red3", pch = 21, cex = 0.9)


#===== 最近傍点をユークリッド距離で見つける =====#
# デフォルトでは， (埋め込み次元+1) 個の最近傍点を選ぶ．今は埋め込みE=2

# t = 240 の点から各点への距離を計算
distance <- sqrt(rowSums((y_embed[,2:3] - c(y_embed[t_target, 2:3]))^2))
nn_index <- order(distance)[2:4] # 自分自身を除いて 3 点近い点を選ぶ. timeが返される
plot(y_embed$col1_1, y_embed$col1, col = rgb(0,0,0,0.5),
     xlab = "Y(t-1)", ylab = "Y(t)", las = 1,
     xlim = c(y_embed[t_target,3] - 0.005, y_embed[t_target,3] + 0.005),
     ylim = c(y_embed[t_target,2] - 0.005, y_embed[t_target,2] + 0.005))
points(y_embed[t_target,3:2], col = "black", bg = "red3", pch = 21, cex = 0.9)
points(y_embed[nn_index,3:2], col = "black", bg = "royalblue", pch = 21, cex = 0.9)


#===== 近未来予測 =====#
# 最近傍点の挙動を調べる
# 最近傍点の現在地
y_embed[nn_index,]

# 最近傍点の１時点未来
y_embed[nn_index + 1,]
plot(y_embed$col1_1, y_embed$col1, col = rgb(0,0,0,0.5),
     xlab = "Y(t-1)", ylab = "Y(t)", las = 1)
points(y_embed[t_target,3:2], col = "black", bg = "red3", pch = 21, cex = 1.2)
points(y_embed[nn_index+1,3:2], col = "black", bg = "green3", pch = 24, cex = 1.2)

# 動態がうまく再構成されていて，しっかり軌道がとらえられていれば，targetの挙動は最近傍点の挙動と似ているはず
# これを利用して，targetの1時点先を予測(より近い最近傍点の挙動を重視するために，重みをつける)
# 最短距離を取得
min_distance <- distance[nn_index][1]
# 最短距離で補正した重みを計算
weights <- exp(-distance[nn_index]/min_distance)
# 合計の重みを計算
total_weight <- sum(weights)
# 重み付け平均を計算
pred <- (weights %*% as.matrix(y_embed[nn_index + 1,3:2])) / total_weight　

pred[2] # 予測値
y_embed[t_target+1, 2] # 実測値
plot(y_embed$col1_1, y_embed$col1, col = rgb(0,0,0,0.5),
     xlab = "Y(t-1)", ylab = "Y(t)", las = 1,
     xlim = c(y_embed[t_target+1,3] - 0.001, y_embed[t_target+1,3] + 0.002),
     ylim = c(y_embed[t_target+1,2] - 0.02, y_embed[t_target+1,2] + 0.01))
points(y_embed[nn_index+1,3:2], col = "black", bg = "green3", pch = 24, cex = 1.2)
points(y_embed[t_target+1,3:2], col = "black", bg = "red3", pch = 24, cex = 1.2)
points(pred, col = "red3", pch = 4, cex = 2)
# => 全ての点をtargetとして計算する rEDM::simplex()を使う

# ===== rEDM::simplex()の実装 =====#
simp_res = rEDM::simplex(d$y, E = 2, stats_only = F) # 秒で出る
# stats_only = Fで，統計情報だけでなく予測値も出力される
simp_res$model_output[[1]][240,] #さっきの例
# simplex(time_series,                     # 解析する時系列 (1変数)
#         lib = c(1, NROW(time_series)),   # 埋め込みに使用するデータの範囲
#         pred = lib,                      # 予測値を出すデータの範囲
#         norm = 2,                        # 重み付けのための距離の計算方法
#         E = 1:10,                        # 試行する埋め込み次元の範囲
#         tau = 1,                         # 時間遅れの単位
#         tp = 1,                          # 何ステップ先を予測するか
#         num_neighbors = "e+1",           # 使用する最近傍点の数
#         stats_only = TRUE,               # 予測の統計情報のみを出力するのか、それとも予測値も実際に出力するのか
#         exclusion_radius = NULL,         # 最近傍点を探す条件として、予測したい点とどの程度時間的に近い点を除くか (数値を指定する)
#         epsilon = NULL,                  # 最近傍点を探す条件として、予測したい点とどの程度距離的に遠い点を除くか (数値を指定する)
#         silent = FALSE)                  # Warning message を出力するかどうか

# 1. 統計情報のみを返す
simplex(d$y, E = 2, silent = T)

# 2. 予測する未来の時間点をtp = 1:10と指定し，1時間点先から10時間点先までを予測．予測する未来が遠くなると予測精度が悪くなっていくので，予測値と実測値の相関係数rhoが下がって行ったり，誤差の指標maeやrmseが上がっていったりする．
simplex(d$y, E = 2, tp = 10, stats_only = T, silent = T)　#tpの書き方が変わったっぽい

# 3. データの半分をトレーニングデータとして動態の再構築に利用し，それを用いて残り半分を予測する．データが十分に長い場合の素行性能の評価として使える．データが短い場合にはlibとpredを完全に一致させることでleave-one-out cross validationを行う．
simplex(d$y, E = 2,
        lib = c(1, 450),     # 時間 t = 1-450 の点で動態を再構成
        pred = c(451, 901),  # 時間 t = 451-901 の点の1時間点先を予測
        tp = 1, stats_only = T, silent = T)


#===== tauの決め方 =====#
# いくつかのtauを試して予測性能を比較したり，データの持つ特性（遅れがあることが知られている場合など）に合わせて決める
# 自己相関関数や相互情報量の利用など，tauを決める方法もいくつか提案されている


#===== 最適埋め込み次元の推定 =====#
# 最近傍点のt+1の重み付き平均で予測がうまくいく場合は，注目する点と最近傍点の挙動が似ていて，状態空間上で近い点はほぼ同じように挙動している，ということ => 状態空間上で動態が滑らかに描けている
# 最近傍点のt+1の重み付き平均で予測がうまくいかない場合は，ある時点に同じような場所にあった最近傍点は，次の時点では全くバラバラの位置に移動していて，描いた軌道はぐちゃぐちゃだったということ
# 埋め込み = 真の動態と再構成した動態が1:1対応してかつ滑らかに対応している状態を指す


#=====　Eを変えて予測精度の変化を見てみる=====#
simp_res2 <- simplex(d$y, E = 1:10, silent = T)
plot(simp_res2$E, simp_res2$rmse, type = "b", xlab = "E", ylab = "RMSE", las = 1)



