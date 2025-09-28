load("./r_data/final_data.rdata")

final_data <- na.omit(final_data)
head(final_data)

# 데이터 준비 (시군구명 제외)
cluster_data <- final_data %>%
  select(-시군구명, -기타관광)

# 변수 표준화 (스케일링)
cluster_scaled <- scale(cluster_data)

# 최적의 군집 수 탐색 (엘보우 방법)
fviz_nbclust(cluster_scaled, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) # 예: 4개로 가정

# K-means 수행 (k=4 예시)
set.seed(123)
km_res <- kmeans(cluster_scaled, centers = 4, nstart = 25)

# 결과 확인
km_res$cluster

# 원본 데이터에 군집 번호 붙이기
final_clustered <- final_data %>%
  mutate(cluster = km_res$cluster)

head(final_clustered)

# 시각화 (PCA 차원축소 후 군집 표시)
fviz_cluster(km_res, data = cluster_scaled,
             geom = "point",
             ellipse.type = "convex",
             repel = TRUE) +
  theme_minimal()

# cluster별 평균값 계산
cluster_summary <- final_clustered %>%
  group_by(cluster) %>%
  summarise(across(-시군구명, mean, na.rm = TRUE))

cluster_summary


write.csv(final_clustered, "final_clustered.csv",
          row.names = FALSE,
          fileEncoding = "CP949")


#───────────────────────────────────────────────────────────────────────────────
#[ㅇㅅ부탁 군집분석]
getwd()

cluster_data <- read.csv("./은성님 요청 데이터.csv", fileEncoding = "CP949")
head(cluster_data)

# NA 제거 및 시군구명 제거
cluster_data <- na.omit(cluster_data) %>%
  select(-시군구명)

# 표준화
cluster_scaled <- scale(cluster_data)

# 엘보우 방법
fviz_nbclust(cluster_scaled, kmeans, method = "wss") +
  labs(title = "엘보우 방법으로 본 최적 군집 수")

# K-means (k = 4 예시)
set.seed(123)
km_res <- kmeans(cluster_scaled, centers = 4, nstart = 25)

# 원본 데이터에 군집 번호 추가
final_clustered <- cluster_data %>%
  mutate(cluster = km_res$cluster)

# 군집별 평균값 요약
cluster_summary <- final_clustered %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# PCA 차원축소 시각화
fviz_cluster(km_res,
             data = cluster_scaled,
             geom = "point",
             ellipse.type = "convex",
             repel = TRUE) +
  theme_minimal() +
  labs(title = "K-means 군집 결과 (PCA 차원 축소)")

cluster_summary
