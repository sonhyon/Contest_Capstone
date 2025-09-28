getwd()

library(tidyverse)

# 한글 → 영어 변환 테이블
region_map <- c(
  "서울" = "Seoul",
  "부산" = "Busan",
  "대구" = "Daegu",
  "인천" = "Incheon",
  "광주" = "Gwangju",
  "대전" = "Daejeon",
  "울산" = "Ulsan",
  "세종" = "Sejong",
  "경기" = "Gyeonggi",
  "강원" = "Gangwon",
  "충북" = "Chungbuk",
  "충남" = "Chungnam",
  "전북" = "Jeonbuk",
  "전남" = "Jeonnam",
  "경북" = "Gyeongbuk",
  "경남" = "Gyeongnam",
  "제주" = "Jeju"
)

read_visitors_all <- function(pattern = "방문자수_.*2020~2024\\.csv$",
                              prefix = "방문자수_") {
  
  files <- list.files("./데이터/기술통계/", pattern = pattern, full.names = TRUE)
  
  map_dfr(files, function(f) {
    # 파일명에서 지역 한글 추출
    region_kr <- basename(f) %>%
      str_remove(prefix) %>%
      str_remove("_2020~2024\\.csv$")
    
    # 영어 변환
    region_en <- region_map[region_kr]
    
    # 데이터 불러오기
    df <- read.csv(f, fileEncoding = "UTF-8")
    
    df %>%
      select(기준년월, 방문자수) %>%
      mutate(
        지역한글 = region_kr,
        지역영문 = region_en
      ) %>%
      select(지역한글, 지역영문, everything())   # 컬럼 순서 정리
  })
}

# 실행
all_regions <- read_visitors_all()
head(all_regions)



library(dplyr)
library(ggplot2)

# 지역별 총 방문자수 합계 계산해서 정렬
all_regions2 <- all_regions %>%
  group_by(지역한글) %>%
  summarise(총방문자수 = sum(방문자수)) %>%
  arrange(desc(총방문자수))

# factor 순서 적용
all_regions$지역한글 <- factor(all_regions$지역한글,
                           levels = all_regions2$지역한글)

# 그래프 그리기
ggplot(all_regions, aes(x = 기준년월, y = 방문자수, 
                        color = 지역한글, group = 지역한글)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "연도별 지역 방문자수 추이",
    x = "기준년월",
    y = "방문자수",
    color = "지역"   # 범례 제목
  ) +
  theme_minimal()

#───────────────────────────────────────────────────────────────────────────────
getwd()

# 불러오기
library(readxl)

# 엑셀 파일 읽기
Visitors <- read_excel("./데이터/기술통계/한국_여행_중_방문_권역_2024년도.xlsx")
head(Visitors)
View(Visitors)



library(dplyr)
library(tidyr)
library(ggplot2)

# 1) 월별(1월~12월) 데이터만 필터링
Visitors_month <- Visitors %>%
  filter(월별 %in% paste0(1:12, "월"))

# 2) wide → long 변환
Visitors_long <- Visitors_month %>%
  pivot_longer(cols = -월별, 
               names_to = "지역", 
               values_to = "방문자수")

# 3) 지역별 합계 기준으로 순위 정하기
region_order <- Visitors_long %>%
  group_by(지역) %>%
  summarise(총방문자 = sum(방문자수, na.rm = TRUE)) %>%
  arrange(desc(총방문자)) %>%
  pull(지역)

# 4) factor 레벨 설정
Visitors_long$지역 <- factor(Visitors_long$지역, levels = region_order)

# ⚡ 5) 월별 순서도 factor로 지정 (1월~12월 순서대로)
Visitors_long$월별 <- factor(Visitors_long$월별, 
                           levels = paste0(1:12, "월"))

# 6) 선그래프
ggplot(Visitors_long, aes(x = 월별, y = 방문자수, 
                          color = 지역, group = 지역)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "월별 지역별 방문자수 추이",
    x = "월",
    y = "방문자수(%)",
    color = "지역"
  ) +
  theme_minimal()

#───────────────────────────────────────────────────────────────────────────────
Visitors <- read_excel("./데이터/기술통계/한국_여행_중_방문_권역_2024년도.xlsx")
head(Visitors)


library(dplyr)
library(tidyr)
library(ggplot2)

# 1) 분기 데이터만 추출
Visitors_quarter <- Visitors %>%
  filter(월별 %in% c("1분기","2분기","3분기","4분기"))

# 2) wide → long 변환
Visitors_long_q <- Visitors_quarter %>%
  pivot_longer(cols = -월별, 
               names_to = "지역", 
               values_to = "방문자수")

# 3) 지역별 총 방문자 수 기준으로 순서 정렬
region_order_q <- Visitors_long_q %>%
  group_by(지역) %>%
  summarise(총방문자 = sum(방문자수, na.rm = TRUE)) %>%
  arrange(desc(총방문자)) %>%
  pull(지역)

Visitors_long_q$지역 <- factor(Visitors_long_q$지역, levels = region_order_q)

# 4) 선그래프
ggplot(Visitors_long_q, aes(x = 월별, y = 방문자수, 
                            color = 지역, group = 지역)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "분기별 지역별 방문자수 추이",
    x = "분기",
    y = "방문자수(%)",
    color = "지역"
  ) +
  theme_minimal()

#───────────────────────────────────────────────────────────────────────────────