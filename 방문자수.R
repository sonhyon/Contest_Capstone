library(readxl) ; library(dplyr) ; library(factoextra) ; library(tidyr)
library(purrr) ; library(readr) ; library(stringr)

getwd()
#───────────────────────────────────────────────────────────────────────────────

# -------------------------------
# 1. 방문자수 불러오기 공통 함수
# -------------------------------
read_visitors <- function(pattern, prefix) {
  files <- list.files("./데이터/", pattern = pattern, full.names = TRUE)
  
  map_dfr(files, function(f) {
    # 파일명에서 시군구명 추출
    region <- gsub(prefix, "", basename(f))
    region <- gsub("\\.csv", "", region)
    
    df <- read.csv(f, fileEncoding = "UTF-8")
    
    df %>%
      slice(1) %>%   # 첫 행만 선택
      transmute(
        시군구명 = region,
        방문객 = 방문자수   # "총방문자수" 대신 "방문객" 변수명 사용
      )
  })
}

# -------------------------------
# 2. 지역별 데이터 불러오기
# -------------------------------
seoul    <- read_visitors("방문자수_서울_.*\\.csv", "방문자수_서울_")
busan    <- read_visitors("방문자수_부산_.*\\.csv", "방문자수_부산_")
daegu    <- read_visitors("방문자수_대구_.*\\.csv", "방문자수_대구_")
incheon  <- read_visitors("방문자수_인천_.*\\.csv", "방문자수_인천_")
gwangju  <- read_visitors("방문자수_광주_.*\\.csv", "방문자수_광주_")
daejeon  <- read_visitors("방문자수_대전_.*\\.csv", "방문자수_대전_")
ulsan    <- read_visitors("방문자수_울산_.*\\.csv", "방문자수_울산_")
sejong   <- read_visitors("방문자수_세종_.*\\.csv", "방문자수_세종_")
gyeonggi <- read_visitors("방문자수_경기_.*\\.csv", "방문자수_경기_")
chungbuk <- read_visitors("방문자수_충북_.*\\.csv", "방문자수_충북_")
chungnam <- read_visitors("방문자수_충남_.*\\.csv", "방문자수_충남_")
jeonnam  <- read_visitors("방문자수_전남_.*\\.csv", "방문자수_전남_")
gyeongbuk<- read_visitors("방문자수_경북_.*\\.csv", "방문자수_경북_")
gyeongnam<- read_visitors("방문자수_경남_.*\\.csv", "방문자수_경남_")
jeju     <- read_visitors("방문자수_제주_.*\\.csv", "방문자수_제주_")
gangwon  <- read_visitors("방문자수_강원_.*\\.csv", "방문자수_강원_")
jeonbuk  <- read_visitors("방문자수_전북_.*\\.csv", "방문자수_전북_")

# -------------------------------
# 3. 전체 지역 합치기
# -------------------------------
all_regions_visitor <- bind_rows(
  seoul, busan, daegu, incheon, gwangju, daejeon, ulsan, sejong,
  gyeonggi, chungbuk, chungnam, jeonnam, gyeongbuk, gyeongnam,
  jeju, gangwon, jeonbuk
)

# -------------------------------
# 4. 확인
# -------------------------------
head(all_regions_visitor)

save(all_regions_visitor, file = "./r_data/all_regions_visitor.rdata")
load("./r_data/all_regions_visitor.rdata")

