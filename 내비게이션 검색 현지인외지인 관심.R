library(readxl) ; library(dplyr) ; library(factoextra) ; library(tidyr)
library(purrr) ; library(readr) ; library(stringr)

getwd()
#───────────────────────────────────────────────────────────────────────────────
#[데이터 불러오기]

# -------------------------------
# 1. 공통 함수 정의
# -------------------------------
read_region_data <- function(pattern, prefix) {
  files <- list.files("./데이터/", pattern = pattern, full.names = TRUE)
  
  map_dfr(files, function(f) {
    # 파일명에서 시군구명 추출
    region <- gsub(prefix, "", basename(f))
    region <- gsub("\\.csv", "", region)
    
    df <- read.csv(f, fileEncoding = "UTF-8")
    
    df %>%
      select(-검색율) %>%
      rename(외지인관심 = X) %>%
      pivot_wider(
        names_from = 업종중분류명,
        values_from = 외지인관심
      ) %>%
      mutate(시군구명 = region) %>%
      select(시군구명, everything())
  })
}

# -------------------------------
# 2. 지역별 데이터 불러오기
# -------------------------------
seoul_int    <- read_region_data("내비게이션 검색 현지인외지인 관심지점_서울_서울.*\\.csv",
                                 "내비게이션 검색 현지인외지인 관심지점_서울_")
busan_int    <- read_region_data("내비게이션 검색 현지인외지인 관심지점_부산_.*\\.csv",
                                 "내비게이션 검색 현지인외지인 관심지점_부산_")
daegu_int    <- read_region_data("내비게이션 검색 현지인외지인 관심지점_대구_.*\\.csv",
                                 "내비게이션 검색 현지인외지인 관심지점_대구_")
incheon_int  <- read_region_data("내비게이션 검색 현지인외지인 관심지점_인천_.*\\.csv",
                                 "내비게이션 검색 현지인외지인 관심지점_인천_")
gwangju_int  <- read_region_data("내비게이션 검색 현지인외지인 관심지점_광주_.*\\.csv",
                                 "내비게이션 검색 현지인외지인 관심지점_광주_")
daejeon_int  <- read_region_data("내비게이션 검색 현지인외지인 관심지점_대전_.*\\.csv",
                                 "내비게이션 검색 현지인외지인 관심지점_대전_")
ulsan_int    <- read_region_data("내비게이션 검색 현지인외지인 관심지점_울산_.*\\.csv",
                                 "내비게이션 검색 현지인외지인 관심지점_울산_")
sejong_int   <- read_region_data("내비게이션 검색 현지인외지인 관심지점_세종_.*\\.csv",
                                 "내비게이션 검색 현지인외지인 관심지점_세종_")
gyeonggi_int <- read_region_data("내비게이션 검색 현지인외지인 관심지점_경기_.*\\.csv",
                                 "내비게이션 검색 현지인외지인 관심지점_경기_")
chungbuk_int <- read_region_data("내비게이션 검색 현지인외지인 관심지점_충북_.*\\.csv",
                                 "내비게이션 검색 현지인외지인 관심지점_충북_")
chungnam_int <- read_region_data("내비게이션 검색 현지인외지인 관심지점_충남_.*\\.csv",
                                 "내비게이션 검색 현지인외지인 관심지점_충남_")
jeonnam_int  <- read_region_data("내비게이션 검색 현지인외지인 관심지점_전남_.*\\.csv",
                                 "내비게이션 검색 현지인외지인 관심지점_전남_")
gyeongbuk_int<- read_region_data("내비게이션 검색 현지인외지인 관심지점_경북_.*\\.csv",
                                 "내비게이션 검색 현지인외지인 관심지점_경북_")
gyeongnam_int<- read_region_data("내비게이션 검색 현지인외지인 관심지점_경남_.*\\.csv",
                                 "내비게이션 검색 현지인외지인 관심지점_경남_")
jeju_int     <- read_region_data("내비게이션 검색 현지인외지인 관심지점_제주_.*\\.csv",
                                 "내비게이션 검색 현지인외지인 관심지점_제주특별자치_")
gangwon_int  <- read_region_data("내비게이션 검색 현지인외지인 관심지점_강원_.*\\.csv",
                                 "내비게이션 검색 현지인외지인 관심지점_강원특별자치_")
jeonbuk_int  <- read_region_data("내비게이션 검색 현지인외지인 관심지점_전북_.*\\.csv",
                                 "내비게이션 검색 현지인외지인 관심지점_전북특별자치도_")

# -------------------------------
# 3. 전체 지역 합치기
# -------------------------------
all_regions_int <- bind_rows(
  seoul_int, busan_int, daegu_int, incheon_int, gwangju_int,
  daejeon_int, ulsan_int, sejong_int, gyeonggi_int,
  chungbuk_int, chungnam_int, jeonnam_int,
  gyeongbuk_int, gyeongnam_int, jeju_int,
  gangwon_int, jeonbuk_int
)

# 확인
head(all_regions_int)

save(all_regions_int, file = "./r_data/all_regions_int.rdata")
#───────────────────────────────────────────────────────────────────────────────


