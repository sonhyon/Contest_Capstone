library(dplyr) ; library(stringr); library(readr)

load("./r_data/all_regions_int.rdata")
load("./r_data/all_regions_visitor.rdata")
Visitor_Stay_Characteristics <- read.csv("./데이터/방문자 체류특성_2024.09~2025.08.csv", fileEncoding = "UTF-8")

# 확인
head(all_regions_int)
head(all_regions_visitor)
head(Visitor_Stay_Characteristics)

# 1) Visitor_Stay_Characteristics 시군구명 정리
Visitor_Stay_Characteristics <- Visitor_Stay_Characteristics %>%
  mutate(
    시도 = str_remove(시도명, "특별시|광역시|자치시|도"),
    시군구명 = paste0(시도, 시군구명)
  ) %>%
  select(시군구명, 평균.체류시간, 평균.숙박일수)

# 2) 데이터 합치기
final_data <- all_regions_int %>%
  left_join(all_regions_visitor, by = "시군구명") %>%
  left_join(Visitor_Stay_Characteristics, by = "시군구명")

# 3) 확인
head(final_data)

save(final_data, file = "./r_data/final_data.rdata")
#───────────────────────────────────────────────────────────────────────────────
#[csv 파일로 저장]
write.csv(final_data, "final_data_cp949.csv",
          row.names = FALSE,
          fileEncoding = "CP949")


