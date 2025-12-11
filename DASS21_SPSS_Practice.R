# ==============================================================================

# [통합 실습] 사회조사분석사 2급 대비: 전처리 & DASS-12 척도 분석

# 흐름: 데이터 생성 -> (가상)오류 주입 -> 전처리(결측/역채점) -> 최종 분석

# ==============================================================================

# 1. 필수 라이브러리 로드

if(!require(lavaan)) install.packages("lavaan")

if(!require(semTools)) install.packages("semTools")

if(!require(psych)) install.packages("psych")

if(!require(dplyr)) install.packages("dplyr")

library(lavaan)

library(semTools)

library(psych)

library(dplyr)

# ==============================================================================

# STEP 1. 원본 데이터 생성 (논문의 통계적 특성 반영)

# ==============================================================================

cat("\n[STEP 1] 데이터 생성 중... (N=477)\n")

set.seed(1234)

N <- 477

# 1. 잠재 변수 생성 (높은 신뢰도를 위해 상관관계 부여)

# 우울, 불안, 스트레스가 서로 관련성이 높도록 설정

true_dep <- rnorm(N, mean=1.5, sd=1)

true_anx <- true_dep * 0.7 + rnorm(N, 0, 0.6) # 우울하면 불안할 확률 높음

true_str <- true_dep * 0.7 + rnorm(N, 0, 0.6)

# 2. 문항 점수 생성 함수 (잠재 변수 + 오차 -> 0~3점)

gen_item <- function(latent) {

  score <- round(latent + rnorm(N, mean=0, sd=0.6))

  pmax(0, pmin(3, score)) # 0~3점 범위 제한

}

# 3. 깨끗한 원본 데이터(myData_raw) 생성

myData_raw <- data.frame(

  ID = 1:N,

  # 그룹 변수 (점수가 높으면 환자군)

  Group = ifelse((true_dep + true_anx)/2 > 2.0, "Psychiatric", "Non-Psychiatric"),

  

  # [cite_start]Depression (3, 10, 13, 17) [cite: 112-116]

  i3  = gen_item(true_dep),

  i10 = gen_item(true_dep),

  i13 = gen_item(true_dep),

  i17 = gen_item(true_dep),

  

  # [cite_start]Anxiety (2, 4, 7, 19) [cite: 112-116]

  i2  = gen_item(true_anx),

  i4  = gen_item(true_anx),

  i7  = gen_item(true_anx),

  i19 = gen_item(true_anx),

  

  # [cite_start]Stress (1, 11, 12, 18) [cite: 112-116]

  i1  = gen_item(true_str),

  i11 = gen_item(true_str),

  i12 = gen_item(true_str),

  i18 = gen_item(true_str)

)

# 타당도 분석용 외부 변수

myData_raw$PHQ9 <- round(rowSums(myData_raw[,c("i3","i10","i13","i17")]) * 0.8 + rnorm(N, 2, 2))

myData_raw$RSES <- round(30 - rowSums(myData_raw[,c("i3","i10","i13","i17")]) * 0.5 + rnorm(N, 0, 3))

# ==============================================================================

# STEP 2. [시험 상황 연출] 데이터 오염시키기 (결측치 & 역채점)

# ==============================================================================

cat("\n[STEP 2] 연습을 위해 데이터를 오염시킵니다 (결측치 주입, 역코딩 적용)\n")

myData_messy <- myData_raw

# 1. 결측치(NA) 주입: i3번 문항의 일부 데이터를 지움

myData_messy$i3[sample(1:N, 10)] <- NA 

# 2. 역채점 상황 연출: i10번(우울) 문항을 '긍정 문항'으로 뒤집어버림

# 원래: 0(전혀 아님) ~ 3(매우 그렇다) -> 뒤집음: 3(전혀 아님) ~ 0(매우 그렇다)

myData_messy$i10 <- 3 - myData_messy$i10

# ==============================================================================

# STEP 3. [실전 전처리] 결측치 처리 & 역채점 (사조사 2급 핵심)

# ==============================================================================

cat("\n[STEP 3] 데이터 전처리 시작 (결측치 처리, 역채점)\n")

# 3-1. 결측치 확인 및 평균 대체 (Mean Imputation)

# 시험 문제: "결측치는 해당 문항의 평균값으로 대체하시오."

cat(">> 전처리 전 결측치 수 (i3):", sum(is.na(myData_messy$i3)), "\n")

# 평균 계산 (na.rm=TRUE 필수)

mean_i3 <- mean(myData_messy$i3, na.rm = TRUE)

# NA 자리에 평균값(반올림) 넣기

myData_messy$i3[is.na(myData_messy$i3)] <- round(mean_i3)

cat(">> 전처리 후 결측치 수 (i3):", sum(is.na(myData_messy$i3)), "(완료)\n")

# 3-2. 역채점 처리 (Reverse Coding)

# 시험 문제: "i10 문항은 역코딩 문항이다. 알맞게 변환하시오."

# 공식: (최대값 + 최소값) - 원래점수 = (3 + 0) - i10

myData_messy$i10_recoded <- 3 - myData_messy$i10

# 처리 결과 확인 (상관분석으로 검증)

# 역채점 전에는 총점과 음의 상관(-), 후에는 양의 상관(+)이 나와야 함

check_cor_before <- cor(myData_messy$i10, myData_messy$PHQ9)

check_cor_after  <- cor(myData_messy$i10_recoded, myData_messy$PHQ9)

cat(sprintf(">> 역채점 전 상관계수: %.2f (음수면 문제 있음)\n", check_cor_before))

cat(sprintf(">> 역채점 후 상관계수: %.2f (양수면 정상)\n", check_cor_after))

# 전처리 완료된 데이터셋 생성 (변수명 정리)

myData_final <- myData_messy

myData_final$i10 <- myData_final$i10_recoded # 덮어쓰기

# ==============================================================================

# STEP 4. [최종 분석] 논문 재현 (기술통계, 타당도, 신뢰도)

# ==============================================================================

cat("\n[STEP 4] 전처리된 데이터로 최종 분석 수행\n")

# ------------------------------------------------------------------------------

# 4-1. 기술통계 (Table 1: 평균, 왜도, 첨도)

# ------------------------------------------------------------------------------

cat("\n--- [Table 1] Descriptive Statistics ---\n")

dass_items <- c("i3","i10","i13","i17", "i2","i4","i7","i19", "i1","i11","i12","i18")

print(round(describe(myData_final[, dass_items])[c("mean","sd","skew","kurtosis")], 2))

# ------------------------------------------------------------------------------

# 4-2. CFA 구성 타당도 (Table 2)

# ------------------------------------------------------------------------------

cat("\n--- [Table 2] CFA Model Fit ---\n")

DASS12_Model <- '

  Depression =~ i3 + i10 + i13 + i17

  Anxiety    =~ i2 + i4 + i7 + i19

  Stress     =~ i1 + i11 + i12 + i18

'

# 부트스트래핑 적용 (시간 단축을 위해 500회 설정, 실제는 1000회 권장)

fit <- cfa(DASS12_Model, data = myData_final, estimator="ML", se="bootstrap", bootstrap=500)

print(round(fitMeasures(fit, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr")), 3))

# ------------------------------------------------------------------------------

# 4-3. 수렴/변별 타당도 (Table 3: 상관분석)

# ------------------------------------------------------------------------------

cat("\n--- [Table 3] Correlations (Validity) ---\n")

# 총점 계산

myData_final$Dep_Score <- rowSums(myData_final[, c("i3","i10","i13","i17")])

# 상관분석 (우울 총점 vs 외부 척도)

validity_check <- cor(myData_final %>% select(Dep_Score, PHQ9, RSES))

print(round(validity_check, 2))

cat(">> 해석: PHQ9(우울)와는 양의 상관, RSES(자존감)와는 음의 상관이어야 함.\n")

# ------------------------------------------------------------------------------

# 4-4. 집단 비교 타당도 (Table 4: T-test)

# ------------------------------------------------------------------------------

cat("\n--- [Table 4] Known-Groups Validity (T-test) ---\n")

print(t.test(Dep_Score ~ Group, data = myData_final, var.equal=FALSE))

cat(">> 해석: p-value < 0.05 이면 두 집단 간 유의미한 차이가 있음.\n")

# ------------------------------------------------------------------------------

# 4-5. 신뢰도 분석 (Table 5: Cronbach's Alpha)

# ------------------------------------------------------------------------------

cat("\n--- [Table 5] Reliability (Cronbach's Alpha) ---\n")

# 우울 척도 신뢰도

alpha_dep <- psych::alpha(myData_final[, c("i3","i10","i13","i17")], check.keys=TRUE)

cat("Depression Alpha:", round(alpha_dep$total$raw_alpha, 3), "\n")

cat(">> 해석: 0.7 이상이면 신뢰도 합격.\n")


