# ==============================================================================

# [통합 스크립트] DASS-12 척도 타당화 분석 (논문 재현)

# 특징: 논문과 동일한 Bootstrap 1000회 적용, DASS-12 문항 번호 정확히 반영

# ==============================================================================

# 1. 필수 패키지 로드

if(!require(lavaan)) install.packages("lavaan")

if(!require(semTools)) install.packages("semTools")

if(!require(psych)) install.packages("psych")

if(!require(dplyr)) install.packages("dplyr")

if(!require(MASS)) install.packages("MASS")

library(lavaan)

library(semTools)

library(psych)

library(dplyr)

library(MASS)

# ==============================================================================

# STEP 1. 논문 통계 특성을 반영한 고품질 데이터 생성

# ==============================================================================

cat("\n[STEP 1] 데이터 생성 중... (N=477, 논문 특성 반영)\n")

set.seed(1234) # 결과 재현을 위해 시드 고정

N_total <- 477 # 일반인 428 + 환자 49

# 1. 잠재 변수(Latent Variables) 생성 (상관관계가 높은 구조)

# 요인: Dep, Anx, Str, 기타 타당도 변수들

mu <- rep(0, 3) 

Sigma <- matrix(0.7, 3, 3) # 요인 간 상관관계 0.7로 설정 (높은 상관)

diag(Sigma) <- 1

latent_scores <- mvrnorm(N_total, mu, Sigma)

# 2. 그룹 변수 생성 (점수 상위 10%는 환자군으로 할당하여 Known-groups validity 확보)

# 실제 논문 비율(약 10%)과 비슷하게 맞춤

total_score_latent <- rowSums(latent_scores)

cutoff <- quantile(total_score_latent, 0.90)

group <- ifelse(total_score_latent > cutoff, "Psychiatric", "Non-Psychiatric")

# 3. 문항 점수 생성 함수 (잠재 변수 + 오차 -> 0~3점 Likert 변환)

gen_items <- function(latent_vec, n_items) {

  mat <- matrix(NA, nrow=length(latent_vec), ncol=n_items)

  for(i in 1:n_items) {

    # 논문의 정적 편포(Skewness)를 재현하기 위해 절단점을 높게 설정 (0점이 많이 나오게)

    val <- latent_vec + rnorm(length(latent_vec), 0, 0.5)

    mat[, i] <- as.integer(cut(val, breaks=c(-Inf, 0.5, 1.5, 2.5, Inf), labels=0:3))

  }

  return(mat)

}

# 4. 데이터프레임 조립 (DASS-21 전체 문항 21개 생성)

# DASS-12에 포함되는 문항들은 나중에 선택해서 씀

d_items <- gen_items(latent_scores[,1], 7) # 우울 7문항

a_items <- gen_items(latent_scores[,2], 7) # 불안 7문항

s_items <- gen_items(latent_scores[,3], 7) # 스트레스 7문항

myData <- data.frame(

  ID = 1:N_total,

  Group = group,

  # 문항 번호 할당 (논문의 문항 번호 체계에 따름)

  # Stress: 1, 6, 8, 11, 12, 14, 18

  i1=s_items[,1], i6=s_items[,2], i8=s_items[,3], i11=s_items[,4], i12=s_items[,5], i14=s_items[,6], i18=s_items[,7],

  # Anxiety: 2, 4, 7, 9, 15, 19, 20

  i2=a_items[,1], i4=a_items[,2], i7=a_items[,3], i9=a_items[,4], i15=a_items[,5], i19=a_items[,6], i20=a_items[,7],

  # Depression: 3, 5, 10, 13, 16, 17, 21

  i3=d_items[,1], i5=d_items[,2], i10=d_items[,3], i13=d_items[,4], i16=d_items[,5], i17=d_items[,6], i21=d_items[,7]

)

# 타당도 분석용 외부 변수 생성 (PHQ9, GAD7 등)

# DASS 점수와 높은 상관을 갖도록 생성

myData$PHQ9 <- round(rowSums(d_items) * 0.8 + rnorm(N_total, 5, 2))

myData$GAD7 <- round(rowSums(a_items) * 0.8 + rnorm(N_total, 4, 2))

myData$RSES <- round(30 - rowSums(d_items) * 0.5 + rnorm(N_total, 0, 3)) # 자존감은 음의 상관

cat("데이터 생성 완료. (N =", nrow(myData), ")\n")

# ==============================================================================

# [Table 1] 기술통계 (Mean, Skewness, Kurtosis)

# ==============================================================================

cat("\n\n=== [Table 1] Mean Scores & Distribution Parameters ===\n")

dass12_items <- c("i3","i10","i13","i17", "i2","i4","i7","i19", "i1","i11","i12","i18")

print(round(describe(myData[, dass12_items])[c("mean","sd","skew","kurtosis")], 2))

# ==============================================================================

# [Table 2] CFA (DASS-12) - 부트스트래핑 1000회 적용

# ==============================================================================

cat("\n\n=== [Table 2] CFA Model Fit (Bootstrap 1000, 잠시만 기다려주세요!) ===\n")

# DASS-12 모델 정의 (사용자 제공 텍스트 기반) 

# Depression: 3, 10, 13, 17

# Anxiety: 2, 4, 7, 19

# Stress: 1, 11, 12, 18

DASS12_Model <- '

  Depression =~ i3 + i10 + i13 + i17

  Anxiety    =~ i2 + i4 + i7 + i19

  Stress     =~ i1 + i11 + i12 + i18

'

# 분석 실행 (시간이 걸리는 구간)

# se="bootstrap", bootstrap=1000 옵션이 핵심입니다.

fit <- cfa(DASS12_Model, 

           data = myData, 

           estimator = "ML", 

           se = "bootstrap",      # 표준오차 부트스트랩

           test = "bootstrap",    # 카이제곱 부트스트랩

           bootstrap = 1000)      # 1000회 반복

# 적합도 출력

fit_idx <- fitMeasures(fit, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))

print(round(fit_idx, 3))

cat(">> 해석: CFI가 0.9 이상, RMSEA가 0.08 이하면 적합한 모델입니다.\n")

# ==============================================================================

# [Table 3] 수렴 및 변별 타당도 (상관분석)

# ==============================================================================

cat("\n\n=== [Table 3] Convergent & Discriminant Validity ===\n")

# DASS-12 하위 척도 총점 계산

myData$D12_Dep <- rowSums(myData[, c("i3","i10","i13","i17")])

myData$D12_Anx <- rowSums(myData[, c("i2","i4","i7","i19")])

myData$D12_Str <- rowSums(myData[, c("i1","i11","i12","i18")])

# 상관분석 (DASS vs PHQ9, GAD7, RSES)

validity_df <- myData %>% dplyr::select(D12_Dep, D12_Anx, D12_Str, PHQ9, GAD7, RSES)

cor_mat <- cor(validity_df)

print(round(cor_mat[1:3, 4:6], 2))

cat(">> 해석: PHQ9/GAD7과는 양(+)의 상관, RSES(자존감)와는 음(-)의 상관이어야 함.\n")

# ==============================================================================

# [Table 4] 집단 비교 타당도 (Known-Groups Validity)

# ==============================================================================

cat("\n\n=== [Table 4] Known-Groups Validity (T-test) ===\n")

# 그룹별 평균 점수 확인

print(aggregate(cbind(D12_Dep, D12_Anx, D12_Str) ~ Group, data = myData, FUN = mean))

# T-test (우울 점수 예시)

t_res <- t.test(D12_Dep ~ Group, data = myData, var.equal = FALSE)

print(t_res)

cat(">> 해석: p-value < 0.05 이고 환자군(Psychiatric) 점수가 높으면 타당도 확보.\n")

# ==============================================================================

# [Table 5] 신뢰도 분석 (Cronbach's Alpha)

# ==============================================================================

cat("\n\n=== [Table 5] Internal Consistency (Cronbach's Alpha) ===\n")

# 신뢰도 계산 함수

get_alpha <- function(items) {

  psych::alpha(myData[, items], check.keys=TRUE)$total$raw_alpha

}

a_dep <- get_alpha(c("i3","i10","i13","i17"))

a_anx <- get_alpha(c("i2","i4","i7","i19"))

a_str <- get_alpha(c("i1","i11","i12","i18"))

a_tot <- get_alpha(dass12_items)

cat(sprintf("Depression Alpha: %.3f\n", a_dep))

cat(sprintf("Anxiety Alpha   : %.3f\n", a_anx))

cat(sprintf("Stress Alpha    : %.3f\n", a_str))

cat(sprintf("Total Alpha     : %.3f\n", a_tot))

cat(">> 해석: 0.7 이상이면 적절, 0.8 이상이면 우수함.\n")

