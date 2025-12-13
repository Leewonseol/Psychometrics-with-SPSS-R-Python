# 필요한 라이브러리 로드
library(dplyr)
library(purrr) # list 조작을 위해
library(ggplot2) # 플로팅을 위해
library(tidyr) # unnest 함수 사용

# --- 1. 친사회적 행동 데이터 (Rhoads et al., 2021 기반) ---

# Cooperation Cluster Data (수정 및 재확인된 버전)
cooperation_data <- data.frame(
  Region = c("Inferior Frontal Gyrus", "Ventral Striatum/Caudate/sgACC", "Subgenual ACC", "MCC/Paracingulate", "Insula", "Hippocampus", "VTA", "VTA", "Postcentral Gyrus", "Supramarginal Gyrus/STG", "Thalamus", "Precuneus", "Cerebellum VIII", "Middle Occipital Gyrus", "Middle Occipital Gyrus"),
  Hemisphere = c("R", "L", "R", "B", "L", "L", "L", "R", "L", "L", "L", "L", "R", "L", "B"),
  SDM_Z = c(3.824, 4.682, 3.650, 3.853, 3.497, 3.408, 4.102, 3.814, 3.861, 3.888, 3.931, 3.744, 3.568, 4.292, 3.736),
  Cluster = "Cooperation",
  Source = "Rhoads_Prosocial_Cooperation"
)

# Equity Cluster Data
equity_data <- data.frame(
  Region = c("Inferior Frontal Gyrus", "Middle Frontal Gyrus", "Middle Frontal Gyrus/Sup Orbital", "ACC/Paracingulate (Equity)", "Inferior Frontal Gyrus (Orbital)", "Superior Frontal Gyrus (Medial)", "Inferior Frontal Gyrus (Opercular)", "Striatum", "Middle Occipital Gyrus"),
  Hemisphere = c("L", "R", "R", "B", "L", "R", "R", "L", "L"),
  SDM_Z = c(2.366, 2.093, 2.391, 2.731, 2.302, 1.957, 1.996, 1.994, 2.390),
  Cluster = "Equity",
  Source = "Rhoads_Prosocial_Equity"
)

# Altruism Cluster Data
altruism_data <- data.frame(
  Region = c("Middle Orbital Gyrus", "Middle Frontal Gyrus", "ACC/Paracingulate/pre-SMA", "Gyrus Rectus (vmPFC)", "Middle Frontal Gyrus", "Anterior Insula", "Anterior Insula", "Inferior Frontal Gyrus (Orbital)", "Ventral Striatum", "Thalamus", "Thalamus", "Inferior Parietal", "Inferior Parietal", "Precuneus", "Precuneus", "Middle Occipital Gyrus", "Inferior Occipital Gyrus"),
  Hemisphere = c("L", "R", "B", "B", "L", "L", "R", "L", "R", "L", "L", "L", "R", "L", "R", "L", "R"),
  SDM_Z = c(3.397, 2.827, 3.871, 2.564, 3.761, 3.961, 2.923, 2.982, 3.150, 2.814, 2.803, 3.917, 3.105, 2.830, 2.960, 2.761, 2.594),
  Cluster = "Altruism",
  Source = "Rhoads_Prosocial_Altruism"
)

# 친사회적 데이터 통합
prosocial_regions_df_raw <- rbind(cooperation_data, equity_data, altruism_data)

# 표준화된 뇌 영역 이름 생성 및 클러스터 정보 유지
prosocial_regions_df <- prosocial_regions_df_raw %>%
  mutate(Simple_Region = case_when(
    grepl("Ventral Striatum", Region) ~ "Ventral Striatum", grepl("Striatum", Region) ~ "Striatum",
    grepl("Subgenual ACC", Region) ~ "Subgenual ACC", grepl("ACC/Paracingulate", Region) ~ "ACC/Paracingulate",
    grepl("MCC/Paracingulate", Region) ~ "Mid-Cingulate Cortex", grepl("Insula", Region) & !grepl("Anterior", Region) ~ "Insula",
    grepl("Anterior Insula", Region) ~ "Anterior Insula", grepl("Frontal Gyrus", Region) & grepl("Orbital", Region) ~ "Orbital Frontal Gyrus",
    grepl("Frontal Gyrus", Region) & grepl("Opercular", Region) ~ "Opercular Frontal Gyrus", grepl("Inferior Frontal Gyrus", Region) ~ "Inferior Frontal Gyrus",
    grepl("Middle Frontal Gyrus", Region) ~ "Middle Frontal Gyrus", grepl("Superior Frontal Gyrus", Region) ~ "Superior Frontal Gyrus",
    grepl("Gyrus Rectus", Region) ~ "vmPFC/Gyrus Rectus", grepl("Supramarginal Gyrus", Region) ~ "Supramarginal Gyrus",
    grepl("Inferior Parietal", Region) ~ "Inferior Parietal Lobule", grepl("Postcentral Gyrus", Region) ~ "Postcentral Gyrus",
    grepl("Middle Occipital Gyrus", Region) ~ "Middle Occipital Gyrus", grepl("Inferior Occipital Gyrus", Region) ~ "Inferior Occipital Gyrus",
    grepl("Cerebellum", Region) ~ "Cerebellum", TRUE ~ as.character(Region)
  )) %>%
  mutate(Standardized_Name_Base = paste(Simple_Region, paste0("(", Hemisphere, ")"))) %>%
  rowwise() %>%
  mutate(Standardized_Name_List = list(
    if (Hemisphere == "B") {
      c(paste(Simple_Region, "(L)"), paste(Simple_Region, "(R)"))
    } else {
      paste(Simple_Region, paste0("(", Hemisphere, ")"))
    }
  )) %>%
  ungroup() %>%
  unnest(Standardized_Name_List) %>%
  select(-Standardized_Name_Base) %>%
  rename(Standardized_Name = Standardized_Name_List) %>%
  # 각 표준화된 이름에 대해 클러스터 정보 결합
  group_by(Standardized_Name) %>%
  summarise(
    Prosocial_Clusters = list(unique(Cluster)), # 이 영역이 속한 친사회적 클러스터 목록
    .groups = 'drop'
  )

prosocial_standardized_names <- prosocial_regions_df$Standardized_Name

# --- 2. 반사회적 행동 데이터 (논문 리뷰 기반 + 방향성 매핑용 리스트) ---
# 방향성 매핑을 위한 리스트 (이름은 표준화 이전 이름 사용)
antisocial_direction_map <- list(
  # Aoki et al. (GMV)
  `Right Lentiform Nucleus` = "GMV Decrease (Aoki)", `Left Insula` = "GMV Decrease (Aoki)", `Left Frontopolar Cortex (FPC)` = "GMV Decrease (Aoki)",
  `Right Fusiform Gyrus` = "GMV Increase (Aoki)", `Right Inferior Parietal Lobule` = "GMV Increase (Aoki)", `Right Superior Parietal Lobule` = "GMV Increase (Aoki)",
  `Right Cingulate Gyrus` = "GMV Increase (Aoki)", `Right Postcentral Gyrus` = "GMV Increase (Aoki)",
  # Wong et al. (Aberrant Activation)
  `Right Precuneus` = "Aberrant Activation (Wong TA Overall & Exec)", `Right Rolandic Operculum` = "Aberrant Activation (Wong TA Exec)",
  `Midcingulate Cortex (MCC)` = "Aberrant Activation (Wong TA Exec)", `Precentral Gyrus` = "Aberrant Activation (Wong TA Exec)",
  `Left Postcentral Gyrus` = "Aberrant Activation (Wong EA)",
  # Dugré et al. (Hypo/Hyper Activation)
  `Bilateral dACC` = "Hypoactivation (Dugré Threat)", `Bilateral SMA/MCC` = "Hypoactivation (Dugré Threat)",
  `Bilateral Anterior Insula/IFG triang` = "Hypoactivation (Dugré Threat)", `Bilateral Middle Occipital Gyrus` = "Hypoactivation (Dugré Threat)",
  `Right dlPFC` = "Hypoactivation (Dugré Threat); Hyperactivation (Dugré Social)", `Left Inferior Parietal Gyrus` = "Hypoactivation (Dugré Threat)",
  `Left Inferior Temporal Gyrus` = "Hypoactivation (Dugré Threat)", `Left Cerebellum Lobule IV` = "Hyperactivation (Dugré Punish)",
  `Midbrain Tegmentum` = "Hyperactivation (Dugré Punish)", `Left Premotor Cortex` = "Hypoactivation (Dugré Punish & Control)",
  `Left Putamen` = "Hyperactivation (Dugré Social)", `Precuneus` = "Hyperactivation (Dugré Social)", `Medial PFC` = "Hyperactivation (Dugré Social)",
  `Right Mid-Cingulate Cortex (MCC)` = "Hypoactivation (Dugré Social)", `Right Lingual Gyrus` = "Hypoactivation (Dugré Social)",
  `Left Hippocampus` = "Hypoactivation (Dugré Social)", `Right Inferior Frontal Gyrus (IFG triang)` = "Hypoactivation (Dugré Social)",
  `Left Fusiform Gyrus` = "Hypoactivation (Dugré Social)", `Left Cerebellum Lobule VI/Crus I` = "Hypoactivation (Dugré Control)",
  `Left Anterior Insula` = "Hypoactivation (Dugré Control)", `Right Middle Temporal Gyrus` = "Hypoactivation (Dugré Control)",
  `Right Cerebellum Crus I/Lobule VI` = "Hypoactivation (Dugré Control)", `Right Ventrolateral PFC` = "Hypoactivation (Dugré Control)",
  `Left Supramarginal Gyrus` = "Hypoactivation (Dugré Control)",
  # Zhang & Peng (Self-Regulation Conjunction)
  `Dorsal Anterior Cingulate Cortex (dACC)` = "Self-Regulation Hub (Zhang)", `Bilateral Anterior Insula (AI)` = "Self-Regulation Hub (Zhang)",
  `Right Inferior Parietal Lobule (IPL)` = "Self-Regulation Hub (Zhang)"
)

# 표준화 함수 (이전과 동일)
standardize_antisocial_name <- function(region_string) {
  region_string <- trimws(region_string)
  hemisphere <- case_when( grepl("Bilateral", region_string) ~ "B", grepl("Left", region_string) ~ "L", grepl("Right", region_string) ~ "R", grepl("\\(L\\)", region_string) ~ "L", grepl("\\(R\\)", region_string) ~ "R", grepl("\\(B\\)", region_string) ~ "B", grepl("Midbrain|Medial PFC", region_string) ~ "M", grepl("MCC|Precuneus|dACC", region_string) & !grepl("Left|Right", region_string) ~ "M", TRUE ~ "Unknown" )
  base_region <- region_string
  base_region <- gsub("Bilateral |Left |Right |\\(L\\)|\\(R\\)|\\(B\\)", "", base_region)
  base_region <- gsub("\\(FPC\\)|\\(IPL\\)|\\(AI\\)|\\(BA6\\)|\\(IFG triang\\)|Lobule IV|Lobule VI|Crus I|triang|Operculum|Tegmentum", "", base_region)
  base_region <- trimws(base_region)
  base_region <- case_when( grepl("Lentiform Nucleus|Putamen", base_region) ~ "Striatum/Lentiform", grepl("Insula", base_region) & grepl("Anterior", region_string) ~ "Anterior Insula", grepl("Insula", base_region) ~ "Insula", grepl("ACC|Cingulate", base_region) & grepl("dACC|Dorsal", region_string) ~ "Dorsal ACC", grepl("ACC|Cingulate", base_region) & grepl("MCC|Mid-Cingulate", region_string) ~ "Mid-Cingulate Cortex", grepl("Cingulate Gyrus", base_region) ~ "Cingulate Gyrus", grepl("Frontopolar Cortex", base_region) ~ "Frontopolar Cortex", grepl("Fusiform Gyrus", base_region) ~ "Fusiform Gyrus", grepl("Inferior Parietal", base_region) ~ "Inferior Parietal Lobule", grepl("Superior Parietal", base_region) ~ "Superior Parietal Lobule", grepl("Postcentral Gyrus", base_region) ~ "Postcentral Gyrus", grepl("Precuneus", base_region) ~ "Precuneus", grepl("Rolandic", base_region) ~ "Rolandic Operculum", grepl("Precentral Gyrus|Premotor cortex", base_region) ~ "Precentral/Premotor Cortex", grepl("Middle Occipital Gyrus", base_region) ~ "Middle Occipital Gyrus", grepl("Inferior Temporal Gyrus", base_region) ~ "Inferior Temporal Gyrus", grepl("Middle Temporal Gyrus", base_region) ~ "Middle Temporal Gyrus", grepl("dlPFC|vlPFC|PFC|Frontal Gyrus", base_region) ~ "Prefrontal Cortex (General/Subregion)", grepl("Hippocampus", base_region) ~ "Hippocampus", grepl("Lingual Gyrus", base_region) ~ "Lingual Gyrus", grepl("Supramarginal Gyrus", base_region) ~ "Supramarginal Gyrus", grepl("Cerebellum", base_region) ~ "Cerebellum", grepl("Midbrain", base_region) ~ "Midbrain", TRUE ~ base_region )
  standardized_name <- if (hemisphere == "M") { paste(base_region, "(Midline/B)") } else if (hemisphere != "Unknown" && hemisphere != "B") { paste(base_region, paste0("(", hemisphere, ")")) } else if (hemisphere == "B") { region_string } else { paste(base_region, "(Hemisphere Unknown)") }
  return(standardized_name)
}
# 반사회적 행동 데이터 프레임 생성 (표준화된 이름 및 방향성 포함)
antisocial_regions_df <- map_df(names(antisocial_direction_map), function(raw_name) {
  standardized_list <- standardize_antisocial_name(raw_name)
  direction <- antisocial_direction_map[[raw_name]]
  if (grepl("Bilateral", raw_name)) {
      base_region <- gsub("Bilateral ", "", raw_name); base_region <- trimws(gsub("\\(.*\\)", "", base_region))
      standardized_l <- standardize_antisocial_name(paste("Left", base_region)); standardized_r <- standardize_antisocial_name(paste("Right", base_region))
      if (!grepl("Unknown", standardized_l) && !grepl("Unknown", standardized_r)) { standardized_names_final <- c(standardized_l, standardized_r) }
      else { if(grepl("\\(Midline/B\\)", standardized_list)) { standardized_names_final <- standardized_list } else { standardized_names_final <- paste(standardized_list, "(Bilateral Source)") } }
  } else { standardized_names_final <- standardized_list }
  tibble( Raw_Name = raw_name, Standardized_Name = standardized_names_final, Direction_Summary = direction,
    Source_Paper = case_when( grepl("Aoki", direction) ~ "Aoki et al.", grepl("Wong", direction) ~ "Wong et al.", grepl("Dugré", direction) ~ "Dugré et al.", grepl("Zhang", direction) ~ "Zhang & Peng", TRUE ~ "Unknown" ),
    Study_Type = case_when( grepl("GMV", direction) ~ "VBM (GMV)", grepl("Activation|Hub", direction) ~ "fMRI (Activation)", TRUE ~ "Unknown" )
  )
}) %>% distinct(Standardized_Name, .keep_all = TRUE)
antisocial_standardized_names <- unique(antisocial_regions_df$Standardized_Name)
antisocial_standardized_names <- antisocial_standardized_names[!grepl("Unknown|General/Subregion \\(Unknown\\)|Bilateral Source", antisocial_standardized_names)]

# --- 3. 비교 분석 ---
common_regions_names <- intersect(prosocial_standardized_names, antisocial_standardized_names)
unique_prosocial_names <- setdiff(prosocial_standardized_names, antisocial_standardized_names)
unique_antisocial_names <- setdiff(antisocial_standardized_names, prosocial_standardized_names)

# --- 4. 플로팅용 데이터 생성 (클러스터 정보 포함) ---

# 공통 영역: 친사회적 클러스터 정보와 반사회적 방향성 결합
common_df_plot <- prosocial_regions_df %>%
  filter(Standardized_Name %in% common_regions_names) %>%
  left_join(antisocial_regions_df %>% select(Standardized_Name, Antisocial_Direction = Direction_Summary), by = "Standardized_Name") %>%
  mutate(
    Prosocial_Direction = paste("Increase (Rhoads -", sapply(Prosocial_Clusters, paste, collapse = "/"), ")"), # 클러스터 정보 추가
    Category = "Common"
  ) %>%
  select(Standardized_Name, Prosocial_Direction, Antisocial_Direction, Category, Prosocial_Clusters) # Prosocial_Clusters 유지

# 친사회적 고유 영역: 클러스터 정보 포함
prosocial_df_plot <- prosocial_regions_df %>%
  filter(Standardized_Name %in% unique_prosocial_names) %>%
  mutate(
    Direction_Summary = paste("Increase (Rhoads -", sapply(Prosocial_Clusters, paste, collapse = "/"), ")"), # 클러스터 정보 추가
    Category = "Prosocial Unique"
  ) %>%
  select(Standardized_Name, Direction_Summary, Category, Prosocial_Clusters) # Prosocial_Clusters 열 유지

# 반사회적 고유 영역 (이전과 동일)
antisocial_df_plot <- antisocial_regions_df %>%
  filter(Standardized_Name %in% unique_antisocial_names) %>%
  mutate(Category = "Antisocial Unique") %>%
  rename(Direction_Summary = Direction_Summary) %>% # 이름 통일
  select(Standardized_Name, Direction_Summary, Category)


# --- 5. 개별 ggplot 생성 함수 (y 라벨 수정, 클러스터별 시각화) ---

# 공통 영역 플롯 생성 함수 (클러스터별 시각화 추가)
create_common_region_plot <- function(df, plot_title) {
  if (nrow(df) == 0) { return(ggplot() + annotate("text", x=0.5, y=0.5, label="해당 카테고리에\n보고된 뇌 영역 없음") + theme_void() + labs(title = plot_title)) }

  # 각 영역이 속한 클러스터 정보 unnest
  df_unnested <- df %>% unnest(Prosocial_Clusters)

  df_unnested <- df_unnested %>%
    mutate(Label = paste0(Standardized_Name, "\n  Prosocial: ", Prosocial_Direction, "\n  Antisocial: ", Antisocial_Direction))

  df_unnested$x_pos <- 1

  plot <- ggplot(df_unnested, aes(x = x_pos, y = reorder(Label, desc(Standardized_Name)))) +
    geom_point(aes(color = Prosocial_Clusters, shape = Prosocial_Clusters), size = 3) + # 클러스터별 색상/모양
    scale_color_manual(values = c("Cooperation" = "red", "Equity" = "darkgoldenrod1", "Altruism" = "blue", "Common" = "purple")) + # 공통색 추가 (사용 안될 수 있음)
    scale_shape_manual(values = c("Cooperation" = 16, "Equity" = 17, "Altruism" = 15, "Common" = 18)) + # 공통모양 추가 (다이아몬드)
    labs(
      title = plot_title,
      subtitle = "Color/Shape indicates Prosocial Cluster (Rhoads et al.)",
      caption = "Source: Prosocial [Rhoads et al.], Antisocial [Aoki et al., Wong et al., Dugré et al., Zhang & Peng]",
      x = "", y = "Brain Region & Directionality Summary",
      color = "Prosocial Cluster", shape = "Prosocial Cluster"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(), axis.ticks.x = element_blank(),
      axis.text.y = element_text(size = 8, lineheight = 0.9),
      plot.title = element_text(size = 14, face = "bold"),
      plot.caption = element_text(hjust = 0, face = "italic", size=8),
      legend.position = "right"
    )
  return(plot)
}

# 친사회적 고유 영역 플롯 생성 함수 (이전과 동일)
create_prosocial_unique_plot <- function(df, plot_title) {
  if (nrow(df) == 0) { return(ggplot() + annotate("text", x=0.5, y=0.5, label="해당 카테고리에\n보고된 뇌 영역 없음") + theme_void() + labs(title = plot_title)) }
  df_unnested <- df %>% unnest(Prosocial_Clusters)
  df_unnested <- df_unnested %>% mutate(Label = paste0(Standardized_Name, "\n  Direction: ", Direction_Summary))
  df_unnested$x_pos <- 1
  plot <- ggplot(df_unnested, aes(x = x_pos, y = reorder(Label, desc(Standardized_Name)))) +
    geom_point(aes(color = Prosocial_Clusters, shape = Prosocial_Clusters), size = 3) +
    scale_color_manual(values = c("Cooperation" = "red", "Equity" = "darkgoldenrod1", "Altruism" = "blue")) +
    scale_shape_manual(values = c("Cooperation" = 16, "Equity" = 17, "Altruism" = 15)) +
    labs( title = plot_title, subtitle = "Color/Shape indicates Prosocial Cluster (Rhoads et al.)", caption = "Source: Prosocial [Rhoads et al.]",
      x = "", y = "Brain Region & Directionality Summary", color = "Prosocial Cluster", shape = "Prosocial Cluster" ) +
    theme_minimal() +
    theme( axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 8, lineheight = 0.9),
      plot.title = element_text(size = 14, face = "bold"), plot.caption = element_text(hjust = 0, face = "italic", size=8), legend.position = "right" )
  return(plot)
}


# 반사회적 고유 영역 플롯 생성 함수 (이전과 동일)
create_antisocial_unique_plot <- function(df, plot_title) {
  if (nrow(df) == 0) { return(ggplot() + annotate("text", x=0.5, y=0.5, label="해당 카테고리에\n보고된 뇌 영역 없음") + theme_void() + labs(title = plot_title)) }
  df <- df %>% mutate(Label = paste0(Standardized_Name, "\n  Direction: ", Direction_Summary))
  df$x_pos <- 1
  plot <- ggplot(df, aes(x = x_pos, y = reorder(Label, desc(Standardized_Name)))) +
    geom_point(color = "darkred", size = 3, shape = 16) +
    labs( title = plot_title, subtitle = "Directionality based on meta-analyses review",
      caption = "Source: Antisocial [Aoki et al., Wong et al., Dugré et al., Zhang & Peng]",
      x = "", y = "Brain Region & Directionality Summary" ) +
    theme_minimal() +
    theme( axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 8, lineheight = 0.9),
      plot.title = element_text(size = 14, face = "bold"), plot.caption = element_text(hjust = 0, face = "italic", size=8) )
  return(plot)
}

# --- 6. 개별 플롯 생성 및 출력 ---

# 공통 영역 플롯 (클러스터별 시각화 적용)
plot_common_detailed <- create_common_region_plot(common_df_plot, "Common Brain Regions (Prosocial & Antisocial) with Directionality")
print(plot_common_detailed)

# 친사회적 고유 영역 플롯 (클러스터별 시각화)
plot_prosocial_unique <- create_prosocial_unique_plot(prosocial_df_plot, "Prosocial Unique Brain Regions")
print(plot_prosocial_unique)

# 반사회적 고유 영역 플롯
plot_antisocial_unique <- create_antisocial_unique_plot(antisocial_df_plot, "Antisocial Unique Brain Regions")
print(plot_antisocial_unique)


# --- 7. 결과 텍스트 출력 (이전과 동일) ---
cat("=======================================================================\n")
cat(" 친사회적 행동과 반사회적 행동 관련 뇌 영역 비교 분석 (클러스터 포함) \n")
cat("=======================================================================\n\n")

cat("--- 공통 뇌 부위 (친사회적 클러스터 정보 포함) ---\n")
if (nrow(common_df_plot) > 0) {
  print(common_df_plot[, c("Standardized_Name", "Prosocial_Direction", "Antisocial_Direction")], row.names = FALSE, right = FALSE, wrap = FALSE) # wrap=FALSE 추가
} else { cat("없음\n") }

cat("\n--- 친사회적 행동 고유 뇌 부위 (클러스터별) ---\n")
if (nrow(prosocial_df_plot) > 0) {
  prosocial_grouped_output <- prosocial_df_plot %>%
    unnest(Prosocial_Clusters) %>%
    arrange(Prosocial_Clusters, Standardized_Name) %>%
    select(Prosocial_Clusters, Standardized_Name, Direction_Summary)
  print(prosocial_grouped_output, row.names = FALSE, right = FALSE)
} else { cat("없음\n") }

cat("\n--- 반사회적 행동 고유 뇌 부위 ---\n")
if (nrow(antisocial_df_plot) > 0) {
   print(antisocial_df_plot[, c("Standardized_Name", "Direction_Summary")], row.names = FALSE, right = FALSE)
} else { cat("없음\n") }

