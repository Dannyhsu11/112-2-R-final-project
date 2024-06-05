library(tidyverse)

##資料處理

# 將'$ 總計'改名為'$ 客運總計'
PassengerVolumePerYear <- PassengerVolumePerYear %>%
  rename(客運量總計 = 總計)

# 將 '總計' 列名改為 '航機架數總計'
AircraftVolumePerYear <- AircraftVolumePerYear %>%
  rename(航機架數總計 = 總計)


# 使用完整外部合併，依照欄位 '年' 水平合併三個數據框
merged_data <- full_join(CargoVolumePerYear, PassengerVolumePerYear, by = "年") %>%
  full_join(AircraftVolumePerYear, by = "年")

# 將 '年度' 欄位改名為 '年'
Economic_index <- Economic_index %>%
  rename(年 = 年度)

# 使用左外部合併，根據欄位 '年'，合併 merged_data 和 Economic_index
merged_data <- left_join(merged_data, Economic_index, by = "年")

# 轉換`消費者物價-指數`和`消費者物價-年增率`為數值
merged_data <- merged_data %>%
  mutate(
    `消費者物價-指數` = as.numeric(str_replace_all(`消費者物價-指數`, "…", NA_character_)),
    `消費者物價-年增率` = as.numeric(str_replace_all(`消費者物價-年增率`, "…", NA_character_)),
  )

# 新增 '總貨運量年成長率' 欄位，内容為（該年 '總貨運量' - 前一年 '總貨運量'）/ 前一年 '總貨運量'
merged_data <- merged_data %>%
  mutate(總貨運量年成長率 = (總貨運量 - lag(總貨運量)) / lag(總貨運量))

# 新增 '進口貨量年成長率' 欄位，根據 '進口貨' 欄位计算，計算方法與 '總貨運量年成長率' 相同
merged_data <- merged_data %>%
  mutate(進口貨量年成長率 = (進口貨量 - lag(進口貨量)) / lag(進口貨量))

# 新增 '出口貨量年成長率' 欄位，根據 '出口貨量' 欄位计算，計算方法與 '總貨運量年成長率' 相同
merged_data <- merged_data %>%
  mutate(出口貨量年成長率 = (出口貨量 - lag(出口貨量)) / lag(出口貨量))

# 初始化描述結果的list
description <- list()

# 描述總體資料
description$total <- list(
  columns = ncol(merged_data),
  rows = nrow(merged_data)
)

# 描述每一個欄位
for (col in colnames(merged_data)) {
  column_data <- merged_data[[col]]
  missing_values <- sum(is.na(column_data))
  missing_percentage <- (missing_values / nrow(merged_data)) * 100
  
  # 數值型欄位描述
  if (is.numeric(column_data)) {
    column_description <- list(
      missing_values = missing_values,
      missing_percentage = missing_percentage,
      range = range(column_data, na.rm = TRUE),
      mean = mean(column_data, na.rm = TRUE),
      max = max(column_data, na.rm = TRUE),
      min = min(column_data, na.rm = TRUE),
      quantiles = quantile(column_data, na.rm = TRUE),
      median = median(column_data, na.rm = TRUE)
    )
  } else {
    # 非數值型欄位描述
    unique_values <- unique(column_data)
    if (length(unique_values) < 10) {
      value_counts <- table(column_data)
      value_percentages <- prop.table(value_counts) * 100
      column_description <- list(
        missing_values = missing_values,
        missing_percentage = missing_percentage,
        value_counts = as.list(value_counts),
        value_percentages = as.list(value_percentages)
      )
    } else {
      column_description <- list(
        missing_values = missing_values,
        missing_percentage = missing_percentage,
        unique_values_count = length(unique_values)
      )
    }
  }
  
  # 添加描述到list
  description[[col]] <- column_description
}

# 查看描述結果
description

# 製作 bar chart
library(ggplot2)
library(tidyr)

# 整理数据，將長格式轉為寬格式
data_long <- merged_data %>%
  select(年, 客運量總計) %>%
  pivot_longer(cols = c(客運量總計), names_to = "類別", values_to = "數量")

# 繪製 bar chart
ggplot(data_long, aes(x = 年, y = 數量, fill = 類別)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "客運量總計年變化", x = "年", y = "數量（人）") +
  theme_minimal()

# 整理数据，將長格式轉為寬格式
data_long <- merged_data %>%
  select(年, 航機架數總計) %>%
  pivot_longer(cols = c(航機架數總計), names_to = "類別", values_to = "數量")

# 繪製 bar chart
ggplot(data_long, aes(x = 年, y = 數量, fill = 類別)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "航機架數總計年變化", x = "年", y = "數量（架數）") +
  theme_minimal()

# 整理數據，將長格式轉為寬格式
data_long <- merged_data %>%
  select(年, 進口貨量, 出口貨量) %>%
  pivot_longer(cols = c(進口貨量, 出口貨量), names_to = "類別", values_to = "數量")

# 繪製 bar chart
ggplot(data_long, aes(x = 年, y = 數量, fill = 類別)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "進口貨量、出口貨量年變化", x = "年", y = "數量（公斤）") +
  theme_minimal()

# 製作折線圖表示 '$ 總貨運量'、'$ 進口貨量'、'$ 出口貨量' 的年變化趨勢

#ggplot(merged_data, aes(x = 年)) +
#  geom_line(aes(y = 總貨運量, color = "總貨運量")) +
#  labs(title = "總貨運量年變化趨勢", x = "年", y = "數量（公斤）", color = "項目") +
#  theme_minimal()


# 提取相關欄位並創建一個新的數據框
growth_rates <- merged_data %>%
  select(年, 總貨運量年成長率, 進口貨量年成長率, 出口貨量年成長率)

# 將數據轉換為長格式
growth_rates_long <- growth_rates %>%
  pivot_longer(cols = -年, names_to = "指標", values_to = "成長率")

# 繪製折線圖
ggplot(growth_rates_long, aes(x = 年, y = 成長率, color = 指標)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "總貨運量、進口貨量和出口貨量年成長率",
       x = "年份",
       y = "成長率",
       color = "指標") +
  theme_minimal() +
  scale_color_manual(values = c("#004B97", "#CE0000", "#009100"))


# 迴歸分析與製圖 -----

# 建立多變量線性迴歸模型 -----
library(dplyr)

# 檢查和過濾缺失值
filtered_data <- merged_data %>%
  select(
    總貨運量,
    經濟成長率,
    `平均每人國民所得毛額（美元）`,
    `失業率（百分比）`,
    `消費者物價-指數`,
    `消費者物價-年增率`
  ) %>%
  drop_na()

# 建立迴歸模型
model <- lm(總貨運量 ~ 經濟成長率 + `平均每人國民所得毛額（美元）` + `失業率（百分比）` + `消費者物價-指數` + 
              `消費者物價-年增率` , data = filtered_data)

# 顯示模型摘要
summary(model)

# 建立預測值和殘差
filtered_data <- filtered_data %>%
  mutate(
    Predicted = predict(model),
    Residuals = residuals(model)
  )

# 實際值與預測值比較圖
ggplot(filtered_data, aes(x = Predicted, y = 總貨運量)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "實際值 vs 預測值", x = "預測值", y = "實際值") +
  theme_minimal()

# 殘差圖
ggplot(filtered_data, aes(x = Predicted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "殘差圖", x = "預測值", y = "殘差") +
  theme_minimal()
