# Chun Dai

myplot <- function(data, title = "Time Series Plot", color = "black", plotmean = TRUE) {
  # Check if the input is a data frame, matrix, or ts object
  if (is.numeric(data) && !is.ts(data)) {
    stop("Error: Input must be a data frame, matrix, or 'ts' object, not a numeric vector.")
  } else if (!is.data.frame(data) && !is.matrix(data) && !is.ts(data)) {
    stop("Error: Unsupported input type. Must be a data frame, matrix, or 'ts' object.")
  }
  
  # Convert ts object to a data frame for consistent handling
  if (is.ts(data)) {
    # Get the time index for the ts object
    time_index <- time(data)
    
    # Convert the ts object into a data frame and add the time information
    data <- as.data.frame(data)
    data$Time <- time_index
    
    # Reshape data into long format
    data <- data %>% pivot_longer(-Time, names_to = "Series", values_to = "Value")
  } else {
    # If data is a data frame or matrix, ensure it's in long format
    if (!("Time" %in% colnames(data))) {
      data <- data.frame(Time = seq_len(nrow(data)), data)
    }
    data <- pivot_longer(data, -Time, names_to = "Series", values_to = "Value")
  }
  
  # Check for missing data and warn the user
  if (any(is.na(data$Value))) {
    warning("Warning: Missing values found. Proceeding with NA handling.")
  }
  
  # Compute summary statistics
  data_summary <- data %>%
    group_by(Time) %>%
    summarize(
      Median = median(Value, na.rm = TRUE),
      Q1 = quantile(Value, 0.25, na.rm = TRUE),
      Q3 = quantile(Value, 0.75, na.rm = TRUE),
      Average = mean(Value, na.rm = TRUE)
    )
  
  # Plotting
  plot <- ggplot(data_summary, aes(x = Time)) +
    geom_ribbon(aes(ymin = Q1, ymax = Q3), fill = "skyblue", alpha = 0.5) +
    geom_line(aes(y = Median), color = "grey", linetype = "solid") +
    geom_point(aes(y = Median), color = "grey", shape = 21, fill = "grey") +
    labs(title = title, y = "Value", x = "Time") +
    theme_minimal() +
    theme(legend.position = "top", legend.box = "horizontal")
  
  # Add average line if requested
  if (plotmean) {
    plot <- plot + geom_line(aes(y = Average), color = color) +
      geom_point(aes(y = Average), color = color, shape = 21, fill = color) +
      labs(color = "Average")
  }
  
  # Display the plot
  print(plot)
}








library(ggplot2)
library(dplyr)
library(tidyr)

myplot <- function(data, title = "Time Series Plot", color = "black", plotmean = TRUE) {
  # 检查输入数据类型
  if (is.vector(data) && is.numeric(data)) {
    stop("Error: Input is a numeric vector. Please provide a data frame, matrix, or 'ts' object.")
  }
  
  # 检查是否为数据框、矩阵或时间序列对象
  if (!is.data.frame(data) && !is.matrix(data) && !is.ts(data)) {
    stop("Error: Input must be a data frame, matrix, or 'ts' object.")
  }
  
  # 转换ts对象为data frame，以便一致处理
  if (is.ts(data)) {
    time_index <- time(data)
    data <- as.data.frame(data)
    data$Time <- time_index
    data <- pivot_longer(data, -Time, names_to = "Series", values_to = "Value")
  } else {
    if (!("Time" %in% colnames(data))) {
      data <- data.frame(Time = seq_len(nrow(data)), data)
    }
    data <- pivot_longer(data, -Time, names_to = "Series", values_to = "Value")
  }
  
  # 检查缺失数据，并给予警告
  if (any(is.na(data$Value))) {
    warning("Warning: Missing values found. Proceeding with NA handling.")
  }
  
  # 计算每个时间点的统计值：Median, Q1, Q3, Average
  data_summary <- data %>%
    group_by(Time) %>%
    summarize(
      Median = median(Value, na.rm = TRUE),
      Q1 = quantile(Value, 0.25, na.rm = TRUE),
      Q3 = quantile(Value, 0.75, na.rm = TRUE),
      Average = mean(Value, na.rm = TRUE)
    )
  
  # 创建绘图
  plot <- ggplot(data_summary, aes(x = Time)) +
    # 绘制Q1和Q3的区域填充
    geom_ribbon(aes(ymin = Q1, ymax = Q3, fill = "Q1 & Q3"), alpha = 0.3) +
    # 绘制Median线（灰色的实线）
    geom_line(aes(y = Median, color = "Median"), size = 1) +
    # 添加标题和坐标轴标签
    labs(title = title, y = "Value", x = "Year") +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "top", # 图例放置在上方
      legend.box = "vertical",  # 图例垂直排列，确保不会覆盖图表
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10)
    ) +
    scale_x_continuous(
      breaks = seq(1985, 2015, 5),  # 自定义横坐标（每5年一个点）
      labels = seq(1985, 2015, 5)
    ) +
    scale_fill_manual(values = c("Q1 & Q3" = "blue")) + # 设置Q1&Q3填充颜色
    scale_color_manual(values = c("Median" = "grey"))  # 设置Median线的颜色
  
  # 如果plotmean为TRUE，则添加Average线（黑色带空心圈的直线）
  if (plotmean) {
    plot <- plot +
      geom_line(aes(y = Average, color = "Average"), size = 1) +  # 平均值线（黑色）
      geom_point(aes(y = Average, color = "Average"), shape = 1, size = 3) +  # 平均值点（空心圆）
      scale_color_manual(values = c("Average" = color))  # 设置Average线的颜色
  }
  
  # 打印绘图
  print(plot)
}


# Ensure required libraries are loaded
library(dplyr)
library(ggplot2)
library(tidyr)  # Load tidyr for pivot_longer

# Example usage with a sample dataset

# Dataset for the source figure
ad <- read.csv("dataraw/AD_cluster_3.csv")
rownames(ad) <- ad[, 1]
ad <- ad[, -1]

# Dataset in ts format
EuStockMarkets

# Dataset with missing values
ad_na <- ad
ad_na$CHOTF[ad$CHOTF == 0] <- NA

# Dataset of 1 time series (should give an error)
ts01 <- rnorm(100)
ts02 <- ts(ts01)
ts03 <- ad[, 1, drop = FALSE]

# Check the function
## Should work in each case
myplot(ad)
myplot(EuStockMarkets)
myplot(ad_na)


## 25 points: Consider changing the color and add/remove the line for the average
## (the arguments plotmean and color can be named differently by the student)
myplot(ad, plotmean = TRUE, color = "red")

## 15 points: Should not work (give an informative error in each case)
myplot(ts01)
myplot(ts02)
myplot(ts03)