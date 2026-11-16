
library(ggmap)
library(rgdal)
library(maps)
library(dplyr)
EC <- read.csv('../IHME-GBD_2021_DATA-1e92dcc0-1/IHME-GBD_2021_DATA-a762cc28-1.csv',header = T)  
ASR_2021 <-subset(EC,EC$year==2021 & 
                    EC$age_name=='20+ years' & 
                    EC$sex_name=='Both' &
                    EC$cause_name=="Ischemic stroke" &
                    EC$metric_name== 'Rate' &
                    EC$measure_name=='Incidence')
ASR_2021 <- ASR_2021[,c(4,14,15,16)]
ASR_2021$val <- round(ASR_2021$val,1) 
ASR_2021$lower <- round(ASR_2021$lower,1)
ASR_2021$upper <- round(ASR_2021$upper,1)
library(GlobalDietaryR)
# ===== Step 1: 定义自定义配色方案 =====
# 蓝色系 (低值) + 红色系 (高值)
colors<-c(
  '#3182BD',  # 深蓝色  (0-20)
  '#6BAED6',  # 中蓝色  (20-40)
  '#9ECAE1', # 浅蓝色   (40-60)
  '#FEE5D9',  # 米色  (60-80)
  '#FCAE91',  # 浅橙色  (80-100)
  '#FB6A4A',  # 橙红色  (100-150)
  '#DE2D26',  # 深红色  (150-500)
  '#A50F15' # 暗红色 (500+, 备用)
)
dtas<-ASR_2021
dtas$median<-dtas$val
# ===== Step 2: 分析数据分布确定分段点 =====
summary(dtas$median)
cat("📊 数据分布:\n")
cat(" 最小值:", min(dtas$median), "\n")
cat(" 25%分位:", quantile(dtas$median, 0.25), "\n")
cat(" 中位数:", median(dtas$median), "\n")
cat(" 75%分位:", quantile(dtas$median, 0.75), "\n")
cat(" 最大值:", max(dtas$median), "\n")

# ===== Step 3: 绘制分段配色地图 =====
plot_binned<-GDD_plot_world_map_D(
  data=dtas,
  val_col='median',
  location_col='location_name',
  only_large_map=FALSE,
  
  # 🔑 分段设置
  breaks=c(36.500,87.800, 113.150, 141.125, 430.900), # 分段边界
  labels=c("36.500-87.800", "87.800-113.150", "113.150-141.125", "141.125-430.900"), # 分段标签
  
  color_palette=colors,  # 自定义颜色
  legend_name="Ischemic stroke(per 100,000)",
  plot_title="Ischemic stroke",
  
  # 局部放大区域标题
  small_titles=c("Caribbean and Central America",
                 "Persian Gulf",
                 "Balkan Peninsula",
                 "Southeast Asia",
                 "West Africa",
                 "Eastern Mediterranean",
                 "Northern Europe")
)+
  theme(
    # plot.title = element_text(size = 20, face = "bold")
    legend.title = element_text(size = 25, face = "bold")
  )

# 显示地图
print(plot_binned)

ggsave("Ischemic stroke.pdf", plot_binned, device = "pdf", width =16, height = 10)
ggsave("Ischemic stroke.png", plot_binned, device = "png", dpi = 600, width = 16, height =10)

ASR_2021 <-subset(EC,EC$year==2021 & 
                    EC$age_name=='20+ years' & 
                    EC$sex_name=='Both' &
                    EC$cause_name=="Diabetes mellitus" &
                    EC$metric_name== 'Rate' &
                    EC$measure_name=='Incidence')
ASR_2021 <- ASR_2021[,c(4,14,15,16)]
ASR_2021$val <- round(ASR_2021$val,1) 
ASR_2021$lower <- round(ASR_2021$lower,1)
ASR_2021$upper <- round(ASR_2021$upper,1)
dtas<-ASR_2021
dtas$median<-dtas$val
# ===== Step 2: 分析数据分布确定分段点 =====
summary(dtas$median)
cat("📊 数据分布:\n")
cat(" 最小值:", min(dtas$median), "\n")
cat(" 25%分位:", quantile(dtas$median, 0.25), "\n")
cat(" 中位数:", median(dtas$median), "\n")
cat(" 75%分位:", quantile(dtas$median, 0.75), "\n")
cat(" 最大值:", max(dtas$median), "\n")

# ===== Step 3: 绘制分段配色地图 =====
plot_binned<-GDD_plot_world_map_D(
  data=dtas,
  val_col='median',
  location_col='location_name',
  only_large_map=FALSE,
  
  # 🔑 分段设置
  breaks=c(147.300,336.15, 469.1, 696.625, 1329.7), # 分段边界
  labels=c("147.300-336.150", "336.150-469.100", "469.100-696.625", "696.625-1329.70"), # 分段标签
  
  color_palette=colors,  # 自定义颜色
  legend_name="Diabetes mellitus(per 100,000)",
  plot_title="Diabetes mellitus",
  
  # 局部放大区域标题
  small_titles=c("Caribbean and Central America",
                 "Persian Gulf",
                 "Balkan Peninsula",
                 "Southeast Asia",
                 "West Africa",
                 "Eastern Mediterranean",
                 "Northern Europe")
)+
  theme(
    # plot.title = element_text(size = 20, face = "bold")
    legend.title = element_text(size =40, face = "bold")
  )

# 显示地图
print(plot_binned)

ggsave("Diabetes mellitus.pdf", plot_binned, device = "pdf", width =16, height = 10)
ggsave("Diabetes mellitus.png", plot_binned, device = "png", dpi = 600, width = 16, height =10)


ASR_2021 <-subset(EC,EC$year==2021 & 
                    EC$age_name=='20+ years' & 
                    EC$sex_name=='Both' &
                    EC$cause_name=="Ischemic heart disease" &
                    EC$metric_name== 'Rate' &
                    EC$measure_name=='Incidence')
ASR_2021 <- ASR_2021[,c(4,14,15,16)]
ASR_2021$val <- round(ASR_2021$val,1) 
ASR_2021$lower <- round(ASR_2021$lower,1)
ASR_2021$upper <- round(ASR_2021$upper,1)

dtas<-ASR_2021
dtas$median<-dtas$val
# ===== Step 2: 分析数据分布确定分段点 =====
summary(dtas$median)
cat("📊 数据分布:\n")
cat(" 最小值:", min(dtas$median), "\n")
cat(" 25%分位:", quantile(dtas$median, 0.25), "\n")
cat(" 中位数:", median(dtas$median), "\n")
cat(" 75%分位:", quantile(dtas$median, 0.75), "\n")
cat(" 最大值:", max(dtas$median), "\n")

# ===== Step 3: 绘制分段配色地图 =====
plot_binned<-GDD_plot_world_map_D(
  data=dtas,
  val_col='median',
  location_col='location_name',
  only_large_map=FALSE,
  
  # 🔑 分段设置
  breaks=c(115.7,328.85, 460.45, 703.625,1696.7), # 分段边界
  labels=c("115.700-328.850", "328.850-460.450", "460.450-703.625", "703.625-1696.70"), # 分段标签
  
  color_palette=colors,  # 自定义颜色
  legend_name="Ischemic heart disease(per 100,000)",
  plot_title="Ischemic heart disease",
  
  # 局部放大区域标题
  small_titles=c("Caribbean and Central America",
                 "Persian Gulf",
                 "Balkan Peninsula",
                 "Southeast Asia",
                 "West Africa",
                 "Eastern Mediterranean",
                 "Northern Europe")
)+
  theme(
    # plot.title = element_text(size = 20, face = "bold")
    legend.title = element_text(size = 25, face = "bold")
  )

# 显示地图
print(plot_binned)

ggsave("Ischemic heart disease.pdf", plot_binned, device = "pdf", width =16, height = 10)
ggsave("Ischemic heart disease.png", plot_binned, device = "png", dpi = 600, width = 16, height =10)
EC <- read.csv('../IHME-GBD_2021_DATA-1e92dcc0-1/IHME-GBD_2021_DATA-a762cc28-1.csv')  
data <- subset(EC,EC$year==2021 & 
                 EC$age_name=='20+ years' & 
                 EC$sex_name=='Both' &
                 EC$cause_name=="Ischemic stroke" &
                 EC$metric_name== 'Rate' &
                 EC$measure_name=='Incidence')
data <- data[,c(4,14)]
data$val <- round(data$val,1) 
# 计算四分位数
quartiles <- quantile(data$val, probs = c(0, 0.25, 0.5, 0.75, 1))
print(quartiles)

# 根据四分位数分组
data$Ischemic_stroke <- cut(
  data$val,
  breaks = quartiles,
  labels = c("低", "中下", "中上", "高"),
  include.lowest = TRUE,
  right = FALSE
)

data1 <- subset(EC,EC$year==2021 & 
                  EC$age_name=='20+ years' & 
                  EC$sex_name=='Both' &
                  EC$cause_name=="Diabetes mellitus" &
                  EC$metric_name== 'Rate' &
                  EC$measure_name=='Incidence')
data1 <- data1[,c(4,14)]
data1$val <- round(data1$val,1) 
# 计算四分位数
quartiles <- quantile(data1$val, probs = c(0, 0.25, 0.5, 0.75, 1))
print(quartiles)

# 根据四分位数分组
data1$Diabetes_mellitus <- cut(
  data1$val,
  breaks = quartiles,
  labels = c("低", "中下", "中上", "高"),
  include.lowest = TRUE,
  right = FALSE
)
data3 <- merge(data1, data, by.x = 'location_name', by.y = 'location_name', all = TRUE)
data3 <- data3[,c(1,3,5)]
library(dplyr)

data3<- data3 %>%
  mutate(
    Diabetes = case_when(
      Diabetes_mellitus == "低" ~ 1,
      Diabetes_mellitus == "中下" ~ 2,
      Diabetes_mellitus == "中上" ~ 3,
      Diabetes_mellitus == "高" ~ 4
    ),
    Ischemic = case_when(
      Ischemic_stroke == "低" ~ 1,
      Ischemic_stroke == "中下" ~ 2,
      Ischemic_stroke == "中上" ~ 3,
      Ischemic_stroke == "高" ~ 4
    )
  )
# 创建分类变量
data3$Classification <- ifelse(
  data3$Diabetes ==data3$Ischemic,
  "Consistent",
  ifelse(
    data3$Diabetes > data3$Ischemic,
    "Diabetes mellitus dominant",
    "Ischemic stroke dominant"
  )
)
data4 <- data3[,c(1,2,3,6)]
write.csv(data4,"../区域划分/data.csv")
plot_consistency <- GDD_plot_world_map_D(
  data = data3,
  val_col = "Class_code",          # 数值编码列
  location_col = "location_name",
  only_large_map = FALSE,
  breaks = c(0.5, 1.5, 2.5, 3.5),  # 半整数边界，把 1/2/3 各切成一段
  labels = c("Ischemic stroke dominant",
             "Consistent",
             "Diabetes mellitus dominant"),
  color_palette = c("#2166AC", "#FEE5D9", "#B2182B"),  # 蓝-白-红，按需换
  legend_name = "Consistency pattern",
  plot_title = "Diabetes mellitus vs Ischemic Stroke Consistency",
  small_titles = c("Caribbean and Central America",
                   "Persian Gulf",
                   "Balkan Peninsula",
                   "Southeast Asia",
                   "West Africa",
                   "Eastern Mediterranean",
                   "Northern Europe")
)

plot_consistency

ggsave("一致性区域（糖脑）.pdf", plot_consistency, device = "pdf", width = 16, height = 10)
ggsave("一致性区域（糖脑）.png", plot_consistency, device = "png", dpi = 600, width = 16, height =10)

data <-read.csv("../区域划分/糖脑/heatmap_data .csv")
# 查看数据框
print(data)
is_stroke <- data$Diabetes
ihd <-data$Ischemic
# 定义自定义颜色映射
data$color <- "white"  # 默认颜色
data$color[is_stroke < ihd] <-"#007B4B" # 左下角为绿色
data$color[is_stroke == ihd] <-"#FF5C00"    # 对角线为红色
data$color[is_stroke > ihd] <- "#FFCD00FF" # 右上角为黄色
# 将百分位数值转换为因子以便用于绘图
data$Diabetes <- factor(data$Diabetes, levels = c(0, 25, 50, 75))
data$Ischemic <- factor(data$Ischemic, levels = c(75, 50, 25, 0))
# 绘制热图
library(ggplot2)
p<-ggplot(data, aes(x =Diabetes , y =Ischemic, fill = color)) +
  geom_tile(color = "white", size = 1) +
  geom_text(
    aes(label = n, color = color),
    fontface = "bold",
    size = 14
  ) +
  scale_color_manual(
    values = c(
      "#007B4B" = "white",  # 蓝色区域
      "#FF5C00" = "black",  # 粉色区域
      "#FFCD00FF" = "white"# 红色区域
      # "white" = "black"     # 白色区域
    )
  ) +
  scale_fill_manual(values = c("#007B4B" ="#2166AC", "#FF5C00" ="#FEE5D9", "#FFCD00FF" ="#B2182B", white = "white")) +
  labs(
    # title = "IHD and Ischaemic Stroke Percentile Counts",
    x = "Diabetes mellitus percentile",
    y = "Ischemic heart disease percentile",size = 18
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 18, face = "bold", color = "black"),
    axis.title = element_text(size = 18, face = "bold", color = "black"),
    
    axis.text.x = element_text(
      size = 20, color = "black",
      margin = ggplot2::margin(r = -10)
    ),
    axis.text.y = element_text(
      size = 20, color = "black",
      margin =ggplot2::margin(r = -0)
    ),
    
    axis.title.x = element_text(
      size = 20, color = "black"
    ),
    axis.title.y = element_text(
      size = 20, color = "black"
    ),
    
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.position = "none"
  ) +
  scale_x_discrete(position = "top")

p
ggsave("热图(糖脑).pdf", p, device = "pdf", width = 10, height = 8)
ggsave("热图(糖脑).png", p, device = "png", dpi = 600, width = 10, height =8)



#糖心
EC <- read.csv('../IHME-GBD_2021_DATA-1e92dcc0-1/IHME-GBD_2021_DATA-a762cc28-1.csv')  
data <- subset(EC,EC$year==2021 & 
                 EC$age_name=='20+ years' & 
                 EC$sex_name=='Both' &
                 EC$cause_name=="Ischemic heart disease" &
                 EC$metric_name== 'Rate' &
                 EC$measure_name=='Incidence')
data <- data[,c(4,14)]
data$val <- round(data$val,1) 
# 计算四分位数
quartiles <- quantile(data$val, probs = c(0, 0.25, 0.5, 0.75, 1))
print(quartiles)

# 根据四分位数分组
data$Ischemic_stroke <- cut(
  data$val,
  breaks = quartiles,
  labels = c("低", "中下", "中上", "高"),
  include.lowest = TRUE,
  right = FALSE
)

data1 <- subset(EC,EC$year==2021 & 
                  EC$age_name=='20+ years' & 
                  EC$sex_name=='Both' &
                  EC$cause_name=="Diabetes mellitus" &
                  EC$metric_name== 'Rate' &
                  EC$measure_name=='Incidence')
data1 <- data1[,c(4,14)]
data1$val <- round(data1$val,1) 
# 计算四分位数
quartiles <- quantile(data1$val, probs = c(0, 0.25, 0.5, 0.75, 1))
print(quartiles)

# 根据四分位数分组
data1$Diabetes_mellitus <- cut(
  data1$val,
  breaks = quartiles,
  labels = c("低", "中下", "中上", "高"),
  include.lowest = TRUE,
  right = FALSE
)
data3 <- merge(data1, data, by.x = 'location_name', by.y = 'location_name', all = TRUE)
data3 <- data3[,c(1,3,5)]
library(dplyr)

data3<- data3 %>%
  mutate(
    Diabetes = case_when(
      Diabetes_mellitus == "低" ~ 1,
      Diabetes_mellitus == "中下" ~ 2,
      Diabetes_mellitus == "中上" ~ 3,
      Diabetes_mellitus == "高" ~ 4
    ),
    Ischemic = case_when(
      Ischemic_stroke == "低" ~ 1,
      Ischemic_stroke == "中下" ~ 2,
      Ischemic_stroke == "中上" ~ 3,
      Ischemic_stroke == "高" ~ 4
    )
  )
# 创建分类变量
data3$Classification <- ifelse(
  data3$Diabetes ==data3$Ischemic,
  "Consistent",
  ifelse(
    data3$Diabetes > data3$Ischemic,
    "Diabetes mellitus dominant",
    "Ischemic heart disease dominant"
  )
)

# 1) 三分类 → 数值编码（顺序即图例顺序）
data3 <- data3 %>%
  mutate(Class_code = case_when(
    Classification == "Ischemic heart disease dominant"   ~ 1,
    Classification == "Consistent"                 ~ 2,
    Classification == "Diabetes mellitus dominant" ~ 3
  ))

table(data3$Classification, useNA = "ifany")  # 检查一下各类数量

# 2) 用 _D 版本画分段地图
plot_consistency <- GDD_plot_world_map_D(
  data = data3,
  val_col = "Class_code",          # 数值编码列
  location_col = "location_name",
  only_large_map = FALSE,
  breaks = c(0.5, 1.5, 2.5, 3.5),  # 半整数边界，把 1/2/3 各切成一段
  labels = c("Ischemic heart disease dominant",
             "Consistent",
             "Diabetes mellitus dominant"),
  color_palette = c("#2166AC", "#FEE5D9", "#B2182B"),  # 蓝-白-红，按需换
  legend_name = "Consistency pattern",
  plot_title = "Diabetes mellitus vs Ischemic heart disease dominant  Consistency",
  small_titles = c("Caribbean and Central America",
                   "Persian Gulf",
                   "Balkan Peninsula",
                   "Southeast Asia",
                   "West Africa",
                   "Eastern Mediterranean",
                   "Northern Europe")
)

plot_consistency

ggsave("一致性区域（糖心）.pdf", plot_consistency, device = "pdf", width = 16, height = 10)
ggsave("一致性区域（糖心）.png", plot_consistency, device = "png", dpi = 600, width = 16, height =10)

# 创建数据框
data5 <- data3[,c(4,5)]
# 将数据分组并计数
library(dplyr)
heatmap_data <- data5 %>%
  group_by(Diabetes, Ischemic) %>%
  tally() %>%
  ungroup()
data <-read.csv("../区域划分/heatmap_data .csv")
# 查看数据框
print(data)
is_stroke <- data$Diabetes
ihd <-data$Ischemic
# 定义自定义颜色映射
data$color <- "white"  # 默认颜色
data$color[is_stroke < ihd] <-"#007B4B" # 左下角为绿色
data$color[is_stroke == ihd] <-"#FF5C00"    # 对角线为红色
data$color[is_stroke > ihd] <- "#FFCD00FF" # 右上角为黄色
# 将百分位数值转换为因子以便用于绘图
data$Diabetes <- factor(data$Diabetes, levels = c(0, 25, 50, 75))
data$Ischemic <- factor(data$Ischemic, levels = c(75, 50, 25, 0))
# 绘制热图
library(ggplot2)
p <- ggplot(data, aes(x = Diabetes, y = Ischemic, fill = color)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(
    aes(label = n, color = color),
    fontface = "bold",
    size = 14
  ) +
  scale_color_manual(
    values = c(
      "#007B4B" = "white",  # 蓝色区域控制文字颜色
      "#FF5C00" = "black",  # 粉色区域
      "#FFCD00FF" = "white"# 红色区域
      # "white" = "black"     # 白色区域
    )
  ) +
  scale_fill_manual(values = c(#控制图形颜色
    "#007B4B" = "#2166AC",
    "#FF5C00" = "#FEE5D9",
    "#FFCD00FF" = "#B2182B",
    white = "white"
  )) +
  labs(
    x = "Diabetes mellitus percentile",
    y = "Ischemic heart disease percentile"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 18, face = "bold", color = "black"),
    axis.title = element_text(size = 18, face = "bold", color = "black"),
    
    axis.text.x = element_text(
      size = 20, color = "black",
      margin = ggplot2::margin(b = -15)
    ),
    axis.text.y = element_text(
      size = 20, color = "black",
      margin = ggplot2::margin(r = -0)
    ),
    
    axis.title.x = element_text(
      size = 20, color = "black"
    ),
    axis.title.y = element_text(
      size = 20, color = "black"
    ),
    
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.position = "none"
  ) +
  scale_x_discrete(position = "top")

p
ggsave("热图（糖心）.pdf", p, device = "pdf", width = 10, height = 8)
ggsave("热图（糖心）.png", p, device = "png", dpi = 600, width = 10, height =8)
