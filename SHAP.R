
library(shapviz)
library(h2o)
library(caret)
library(modeldata)         
library(tidymodels) #
library(tidyverse)
EC =read.csv("../IHME-GBD_2021_DATA-1e92dcc0-1/IHME-GBD_2021_DATA-d6aa8b0f-1.csv",header = T,check.names = F)
ASR_2021 <- EC[,c(4,10,14)]
# 转换为宽格式
data <- ASR_2021%>%
  pivot_wider(
    names_from = rei_name, # 用rei_name列的值作为新列名
    values_from = val, # 用value列的值填充新列
    values_fn = mean
  )

# 查看宽格式数据
names(data)
EC1 <- read.csv('../IHME-GBD_2021_DATA-1e92dcc0-1/IHME-GBD_2021_DATA-a762cc28-1.csv')  
data1 <- subset(EC1,EC1$year==2021 & 
                  EC1$age_name=='20+ years' & 
                  EC1$sex_name=='Both' &
                  EC1$cause_name=="Ischemic heart disease" &
                  EC1$metric_name== 'Rate' &
                  EC1$measure_name=='Incidence')
data1 <- data1[,c(4,14)]
data2 <- merge(data, data1, by.x = "location_name", by.y = "location_name", all.x = TRUE)
data2 <- data2[,-c(1)]
gene<-c("Low physical activity",'Intimate partner violence','Diet low in calcium','No access to handwashing facility','Chewing tobacco','Diet low in legumes',
        'High temperature','Low temperature','Kidney dysfunction','High LDL cholesterol','Ambient particulate matter pollution','Household air pollution from solid fuels',
        'Ambient ozone pollution','Residential radon','Lead exposure','Iron deficiency','Secondhand smoke',
        'High alcohol use','Drug use','High fasting plasma glucose','High systolic blood pressure','High body-mass index','Low bone mineral density',
        'Diet low in fruits','Diet low in vegetables','Diet low in whole grains','Diet low in nuts and seeds','Diet low in milk','Diet high in red meat',
        'Diet high in processed meat','Diet high in sugar-sweetened beverages','Diet low in fiber','Diet low in seafood omega-3 fatty acids','Diet Low in Omega-6 Polyunsaturated Fatty Acids',
        'Diet high in trans fatty acids','Diet high in sodium','Unsafe water source','Unsafe sanitation','val')
#data<-merge(data, data1, by.x = 'location_name', by.y = 'location_name')
data2 <- data2[,gene]
data2$val <- as.numeric(as.character(data2$val))

#加载包#
library(readxl)
library(dplyr)
library(caret)
library(xgboost)
library(SHAPforxgboost)
library(ggplot2)
pollutant<-"val"
#分割数据#
set.seed(123)
index <- createDataPartition(data2[[pollutant]], p =0.9, list = FALSE)
#选取自变量和目标变量
train<- data2[index, c(1:38, which(colnames(data2) == pollutant))]
test<- data2[-index, c(1:38, which(colnames(data2) == pollutant))]
#创建数值矩阵#
train_x<- data.matrix(train[,1:37])
train_y<- train[[38]]
test_x<- data.matrix(test[,1:37])
test_y<- test[[38]]
#转换为DMatrix#
dtrain<- xgb.DMatrix(data = train_x, label = train_y)
dtest<- xgb.DMatrix(data = test_x, label = test_y)
#设置XGBoost参数##控制树的深度（max_depth）与学习率（eta）；设置早停（early_stopping_rounds）避免过拟合#
params<- list(objective ="reg:squarederror", 
              eta=0.1,
              max_depth=6,
              subsample = 0.8,
              colsample_bytree=0.8,
              eval_metric="rmse")

xgb_model<- xgb.train(params = params,
                      data = dtrain,
                      nrounds=100,
                      early_stopping_rounds=10,
                      watchlist= list(train = dtrain, eval = dtest),
                      verbose=0)
shap_values<- shap.values(xgb_model, X_train = train_x)
shap_long <- shap.prep(shap_contrib = shap_values$shap_score, X_train = train_x)
shap.plot.dependence(data_long = shap_long, x =" ", color_feature =" ")
shap.plot.summary(shap_long) + labs(y ="SHAP value") +
  theme(
    text = element_text(size = 9),  # 设置整体字体大小
    axis.text = element_text(size = 9),  # 设置坐标轴文本大小
    axis.title = element_text(size = 9, face = "bold"),  # 设置坐标轴标题大小和加粗
    plot.title = element_text(size = 9, face = "bold", hjust = 0.5),  # 设置图表标题大小和加粗
    panel.border = element_blank(),  # 不显示边框
    panel.grid.major = element_blank(),  # 设置网格线
    panel.grid.minor = element_blank(),  # 隐藏次要网格线
    # legend.position = "right",  # 设置图例位置
    legend.text = element_text(size = 8),  # 设置图例文本大小
    legend.title = element_text(size = 10, face = "bold"),  # 设置图例标题大小和加粗
    axis.line.x = element_line(color = "black", size = 0.5),  # 添加x轴线
    axis.line.y = element_line(color = "black", size = 0.5),  # 添加y轴线
    panel.background = element_rect(color = "black", size = 0.5)  # 添加面板边框线
  )
shap = shapviz(xgb_model, X_pred = train_x)
# 将 mean_value 保留两位有效数字
shap_long$mean_value <- round(shap_long$mean_value, 2)

#绘制偏相关依赖图
p <- sv_importance(shap, kind = "beeswarm",max_display = 73) +
  theme_bw() +
  theme(
    text = element_text(size = 9),  # 设置整体字体大小
    axis.text = element_text(size =12),  # 设置坐标轴文本大小
    axis.title = element_text(size = 9, face = "bold"),  # 设置坐标轴标题大小和加粗
    plot.title = element_text(size = 9, face = "bold", hjust = 0.5),  # 设置图表标题大小和加粗
    panel.border = element_blank(),  # 不显示边框
    panel.grid.major = element_blank(),  # 设置网格线
    panel.grid.minor = element_blank(),  # 隐藏次要网格线
    # legend.position = "right",  # 设置图例位置
    legend.text = element_text(size = 8),  # 设置图例文本大小
    legend.title = element_text(size = 10, face = "bold"),  # 设置图例标题大小和加粗
    axis.line.x = element_line(color = "black", size = 0.5),  # 添加x轴线
    axis.line.y = element_line(color = "black", size = 0.5),  # 添加y轴线
    panel.background = element_rect(color = "black", size = 0.5)  # 添加面板边框线
  ) +
  labs(
    x = "SHAP Value",  # 设置 x 轴标签
    y = "",  # 不设置 y 轴标签
    title = ""  # 不设置图表标题
  )+ 
  annotate("text",x = -15, y = shap_long$variable, label = shap_long$mean_value, size = 4, hjust = 0.5)
print(p)
ggsave("SHAP(Ischemic heart disease).pdf", p, device = "pdf", width = 10, height =20)
ggsave("SHAP(Ischemic heart disease).png", p, device = "png", dpi = 600, width = 10, height =15)

data1 <- subset(EC1,EC1$year==2021 & 
                  EC1$age_name=='20+ years' & 
                  EC1$sex_name=='Both' &
                  EC1$cause_name=="Ischemic stroke" &
                  EC1$metric_name== 'Rate' &
                  EC1$measure_name=='Incidence')
data1 <- data1[,c(4,14)]
data2 <- merge(data, data1, by.x = "location_name", by.y = "location_name", all.x = TRUE)
data2 <- data2[,-c(1)]
gene<-c("Low physical activity",'Intimate partner violence','Diet low in calcium','No access to handwashing facility','Chewing tobacco','Diet low in legumes',
        'High temperature','Low temperature','Kidney dysfunction','High LDL cholesterol','Ambient particulate matter pollution','Household air pollution from solid fuels',
        'Ambient ozone pollution','Residential radon','Lead exposure','Iron deficiency','Secondhand smoke',
        'High alcohol use','Drug use','High fasting plasma glucose','High systolic blood pressure','High body-mass index','Low bone mineral density',
        'Diet low in fruits','Diet low in vegetables','Diet low in whole grains','Diet low in nuts and seeds','Diet low in milk','Diet high in red meat',
        'Diet high in processed meat','Diet high in sugar-sweetened beverages','Diet low in fiber','Diet low in seafood omega-3 fatty acids','Diet Low in Omega-6 Polyunsaturated Fatty Acids',
        'Diet high in trans fatty acids','Diet high in sodium','Unsafe water source','Unsafe sanitation','val')
#data<-merge(data, data1, by.x = 'location_name', by.y = 'location_name')
data2 <- data2[,gene]
data2$val <- as.numeric(as.character(data2$val))

library(readxl)
library(dplyr)
library(caret)
library(xgboost)
library(SHAPforxgboost)
library(ggplot2)
pollutant<-"val"
#分割数据#
set.seed(123)
index <- createDataPartition(data2[[pollutant]], p =0.9, list = FALSE)
train<- data2[index, c(1:38, which(colnames(data2) == pollutant))]
test<- data2[-index, c(1:38, which(colnames(data2) == pollutant))]
#创建数值矩阵#
train_x<- data.matrix(train[,1:37])
train_y<- train[[38]]
test_x<- data.matrix(test[,1:37])
test_y<- test[[38]]
#转换为DMatrix#
dtrain<- xgb.DMatrix(data = train_x, label = train_y)
dtest<- xgb.DMatrix(data = test_x, label = test_y)
params<- list(objective ="reg:squarederror", 
              eta=0.1,
              max_depth=6,
              subsample = 0.8,
              colsample_bytree=0.8,
              eval_metric="rmse")
xgb_model<- xgb.train(params = params,
                      data = dtrain,
                      nrounds=100,
                      early_stopping_rounds=10,
                      watchlist= list(train = dtrain, eval = dtest),
                      verbose=0)
shap_values<- shap.values(xgb_model, X_train = train_x)
shap_long <- shap.prep(shap_contrib = shap_values$shap_score, X_train = train_x)
shap.plot.dependence(data_long = shap_long, x =" ", color_feature =" ")#更换需要因素
shap.plot.summary(shap_long) + labs(y ="SHAP value") +
  theme(
    text = element_text(size = 9),  # 设置整体字体大小
    axis.text = element_text(size = 9),  # 设置坐标轴文本大小
    axis.title = element_text(size = 9, face = "bold"),  # 设置坐标轴标题大小和加粗
    plot.title = element_text(size = 9, face = "bold", hjust = 0.5),  # 设置图表标题大小和加粗
    panel.border = element_blank(),  # 不显示边框
    panel.grid.major = element_blank(),  # 设置网格线
    panel.grid.minor = element_blank(),  # 隐藏次要网格线
    # legend.position = "right",  # 设置图例位置
    legend.text = element_text(size = 8),  # 设置图例文本大小
    legend.title = element_text(size = 10, face = "bold"),  # 设置图例标题大小和加粗
    axis.line.x = element_line(color = "black", size = 0.5),  # 添加x轴线
    axis.line.y = element_line(color = "black", size = 0.5),  # 添加y轴线
    panel.background = element_rect(color = "black", size = 0.5)  # 添加面板边框线
  )
shap = shapviz(xgb_model, X_pred = train_x)
# 将 mean_value 保留两位有效数字
shap_long$mean_value <- round(shap_long$mean_value, 2)

#绘制偏相关依赖图
p <- sv_importance(shap, kind = "beeswarm",max_display = 73) +
  theme_bw() +
  theme(
    text = element_text(size = 9),  # 设置整体字体大小
    axis.text = element_text(size =12),  # 设置坐标轴文本大小
    axis.title = element_text(size = 9, face = "bold"),  # 设置坐标轴标题大小和加粗
    plot.title = element_text(size = 9, face = "bold", hjust = 0.5),  # 设置图表标题大小和加粗
    panel.border = element_blank(),  # 不显示边框
    panel.grid.major = element_blank(),  # 设置网格线
    panel.grid.minor = element_blank(),  # 隐藏次要网格线
    # legend.position = "right",  # 设置图例位置
    legend.text = element_text(size = 8),  # 设置图例文本大小
    legend.title = element_text(size = 10, face = "bold"),  # 设置图例标题大小和加粗
    axis.line.x = element_line(color = "black", size = 0.5),  # 添加x轴线
    axis.line.y = element_line(color = "black", size = 0.5),  # 添加y轴线
    panel.background = element_rect(color = "black", size = 0.5)  # 添加面板边框线
  ) +
  labs(
    x = "SHAP Value",  # 设置 x 轴标签
    y = "",  # 不设置 y 轴标签
    title = ""  # 不设置图表标题
  )+ 
  annotate("text",x = -15, y = shap_long$variable, label = shap_long$mean_value, size = 4, hjust = 0.5)
print(p)
ggsave("SHAP(Ischemic stroke).pdf", p, device = "pdf", width = 10, height =20)
ggsave("SHAP(Ischemic stroke).png", p, device = "png", dpi = 600, width = 10, height =15)


data1 <- subset(EC1,EC1$year==2021 & 
                  EC1$age_name=='20+ years' & 
                  EC1$sex_name=='Both' &
                  EC1$cause_name=="Diabetes mellitus" &
                  EC1$metric_name== 'Rate' &
                  EC1$measure_name=='Incidence')
data1 <- data1[,c(4,14)]
data2 <- merge(data, data1, by.x = "location_name", by.y = "location_name", all.x = TRUE)
data2 <- data2[,-c(1)]
gene<-c("Low physical activity",'Intimate partner violence','Diet low in calcium','No access to handwashing facility','Chewing tobacco','Diet low in legumes',
        'High temperature','Low temperature','Kidney dysfunction','High LDL cholesterol','Ambient particulate matter pollution','Household air pollution from solid fuels',
        'Ambient ozone pollution','Residential radon','Lead exposure','Iron deficiency','Secondhand smoke',
        'High alcohol use','Drug use','High fasting plasma glucose','High systolic blood pressure','High body-mass index','Low bone mineral density',
        'Diet low in fruits','Diet low in vegetables','Diet low in whole grains','Diet low in nuts and seeds','Diet low in milk','Diet high in red meat',
        'Diet high in processed meat','Diet high in sugar-sweetened beverages','Diet low in fiber','Diet low in seafood omega-3 fatty acids','Diet Low in Omega-6 Polyunsaturated Fatty Acids',
        'Diet high in trans fatty acids','Diet high in sodium','Unsafe water source','Unsafe sanitation','val')
#data<-merge(data, data1, by.x = 'location_name', by.y = 'location_name')
data2 <- data2[,gene]
data2$val <- as.numeric(as.character(data2$val))

#加载包#
library(readxl)
library(dplyr)
library(caret)
library(xgboost)
library(SHAPforxgboost)
library(ggplot2)

pollutant<-"val"
#分割数据#
set.seed(123)
index <- createDataPartition(data2[[pollutant]], p =0.9, list = FALSE)
train<- data2[index, c(1:38, which(colnames(data2) == pollutant))]
test<- data2[-index, c(1:38, which(colnames(data2) == pollutant))]

train_x<- data.matrix(train[,1:37])
train_y<- train[[38]]
test_x<- data.matrix(test[,1:37])
test_y<- test[[38]]
dtrain<- xgb.DMatrix(data = train_x, label = train_y)
dtest<- xgb.DMatrix(data = test_x, label = test_y)
params<- list(objective ="reg:squarederror", 
              eta=0.1,
              max_depth=6,
              subsample = 0.8,
              colsample_bytree=0.8,
              eval_metric="rmse")
xgb_model<- xgb.train(params = params,
                      data = dtrain,
                      nrounds=100,
                      early_stopping_rounds=10,
                      watchlist= list(train = dtrain, eval = dtest),
                      verbose=0)
shap_values<- shap.values(xgb_model, X_train = train_x)
shap_long <- shap.prep(shap_contrib = shap_values$shap_score, X_train = train_x)
shap.plot.dependence(data_long = shap_long, x =" ", color_feature =" ")#更换需要因素
shap.plot.summary(shap_long) + labs(y ="SHAP value") +
  theme(
    text = element_text(size = 9),  # 设置整体字体大小
    axis.text = element_text(size = 9),  # 设置坐标轴文本大小
    axis.title = element_text(size = 9, face = "bold"),  # 设置坐标轴标题大小和加粗
    plot.title = element_text(size = 9, face = "bold", hjust = 0.5),  # 设置图表标题大小和加粗
    panel.border = element_blank(),  # 不显示边框
    panel.grid.major = element_blank(),  # 设置网格线
    panel.grid.minor = element_blank(),  # 隐藏次要网格线
    # legend.position = "right",  # 设置图例位置
    legend.text = element_text(size = 8),  # 设置图例文本大小
    legend.title = element_text(size = 10, face = "bold"),  # 设置图例标题大小和加粗
    axis.line.x = element_line(color = "black", size = 0.5),  # 添加x轴线
    axis.line.y = element_line(color = "black", size = 0.5),  # 添加y轴线
    panel.background = element_rect(color = "black", size = 0.5)  # 添加面板边框线
  )
shap = shapviz(xgb_model, X_pred = train_x)
# 将 mean_value 保留两位有效数字
shap_long$mean_value <- round(shap_long$mean_value, 2)

#绘制偏相关依赖图
p <- sv_importance(shap, kind = "beeswarm",max_display = 73) +
  theme_bw() +
  theme(
    text = element_text(size = 9),  # 设置整体字体大小
    axis.text = element_text(size =12),  # 设置坐标轴文本大小
    axis.title = element_text(size = 9, face = "bold"),  # 设置坐标轴标题大小和加粗
    plot.title = element_text(size = 9, face = "bold", hjust = 0.5),  # 设置图表标题大小和加粗
    panel.border = element_blank(),  # 不显示边框
    panel.grid.major = element_blank(),  # 设置网格线
    panel.grid.minor = element_blank(),  # 隐藏次要网格线
    # legend.position = "right",  # 设置图例位置
    legend.text = element_text(size = 8),  # 设置图例文本大小
    legend.title = element_text(size = 10, face = "bold"),  # 设置图例标题大小和加粗
    axis.line.x = element_line(color = "black", size = 0.5),  # 添加x轴线
    axis.line.y = element_line(color = "black", size = 0.5),  # 添加y轴线
    panel.background = element_rect(color = "black", size = 0.5)  # 添加面板边框线
  ) +
  labs(
    x = "SHAP Value",  # 设置 x 轴标签
    y = "",  # 不设置 y 轴标签
    title = ""  # 不设置图表标题
  )+ 
  annotate("text",x = -15, y = shap_long$variable, label = shap_long$mean_value, size = 4, hjust = 0.5)
print(p)
ggsave("SHAP(Diabetes mellitus).pdf", p, device = "pdf", width = 10, height =20)
ggsave("SHAP(Diabetes mellitus).png", p, device = "png", dpi = 600, width = 10, height =15)
