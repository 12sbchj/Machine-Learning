
library(ggmap)
library(rgdal)
library(maps)

EC <- read.csv('../IHME-GBD_2021_DATA-1e92dcc0-1/IHME-GBD_2021_DATA-4f4711e4-1.csv',header = T)
data <-subset(EC,EC$year=="2021" & 
                EC$age_name=='All ages' & 
                EC$sex_name=='Both' &
                EC$cause_name=="Ischemic heart disease" &
                EC$metric_name== 'Number' &
                EC$measure_name=='Deaths')
data <- data[,c(12,16)]
data$val1 <- abs(data$val)
library(MASS)
library(dplyr)
f <- glm.nb(val1~rei_name, data = data)
summary(f)
coefficients <- summary(f)$coefficients
rr_results <- data.frame(
  variable = rownames(coefficients),
  Estimate = coefficients[, "Estimate"],
  P = coefficients[, "Pr(>|z|)"],
  RR = exp(coefficients[, "Estimate"]),
  lower_CI = exp(coefficients[, "Estimate"] - 1.96 * coefficients[, "Std. Error"]),
  upper_CI = exp(coefficients[, "Estimate"] + 1.96 * coefficients[, "Std. Error"])
)
print(rr_results)
rr<- rr_results[,c(1,4,5,6,3)]
rr <- rr %>% mutate(variable = sub("rei_name", "", variable))
rr<- rr[-1,]
data <-subset(EC,EC$year=="2021" & 
                EC$age_name=='All ages' & 
                EC$sex_name=='Both' &
                EC$cause_name=="Diabetes mellitus" &
                EC$metric_name== 'Number' &
                EC$measure_name=='Deaths')
data <- data[,c(12,16)]
data$val1 <- abs(data$val)
library(MASS)
library(dplyr)
f <- glm.nb(val1~rei_name, data = data)
summary(f)
coefficients <- summary(f)$coefficients
rr_results <- data.frame(
  variable = rownames(coefficients),
  Estimate = coefficients[, "Estimate"],
  P = coefficients[, "Pr(>|z|)"],
  RR = exp(coefficients[, "Estimate"]),
  lower_CI = exp(coefficients[, "Estimate"] - 1.96 * coefficients[, "Std. Error"]),
  upper_CI = exp(coefficients[, "Estimate"] + 1.96 * coefficients[, "Std. Error"])
)
print(rr_results)
rr<- rr_results[,c(1,4,5,6,3)]
rr <- rr %>% mutate(variable = sub("rei_name", "", variable))
rr<- rr[-1,]
data <-subset(EC,EC$year=="2021" & 
                EC$age_name=='All ages' & 
                EC$sex_name=='Both' &
                EC$cause_name=="Ischemic stroke" &
                EC$metric_name== 'Number' &
                EC$measure_name=='Deaths')
data <- data[,c(12,16)]
data$val1 <- abs(data$val)
library(MASS)
library(dplyr)
f <- glm.nb(val1~rei_name, data = data)
summary(f)
coefficients <- summary(f)$coefficients
rr_results <- data.frame(
  variable = rownames(coefficients),
  Estimate = coefficients[, "Estimate"],
  P = coefficients[, "Pr(>|z|)"],
  RR = exp(coefficients[, "Estimate"]),
  lower_CI = exp(coefficients[, "Estimate"] - 1.96 * coefficients[, "Std. Error"]),
  upper_CI = exp(coefficients[, "Estimate"] + 1.96 * coefficients[, "Std. Error"])
)
print(rr_results)
rr<- rr_results[,c(1,4,5,6,3)]
rr <- rr %>% mutate(variable = sub("rei_name", "", variable))
rr<- rr[-1,]
data <-subset(EC,EC$year=="2021" & 
                EC$age_name=='All ages' & 
                EC$sex_name=='Both' &
                EC$cause_name=="Diabetes mellitus" &
                EC$metric_name== 'Number' &
                EC$measure_name=='Deaths')
data <- data[,c(4,12,16)]
data$val1 <- abs(data$val)
library(MASS)
library(dplyr)
f <- glm.nb(val1~location_name, data = data)
summary(f)
# 提取系数并计算相对风险（RR）
coefficients <- summary(f)$coefficients
rr_results <- data.frame(
  variable = rownames(coefficients),
  Estimate = coefficients[, "Estimate"],
  P = coefficients[, "Pr(>|z|)"],
  RR = exp(coefficients[, "Estimate"]),
  lower_CI = exp(coefficients[, "Estimate"] - 1.96 * coefficients[, "Std. Error"]),
  upper_CI = exp(coefficients[, "Estimate"] + 1.96 * coefficients[, "Std. Error"])
)
# 查看相对风险结果
print(rr_results)
rr<- rr_results[,c(1,4,5,6,3)]
# 使用 sub 函数删除 "location_name" 部分
rr <- rr %>% mutate(variable = sub("location_name", "", variable))
rr<- rr[-1,]

data <-subset(EC,EC$year=="2021" & 
                EC$age_name=='All ages' & 
                EC$sex_name=='Both' &
                EC$cause_name=="Ischemic heart disease" &
                EC$metric_name== 'Number' &
                EC$measure_name=='Deaths')
data <- data[,c(4,12,16)]
data$val1 <- abs(data$val)
library(MASS)
library(dplyr)
f <- glm.nb(val1~location_name, data = data)
summary(f)
# 提取系数并计算相对风险（RR）
coefficients <- summary(f)$coefficients
rr_results <- data.frame(
  variable = rownames(coefficients),
  Estimate = coefficients[, "Estimate"],
  P = coefficients[, "Pr(>|z|)"],
  RR = exp(coefficients[, "Estimate"]),
  lower_CI = exp(coefficients[, "Estimate"] - 1.96 * coefficients[, "Std. Error"]),
  upper_CI = exp(coefficients[, "Estimate"] + 1.96 * coefficients[, "Std. Error"])
)
# 查看相对风险结果
print(rr_results)
rr<- rr_results[,c(1,4,5,6,3)]
# 使用 sub 函数删除 "location_name" 部分
rr <- rr %>% mutate(variable = sub("location_name", "", variable))
rr<- rr[-1,]

data <-subset(EC,EC$year=="2021" & 
                EC$age_name=='All ages' & 
                EC$sex_name=='Both' &
                EC$cause_name=="Ischemic stroke" &
                EC$metric_name== 'Number' &
                EC$measure_name=='Deaths')
data <- data[,c(4,12,16)]
data$val1 <- abs(data$val)
library(MASS)
library(dplyr)
f <- glm.nb(val1~location_name, data = data)
summary(f)
# 提取系数并计算相对风险（RR）
coefficients <- summary(f)$coefficients
rr_results <- data.frame(
  variable = rownames(coefficients),
  Estimate = coefficients[, "Estimate"],
  P = coefficients[, "Pr(>|z|)"],
  RR = exp(coefficients[, "Estimate"]),
  lower_CI = exp(coefficients[, "Estimate"] - 1.96 * coefficients[, "Std. Error"]),
  upper_CI = exp(coefficients[, "Estimate"] + 1.96 * coefficients[, "Std. Error"])
)
# 查看相对风险结果
print(rr_results)
rr<- rr_results[,c(1,4,5,6,3)]
# 使用 sub 函数删除 "location_name" 部分
rr <- rr %>% mutate(variable = sub("location_name", "", variable))
rr<- rr[-1,]
res<-read.csv("../区域划分/结果/分析二/糖脑/Ischemic stroke危险因素1.csv")
colnames(res) <- c("Indicator","RR","low","up","pv")#重命名列
res$p <- signif(res$pv, 2) %>% paste0("p = ", .)#添加P值列
res$p[is.na(res$pv)] <- NA #将P为NA的赋值NA
res2 <- data.frame(p.value=res$pv,#将P值错误率上下限和指标存为数据框
                   RR=res$RR,
                   HR.95L=res$low,
                   HR.95H=res$up,
                   Indicator=res$Indicator )
# rownames(res2) <- res2$Indicator#重命名行名为指标名
library(tidyr)
hz <- paste(round(res2$RR,3),
            "(",round(res2$HR.95L,3),
            "-",round(res2$HR.95H,3),")",sep = "")#将hz值和95置信区间粘贴到一起
hz
hz[c(1,7)] <- ""###
# hz[c(6,12,16,18,22)] <- ""###
tabletext <- cbind(c(NA,res2$Indicator),c("RR(95% CI)",hz))
nrow(tabletext) + 1
library(forestplot)
library(checkmate)
library(abind)
library(forestploter)

p <- forestplot(labeltext=tabletext, 
                # graph.pos=4,  #为Pvalue箱线图所在的位置
                # is.summary = c(T,rep(T,1),T,rep(F,2),T,rep(F,2),T,rep(F,2)),#那行是汇总行
                graph.pos = ncol(tabletext),  # 图形放在倒数第二列
                col=fpColors(box="black", lines="black", zero = "black"),
                mean=c(NA,res2$RR),
                lower=c(NA,res2$HR.95L), #95%置信区间下限
                upper=c(NA,res2$HR.95H), #95%置信区间上限
                boxsize=0.2,lwd.ci=1,   #箱子大小，线的宽度
                ci.vertices.height = 0.08,ci.vertices=F, #置信区间用线宽、高、型
                zero=1,lwd.zero=0.5,    #zero线宽 基准线的位置
                colgap=unit(5,"mm"),    #列间隙
                xticks = c(0.9,1.0,1.5), #横坐标刻度
                lwd.xaxis=2,            #X轴线宽
                lineheight = unit(1.4,"cm"), #固定行高
                graphwidth = unit(.6,"npc"), #图在表中的宽度比例
                cex=2, fn.ci_norm = fpDrawNormalCI, #误差条显示方式
                hrzl_lines = list("2" = gpar(col = "black", lty = 1, lwd = 2)),
                # hrzl_lines=list("2" = gpar(lwd=2, col="black"),
                #                 "3" = gpar(lwd=2, col="black"), #第三行顶部加黑线，引号内数字标记行位置
                #                 "59" = gpar(lwd=2, col="black")),#最后一行底部加黑线,"16"中数字为nrow(tabletext)+1
                mar=unit(rep(8, times = 8), "cm"),#图形页边距
                #fpTxtGp函数中的cex参数设置各个组件的大小
                txt_gp=fpTxtGp(label=gpar(cex=1.5, fontfamily = "Times"),#全部字体大小
                               ticks=gpar(cex=1, fontface = "bold", fontfamily = "Times"),
                               xlab=gpar(cex = 2.5, fontface = "bold", fontfamily = "Times"),
                               title=gpar(cex =2.5, fontface = "bold", fontfamily = "Times")),#标题字大小
                xlab="RR",
                grid = T,
                title = "",
                # clip = c(0,25)
)# 
p
pdf(file = "../区域划分/结果/分析二/糖脑/Ischemic stroke危险因素森林图.pdf", family = "Times", height = 10, width = 12, onefile = F)
p
dev.off()
png(file = "../区域划分/结果/分析二/糖脑/Ischemic stroke危险因素森林图.png", family = "Times", height = 10, width = 12,units = "in",res = 600)#
p
dev.off()

res<-read.csv("../区域划分/结果/分析二/糖脑/Diabetes mellitus危险因素1.csv")
colnames(res) <- c("Indicator","RR","low","up","pv")#重命名列
res$p <- signif(res$pv, 2) %>% paste0("p = ", .)#添加P值列
res$p[is.na(res$pv)] <- NA #将P为NA的赋值NA
res2 <- data.frame(p.value=res$pv,#将P值错误率上下限和指标存为数据框
                   RR=res$RR,
                   HR.95L=res$low,
                   HR.95H=res$up,
                   Indicator=res$Indicator )
# rownames(res2) <- res2$Indicator#重命名行名为指标名
library(tidyr)
hz <- paste(round(res2$RR,3),
            "(",round(res2$HR.95L,3),
            "-",round(res2$HR.95H,3),")",sep = "")#将hz值和95置信区间粘贴到一起
hz
hz[c(1,7)] <- ""###
# hz[c(6,12,16,18,22)] <- ""###
tabletext <- cbind(c(NA,res2$Indicator),c("RR(95% CI)",hz))
nrow(tabletext) + 1
library(forestplot)
library(checkmate)
library(abind)
library(forestploter)

p <- forestplot(labeltext=tabletext, 
                graph.pos=ncol(tabletext),  #为Pvalue箱线图所在的位置
                # is.summary = c(T,rep(T,1),T,rep(F,2),T,rep(F,2),T,rep(F,2)),#那行是汇总行
                col=fpColors(box="black", lines="black", zero = "black"),
                mean=c(NA,res2$RR),
                lower=c(NA,res2$HR.95L), #95%置信区间下限
                upper=c(NA,res2$HR.95H), #95%置信区间上限
                boxsize=0.2,lwd.ci=1,   #箱子大小，线的宽度
                ci.vertices.height = 0.08,ci.vertices=F, #置信区间用线宽、高、型
                zero=1,lwd.zero=0.5,    #zero线宽 基准线的位置
                colgap=unit(5,"mm"),    #列间隙
                xticks = c(0.9,1.0,1.5), #横坐标刻度
                lwd.xaxis=2,            #X轴线宽
                lineheight = unit(1.4,"cm"), #固定行高
                graphwidth = unit(.6,"npc"), #图在表中的宽度比例
                cex=2, fn.ci_norm = fpDrawNormalCI, #误差条显示方式
                hrzl_lines = list("2" = gpar(col = "black", lty = 1, lwd = 2)),
                # hrzl_lines=list("2" = gpar(lwd=2, col="black"),
                #                 "3" = gpar(lwd=2, col="black"), #第三行顶部加黑线，引号内数字标记行位置
                #                 "59" = gpar(lwd=2, col="black")),#最后一行底部加黑线,"16"中数字为nrow(tabletext)+1
                mar=unit(rep(8, times = 4), "cm"),#图形页边距
                #fpTxtGp函数中的cex参数设置各个组件的大小
                txt_gp=fpTxtGp(label=gpar(cex=1.5, fontfamily = "Times"),#全部字体大小
                               ticks=gpar(cex=1, fontface = "bold", fontfamily = "Times"),
                               xlab=gpar(cex = 2.5, fontface = "bold", fontfamily = "Times"),
                               title=gpar(cex =2.5, fontface = "bold", fontfamily = "Times")),#标题字大小
                xlab="RR",
                grid = T,
                title = "",
                # clip = c(0,25)
)# 
p
pdf(file = "../区域划分/结果/分析二/糖脑/Diabetes mellitus危险因素森林图.pdf", family = "Times", height = 8, width = 12, onefile = F)
p
dev.off()
png(file = "../区域划分/结果/分析二/糖脑/Diabetes mellitus危险因素森林图.png", family = "Times", height = 8, width = 12,units = "in",res = 600)#
p
dev.off()


res<-read.csv("../区域划分/结果/分析二/糖心/Ischemic heart disease危险因素1.csv")
colnames(res) <- c("Indicator","RR","low","up","pv")#重命名列
res$p <- signif(res$pv, 2) %>% paste0("p = ", .)#添加P值列
res$p[is.na(res$pv)] <- NA #将P为NA的赋值NA
res2 <- data.frame(p.value=res$pv,#将P值错误率上下限和指标存为数据框
                   RR=res$RR,
                   HR.95L=res$low,
                   HR.95H=res$up,
                   Indicator=res$Indicator )
# rownames(res2) <- res2$Indicator#重命名行名为指标名
library(tidyr)
hz <- paste(round(res2$RR,3),
            "(",round(res2$HR.95L,3),
            "-",round(res2$HR.95H,3),")",sep = "")#将hz值和95置信区间粘贴到一起
hz
hz[c(1,7)] <- ""###
# hz[c(6,12,16,18,22)] <- ""###
tabletext <- cbind(c(NA,res2$Indicator),c("RR(95% CI)",hz))
nrow(tabletext) + 1
library(forestplot)
library(checkmate)
library(abind)
library(forestploter)

p <- forestplot(labeltext=tabletext, 
                graph.pos=ncol(tabletext),  #为Pvalue箱线图所在的位置
                # is.summary = c(T,rep(T,1),T,rep(F,2),T,rep(F,2),T,rep(F,2)),#那行是汇总行
                col=fpColors(box="black", lines="black", zero = "black"),
                mean=c(NA,res2$RR),
                lower=c(NA,res2$HR.95L), #95%置信区间下限
                upper=c(NA,res2$HR.95H), #95%置信区间上限
                boxsize=0.2,lwd.ci=1,   #箱子大小，线的宽度
                ci.vertices.height = 0.08,ci.vertices=F, #置信区间用线宽、高、型
                zero=1,lwd.zero=0.5,    #zero线宽 基准线的位置
                colgap=unit(5,"mm"),    #列间隙
                xticks = c(0.9,1.0,1.5), #横坐标刻度
                lwd.xaxis=2,            #X轴线宽
                lineheight = unit(1.4,"cm"), #固定行高
                graphwidth = unit(.6,"npc"), #图在表中的宽度比例
                cex=2, fn.ci_norm = fpDrawNormalCI, #误差条显示方式
                hrzl_lines = list("2" = gpar(col = "black", lty = 1, lwd = 2)),
                # hrzl_lines=list("2" = gpar(lwd=2, col="black"),
                #                 "3" = gpar(lwd=2, col="black"), #第三行顶部加黑线，引号内数字标记行位置
                #                 "59" = gpar(lwd=2, col="black")),#最后一行底部加黑线,"16"中数字为nrow(tabletext)+1
                mar=unit(rep(10, times = 9), "cm"),#图形页边距
                #fpTxtGp函数中的cex参数设置各个组件的大小
                txt_gp=fpTxtGp(label=gpar(cex=1.5, fontfamily = "Times"),#全部字体大小
                               ticks=gpar(cex=1, fontface = "bold", fontfamily = "Times"),
                               xlab=gpar(cex = 2.5, fontface = "bold", fontfamily = "Times"),
                               title=gpar(cex =2.5, fontface = "bold", fontfamily = "Times")),#标题字大小
                xlab=" ",
                grid = T,
                title = "",
                # clip = c(0,25)
)# 
p
pdf(file = "../区域划分/结果/分析二/糖心/Ischemic heart disease危险因素森林图.pdf", family = "Times", height = 8, width = 12, onefile = F)
p
dev.off()
png(file = "../区域划分/结果/分析二/糖心/Ischemic heart disease危险因素森林图.png", family = "Times", height = 8, width = 12,units = "in",res = 600)#
p
dev.off()

res<-read.csv("../区域划分/结果/分析二/糖心/Diabetes mellitus危险因素1.csv")
colnames(res) <- c("Indicator","RR","low","up","pv")#重命名列
res$p <- signif(res$pv, 2) %>% paste0("p = ", .)#添加P值列
res$p[is.na(res$pv)] <- NA #将P为NA的赋值NA
res2 <- data.frame(p.value=res$pv,#将P值错误率上下限和指标存为数据框
                   RR=res$RR,
                   HR.95L=res$low,
                   HR.95H=res$up,
                   Indicator=res$Indicator )
# rownames(res2) <- res2$Indicator#重命名行名为指标名
library(tidyr)
hz <- paste(round(res2$RR,3),
            "(",round(res2$HR.95L,3),
            "-",round(res2$HR.95H,3),")",sep = "")#将hz值和95置信区间粘贴到一起
hz
hz[c(1,7)] <- ""###
# hz[c(6,12,16,18,22)] <- ""###
tabletext <- cbind(c(NA,res2$Indicator),c("P value",ifelse(res2$p.value<0.05,"< 0.05",round(res2$p.value,4))),c("RR(95% CI)",hz))
nrow(tabletext) + 1
library(forestplot)
library(checkmate)
library(abind)
library(forestploter)

p <- forestplot(labeltext=tabletext, 
                graph.pos=4,  #为Pvalue箱线图所在的位置
                # is.summary = c(T,rep(T,1),T,rep(F,2),T,rep(F,2),T,rep(F,2)),#那行是汇总行
                col=fpColors(box="#ED7474", lines="#95A2BF", zero = "#F8C9BA"),
                mean=c(NA,res2$RR),
                lower=c(NA,res2$HR.95L), #95%置信区间下限
                upper=c(NA,res2$HR.95H), #95%置信区间上限
                boxsize=0.1,lwd.ci=3,   #箱子大小，线的宽度
                ci.vertices.height = 0.08,ci.vertices=TRUE, #置信区间用线宽、高、型
                zero=1,lwd.zero=0.5,    #zero线宽 基准线的位置
                colgap=unit(5,"mm"),    #列间隙
                xticks = c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0), #横坐标刻度
                lwd.xaxis=2,            #X轴线宽
                lineheight = unit(1.4,"cm"), #固定行高
                graphwidth = unit(.6,"npc"), #图在表中的宽度比例
                cex=0.9, fn.ci_norm = fpDrawCircleCI, #误差条显示方式
                hrzl_lines = list("2" = gpar(col = "black", lty = 1, lwd = 2)),
                # hrzl_lines=list("2" = gpar(lwd=2, col="black"),
                #                 "3" = gpar(lwd=2, col="black"), #第三行顶部加黑线，引号内数字标记行位置
                #                 "59" = gpar(lwd=2, col="black")),#最后一行底部加黑线,"16"中数字为nrow(tabletext)+1
                mar=unit(rep(6, times = 4), "cm"),#图形页边距
                #fpTxtGp函数中的cex参数设置各个组件的大小
                txt_gp=fpTxtGp(label=gpar(cex=1.5, fontfamily = "Times"),#全部字体大小
                               ticks=gpar(cex=1, fontface = "bold", fontfamily = "Times"),
                               xlab=gpar(cex = 2.5, fontface = "bold", fontfamily = "Times"),
                               title=gpar(cex =2.5, fontface = "bold", fontfamily = "Times")),#标题字大小
                xlab="RR",
                grid = T,
                title = "",
                # clip = c(0,25)
)# 
p
pdf(file = "../区域划分/结果/分析二/糖心/Diabetes mellitus危险因素森林图.pdf", family = "Times", height = 8, width = 15, onefile = F)
p
dev.off()
png(file = "../区域划分/结果/分析二/糖心/Diabetes mellitus危险因素森林图.png", family = "Times", height = 8, width = 15,units = "in",res = 600)#
p
dev.off()
