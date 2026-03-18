# bph_new_code




####导入数据---------------------------------------------------------------------------------------
library(readxl)
贵宾区前列腺数据_脱敏 <- read_excel("D:/A学习资料/大四下/毕设/贵宾区前列腺数据 - 脱敏.xlsx", 
                          col_types = c("text", "numeric", "numeric", 
                                        "numeric", "numeric", "text", "numeric", 
                                        "text", "numeric", "numeric", "numeric", 
                                        "numeric", "text", "numeric", "text", 
                                        "numeric", "numeric", "text", "text", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "text", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "text", "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "text", "text", 
                                        "text", "text", "text", "text", "text", 
                                        "text", "text", "text", "text", "text", 
                                        "text", "text", "text", "numeric", 
                                        "numeric", "text", "text", "text", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "text", "text", "text", 
                                        "text", "text", "text"))
View(贵宾区前列腺数据_脱敏)

library(readxl)
BPH数据副本 <- read_excel("BPH数据副本.xlsx", 
                      col_types = c("text", "text", "numeric", 
                                    "text", "numeric", "numeric", "numeric", 
                                    "numeric", "text", "numeric", "text", 
                                    "numeric", "numeric", "text", "text", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "text", "numeric", "numeric", 
                                    "numeric", "text", "text", "numeric", 
                                    "numeric", "numeric", "text", "numeric", 
                                    "numeric", "text", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "text", "text", "text", "text", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "text", "numeric"))
View(BPH数据副本)
bph_data <- as.data.frame(BPH数据副本)
save(bph_data, file = "D:/A学习资料/大四下/毕设/数据/BPH_data.RData")
save(X_data, file = "D:/A学习资料/大四下/毕设/数据/X_data.RData")
save(T_data, file = "D:/A学习资料/大四下/毕设/数据/T_data.RData")

# 导出数据
library(writexl)
write_xlsx(bph_data, path = "D:/A学习资料/大四下/毕设/数据/BPH_data_0315.xlsx")
write_xlsx(T_data, path = "D:/A学习资料/大四下/毕设/数据/T_data.xlsx")
write_xlsx(X_data, path = "D:/A学习资料/大四下/毕设/数据/X_data_0315.xlsx")

bph_data <- type.convert(bph_data, as.is = FALSE)   # as.is=FALSE 表示把字符型数字自动转成数值/整数
str(bph_data)
install.packages("gtable")
install.packages("ggplot2")
library(ggplot2)
bph_data <- as.data.frame(
  lapply(df, function(col) {
    # 如果是因子，先转字符再转数值；否则直接转数值
    if (is.factor(col)) {
      as.numeric(as.character(col))
    } else {
      as.numeric(col)
    }
  })
)

ggplot(bph_data, aes(y = bph_data$`体重 | 010002`, x =bph_data$`钙化形状（点状1，带状2，斑片状3`, color = bph_data$病理症状)) +
  geom_jitter(width = 0.2, size = 3, alpha = 0.7) +
  labs(title = "图", x = "x", y = "y") +
  theme(text = element_text(family = "SimHei"), plot.title = element_text(hjust = 0.5))



#####数据整理----------------------------------------------------------------------------------------------------------------------------
###新建年龄组变量，当年龄小于24时值为0，25到44值为1，45到64为2，大于等于65为3
library(dplyr)
bph_data <- bph_data %>%
  mutate(
    age_group = case_when(
      bph_data$年龄 <= 30        ~ 1,
      bph_data$年龄 >= 31 & bph_data$年龄 <= 50 ~ 2,
      bph_data$年龄 >= 51 & bph_data$年龄 <= 70 ~ 3,
      bph_data$年龄 >= 71       ~ 4
    )
  )
bph_data$`游离前列腺特异性抗原，正常0，1-2倍1,2倍以上2） | 130192` <-  as.numeric(bph_data$`游离前列腺特异性抗原，正常0，1-2倍1,2倍以上2） | 130192`)
str(bph_data)
###新建BMI变量
bph_data <- bph_data[!is.na(bph_data$`身高 | 010001`), ]
bph_data$BMI <- bph_data$`体重 | 010002` / ((bph_data$`身高 | 010001`/100) ^ 2)
#新建前列腺体积变量
bph_data$PV <- 0.52*bph_data$长径*bph_data$短径*bph_data$厚径

bph_data <- bph_data %>%
  mutate(
    PV1 = case_when(
      bph_data$PV <= 25        ~ 0,
      bph_data$PV > 25 ~ 1,
      
    )
  )
table(bph_data$PV1)


bph_data$是否吸烟 <- ifelse(grepl("无", bph_data$`吸烟史 | 570380`), 0, 1)
bph_data$病史 <- ifelse(grepl("无", bph_data$`既往史 | 020015`), 0, 1)
##新建是否高血压列
bph_data$`血压 | 010003`
library(tidyr)
bph_data <- bph_data %>% separate(`血压 | 010003`, into = c("收缩压(SBP)","舒张压(DBP)"), sep = "/", convert = TRUE)

bph_data <- bph_data %>%
  mutate(
    `高血压` = case_when(
      bph_data$`收缩压(SBP)` <= 135        ~ 0,
      bph_data$`收缩压(SBP)` > 135 ~ 1,
      
    )
  )


##整理病理结果
str(bph_data$病理症状)
bph_data$病理症状
bph_data <- bph_data %>%
  mutate(
    `病理结果`  = case_when(
      bph_data$病理症状 == 1       ~ 1,
      bph_data$病理症状 == 2       ~ 1,
      bph_data$病理症状 == 3       ~ 0,
      bph_data$病理症状 == 0       ~ 0
    )
  )



table(bph_data$病理结果)
table(bph_data$高血压)
table(bph_data$病史)
table(bph_data$PV1)
summary(bph_data$PV)
boxplot(bph_data$PV, horizontal = TRUE, col = "lightgreen",
        main = "前列腺体积箱线图")

# 删除列
data2 <- data2 %>% select(-c('疾病史/用药史', '早餐'))

#合并列
library(dplyr)
bph_data <- bph_data %>%
  mutate(`腰围` = coalesce(`腰围 | 010005`, `腰围 | 020018`))   # coalesce 依次取第一个非 NA

bph_data <- bph_data %>%
  mutate(`臀围` = coalesce(`臀围 | 010006`, `臀围 | 570272`))

##改为因子性
bph_data$PV1 <- as.factor(bph_data$PV1)
bph_data$是否吸烟 <- as.factor(bph_data$是否吸烟)
bph_data$病史 <- as.factor(bph_data$病史)
bph_data$高血压 <- as.factor(bph_data$高血压)
str(bph_data)
summary(bph_data)



# 加载必要的包
library(tidyverse)    # 数据处理
library(tableone)     # 描述性统计表格
library(rms)          # Logistic回归、列线图
library(pROC)         # ROC曲线
library(caret)        # 数据划分
library(glmnet)       # LASSO回归
library(rmda)         # 决策曲线分析

summary(bph_data)
table(bph_data$病理症状)
install.packages(c("tidyverse","caret","glmnet","pROC","xgboost","randomForest","corrplot"))
library(tidyverse)       # 数据清洗与可视化
library(caret)           # 建模与数据划分核心包
library(glmnet)          # LASSO回归核心包
library(pROC)            # ROC曲线与AUC计算（预测模型金标准）
library(xgboost)         # XGBoost模型
library(randomForest)    # 随机森林模型
library(corrplot)        # 相关性热力图
library(dplyr)

# 旧名向量
old  <- c( "血糖(GLU) | 060075",
           "总前列腺特异性抗原(PSA)（正常0,4-10标记为1，大于10标记2） | 130105",
           "游离前列腺特异性抗原，正常0，1-2倍1,2倍以上2） | 130192"
)
# 新名向量（顺序一一对应）
new  <- c("血糖(GLU)", "总PSA", "游离PSA")

# 一次性改名
X_data <- X_data %>% rename(!!!setNames(old, new))



vars <- c("PV",
          "BMI",
          "年龄",
          "腰围",
          "臀围",
          "总胆固醇(CHOL) | 060061",
          "甘油三酯(TG) | 060060",
          "低密度脂蛋白胆固醇(LDL) | 060063",
          "高密度脂蛋白胆固醇(HDL) | 060062",
          "总蛋白(TP) | 060042",
          "球蛋白(GLB) | 060044",
          "白蛋白(ALB) | 060043",
          "肌酐(Cr) | 060052",
          "尿素(BUN) | 060051",
          "尿酸(UA) | 060053",
          "血糖(GLU) | 060075",
          "总前列腺特异性抗原(PSA)（正常0,4-10标记为1，大于10标记2） | 130105",
          "游离前列腺特异性抗原，正常0，1-2倍1,2倍以上2） | 130192",
          "总维生素D | 570423",
          "是否吸烟",
          "病史",
          "高血压",
          "PV1")
X_data <- bph_data[,vars]
str(X_data)

table(X_data$PV1)
X_data$PV1 <- as.factor(X_data$PV1)
# 改完数据记得改名称；重命名变量（去除特殊字符和编码，与论文特征对齐）
X_data <- X_data %>%
  rename(
    # 核心预测特征
    
    总胆固醇 = `总胆固醇(CHOL) | 060061`,
    甘油三酯 = `甘油三酯(TG) | 060060`,
    低密度脂蛋白 = `低密度脂蛋白胆固醇(LDL) | 060063`,
    高密度脂蛋白 = `高密度脂蛋白胆固醇(HDL) | 060062`,
    总蛋白 = `总蛋白(TP) | 060042`,
    球蛋白 = `球蛋白(GLB) | 060044`,
    白蛋白 = `白蛋白(ALB) | 060043`,
    肌酐 = `肌酐(Cr) | 060052`,
    尿素 = `尿素(BUN) | 060051`,
    尿酸 = `尿酸(UA) | 060053`,
    血糖 = `血糖(GLU) | 060075`,
    tPSA = `总前列腺特异性抗原(PSA)（正常0,4-10标记为1，大于10标记2） | 130105`,
    fPSA = `游离前列腺特异性抗原，正常0，1-2倍1,2倍以上2） | 130192`,
    总维生素D = `总维生素D | 570423`
    # 无需修改的变量：PV、PV1、BMI、年龄、是否吸烟、病史、高血压
  )

#------------新建维生素D不为空的子集-------------------------------------------------------------------

library(dplyr)
D_data <- X_data %>% filter(!is.na(总维生素D), 总维生素D != "")
summary(D_data)


X_data <- X_data %>% mutate_all(~ifelse(is.na(.), median(., na.rm=T), .))
# 数据标准化（逻辑回归/XGBoost必需，随机森林无需标准化）
preProc <- preProcess(X_data, method = c("center", "scale"))
X_scaled <- predict(preProc, X_data)




# =====================热力图=====================
### 3.1 相关性分析-剔除共线性变量
# 看每列类型
str(X_scaled)

cor_matrix <- cor(X_scaled, method = "spearman")
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.4, 
         addCoef.col = "grey30",number.cex    = 0.4, 
         title = "变量相关性热力图")

cor_matrix2 <- cor(X_scaled, method = "kendall")
corrplot(cor_matrix2, method = "color", type = "upper", tl.cex = 0.6, 
         addCoef.col = "grey30",number.cex    = 0.4, 
         title = "变量相关性热力图")



corrplot(cor_matrix,
         method = "color",   # 彩色方块，比 circle 更直观
         type   = "upper",   # 只画右上角
         tl.cex = 0.8,       # 变量名字体大小，按变量多少自己调
         tl.col = "black",
         order  = "hclust",  # 聚类排序，让高相关聚在一起
         addCoef.col = "grey30",  # 把相关系数写进格子里
         number.cex = 0.7)
high_corr <- findCorrelation(cor_matrix, cutoff = 0.7) # 剔除|r|>0.7的变量
X_corr <- X_scaled[, -high_corr]











#####要不要---------------------------------
### 3.2 单因素逻辑回归筛选（P<0.05保留）
single_p <- sapply(colnames(X_corr), function(x) {
  fit <- glm(paste0("BPH ~ ", x), data = cbind(data$BPH, X_corr), family = binomial())
  coef(summary(fit))[2,4]
})
single_vars <- names(single_p[single_p < 0.05])
X_single <- X_corr[, single_vars]

### 3.3 LASSO回归筛选（核心！最优特征选择）
x <- as.matrix(X_single)
y <- as.numeric(as.character(data$BPH))
set.seed(123) # 固定随机种子，结果可重复（毕设必做）
cv_lasso <- cv.glmnet(x, y, family = "binomial", alpha = 1, nfolds = 10) # alpha=1=LASSO

# 绘制LASSO核心可视化图（论文必加）
par(mfrow = c(1,2))
plot(cv_lasso$glmnet.fit, xvar = "lambda", label = T, main = "LASSO系数轨迹图")
abline(v = log(cv_lasso$lambda.min), lty = 2, col = "red")
plot(cv_lasso, main = "LASSO交叉验证误差图")

# 提取最终核心变量（系数≠0）
lasso_coef <- coef(cv_lasso, s = cv_lasso$lambda.min)
lasso_vars <- names(lasso_coef[lasso_coef != 0])[-1]
X_final <- X_single[, lasso_vars]
cat("LASSO筛选出的核心变量：", lasso_vars, "\n")




#####---------------1.16/单因素逻辑回归筛选变量---------------
str(X_data)
str(train_data)

# 加载包
library(dplyr)
library(tidyr)


##数据拆分（7:3训练集/测试集）
set.seed(123)  # 固定随机种子
trainIndex <- createDataPartition(X_data$PV1, p = 0.7, list = FALSE)
train_data <- X_data[trainIndex, ]  # 训练集（用于特征筛选）
test_data <- X_data[-trainIndex, ]  # 测试集（仅用于模型验证）
# 查看原始变量名（确认需修改的列）
colnames(train_data)



# 加载核心包
library(caret)    # 数据拆分与建模
library(rms)      # 回归分析与结果可视化
library(glmnet)   # 逻辑回归建模
library(pROC)     # 模型评价辅助
library(dplyr)    # 数据处理






# 查看各变量缺失率（重点关注核心特征）
missing_rate <- D_data %>%
  summarise_all(~sum(is.na(.))/n()*100) %>%
  pivot_longer(everything(), names_to = "变量", values_to = "缺失率(%)") %>%
  arrange(desc(`缺失率(%)`))

print(missing_rate)



# 单因素回归结果存储
vars1 <- c("PV",
           "BMI",
           "年龄",
           "腰围",
           "臀围",
           "总胆固醇",
           "甘油三酯",
           "低密度脂蛋白",
           "高密度脂蛋白",
           "总蛋白",
           "球蛋白",
           "白蛋白",
           "肌酐",
           "尿素",
           "尿酸",
           "血糖",
           "tPSA",
           "fPSA",
           "总维生素D",
           "是否吸烟",
           "病史",
           "高血压")
univar_result <- data.frame()
for (var in vars1) {
  # 构建单变量逻辑回归模型
  formula <- as.formula(paste("PV1 ~", var))
  model <- glm(formula, data = train_data, family = binomial())
  # 提取OR值、95%CI、P值
  coef <- exp(coef(summary(model))[2, ])  # OR值
  ci <- exp(confint(model))[2, ]          # 95%CI
  p_val <- summary(model)$coefficients[2, 4]  # P值
  # 整合结果
  univar_result <- rbind(univar_result,
                         data.frame(变量 = var,
                                    OR = round(coef[1], 3),
                                    CI_lower = round(ci[1], 3),
                                    CI_upper = round(ci[2], 3),
                                    P值 = round(p_val, 4)))
}

univar_signif <- univar_result %>% filter(P值 < 0.05)
print("单因素筛选出的显著变量：")
print(univar_signif)
univar_signif[,1]


# 提取单因素显著变量名称
signif_vars <- univar_signif$变量
# 构建多因素回归公式
multivar_formula <- as.formula(paste("PV1 ~", paste(signif_vars, collapse = " + ")))
# 拟合多因素逻辑回归模型
multivar_model <- glm(multivar_formula, data = train_data, family = binomial())
# 提取多因素回归结果
multivar_summary <- summary(multivar_model)
multivar_coef <- exp(coef(multivar_model))  # OR值
multivar_p <- multivar_summary$coefficients[-1, 4]  # 排除截距项的P值

# 筛选多因素P<0.05的最终特征（论文最终纳入特征）
final_features <- names(multivar_p[multivar_p < 0.05])
print("多因素验证后最终特征：")
print(final_features)  # 输出





####-----------------t检验筛选变量-----------------------------
library(dplyr)
library(car)
library(ggplot2)
library(broom)
library(purrr)
library(dplyr)
#劲呐
X_data1 <- X_data %>% filter(!(PV >= 22 & PV <= 30))
table(X_data1$PV1)
str(X_data1)


X_data1$PV1 <- factor(X_data$PV1, levels = c("0", "1"), labels = c("对照组", "PV1阳性组"))
# 筛选出所有连续变量（排除因子变量和 PV1 本身）
continuous_vars <- X_data %>%
  select(-c(是否吸烟, 病史, 高血压, PV1 ,总维生素D )) %>%
  colnames()

# 处理缺失值（删除含缺失值的行，或根据需求替换）
clean_data <- X_data %>%
  filter(complete.cases(.))


# ==============================================
# 3. 批量执行 t检验/非参数检验
# ==============================================
# 定义函数：对单个变量执行检验
test_single_var <- function(var_name, data) {
  # 提取变量
  formula <- as.formula(paste(var_name, "~ PV1"))
  
  # 拆分两组数据
  group0 <- subset(data, PV1 == "0")[[var_name]]
  group1 <- subset(data, PV1 == "1")[[var_name]]
  
  # 正态性检验（Shapiro-Wilk）
  shapiro_p0 <- shapiro.test(group0)$p.value
  shapiro_p1 <- shapiro.test(group1)$p.value
  normality_ok <- (shapiro_p0 > 0.05) & (shapiro_p1 > 0.05)
  
  # 方差齐性检验（Levene）
  levene_p <- leveneTest(formula, data = data)$`Pr(>F)`[1]
  var_equal_ok <- (levene_p > 0.05)
  
  # 选择检验方法
  if (normality_ok & var_equal_ok) {
    # 满足假设：独立样本t检验
    test_result <- t.test(formula, data = data, var.equal = TRUE)
    method <- "独立样本t检验"
  } else {
    # 不满足假设：Wilcoxon秩和检验
    test_result <- wilcox.test(formula, data = data)
    method <- "Wilcoxon秩和检验"
  }
  
  # 整理结果
  tibble(
    变量名 = var_name,
    检验方法 = method,
    正态性满足 = normality_ok,
    方差齐性满足 = var_equal_ok,
    p值 = test_result$p.value,
    均值对照组 = mean(group0, na.rm = TRUE),
    均值PV1阳性组 = mean(group1, na.rm = TRUE),
    均值差异 = mean(group1, na.rm = TRUE) - mean(group0, na.rm = TRUE)
  )
}

# 批量执行所有连续变量的检验
test_results <- map_dfr(continuous_vars, ~test_single_var(.x, clean_data))

# ==============================================
# 4. 多重检验校正（FDR）
# ==============================================
test_results <- test_results %>%
  mutate(
    校正后p值 = p.adjust(p值, method = "fdr"),
    显著性 = ifelse(校正后p值 < 0.05, "显著", "不显著")
  )

# 查看所有结果（按校正后p值排序）
test_results <- test_results %>% arrange(校正后p值)
print(test_results, n = Inf)

# ==============================================
# 5. 可视化：显著变量的组间差异箱线图
# ==============================================
# 筛选显著变量
significant_vars <- test_results %>%
  filter(显著性 == "显著") %>%
  pull(变量名)

# 批量绘制箱线图
walk(significant_vars, function(var) {
  p <- ggplot(clean_data, aes(x = PV1, y = .data[[var]], fill = PV1)) +
    geom_boxplot(width = 0.5, alpha = 0.7) +
    labs(
      title = paste(var, "在两组间的对比"),
      x = "分组", y = var,
      fill = "分组"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  print(p)
})


# 加载包
if (!require("glmnet")) install.packages("glmnet")
if (!require("caret")) install.packages("caret")
library(glmnet)
library(caret)

# 准备数据（假设你的目标变量是 PV1，0/1 分类）
X <- clean_data %>% select(fPSA, tPSA, 年龄, 白蛋白, 腰围, 血糖, 甘油三酯, 尿素) %>% as.matrix()
y <- clean_data$PV1

# 弹性网交叉验证（自动平衡L1和L2惩罚）
set.seed(123)
cv_enet <- cv.glmnet(X, y, family = "binomial", alpha = 0.5)  # alpha=0.5 是弹性网，alpha=1是Lasso

# 查看最优lambda对应的系数
coef(cv_enet, s = "lambda.min")

# 查看最优lambda对应的系数（稀疏矩阵转成普通矩阵）
coef_matrix <- as.matrix(coef(cv_enet, s = "lambda.min"))
print(coef_matrix)

# 提取筛选后的变量（排除截距项）
selected_vars <- rownames(coef_matrix)[coef_matrix != 0 & rownames(coef_matrix) != "(Intercept)"]
print("弹性网筛选后的变量：")
print(selected_vars)

###---------将单因素逻辑回归完成筛选的变量组件新数据----------------

# 安装未安装的包

# 安装需要的包
required_packages <- c("dplyr", "caret", "DMwR", "randomForest", "rpart", "kernlab", "Metrics", "pROC")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages) > 0) install.packages(new_packages)
install.packages("smotefamily")
library(smotefamily)
# 加载包
library(dplyr)        # 数据清洗与处理 核心包
library(caret)        # 模型训练+交叉验证+超参数调优 核心包
library(DMwR)         # 处理类别不平衡的SMOTE过采样 核心包
library(randomForest) # 随机森林基模型
library(rpart)        # 决策树基模型
library(kernlab)      # SVM支持向量机基模型（径向核）
library(Metrics)      # 模型评估指标计算（准确率/精确率/召回率/F1）
library(pROC)         # ROC曲线+AUC值计算（医学预测必看指标）




new_vars <- c("BMI",
              "年龄",
              "腰围",
              "总胆固醇",
              "低密度脂蛋白",
              "总蛋白",
              "白蛋白",
              "尿素",
              "尿酸",
              "tPSA",
              "fPSA",
              "病史",
              "高血压",
              "PV1")
#另一个方法筛出来的变量
new_vars <- c("BMI",
              "年龄",
              "腰围",
              "总胆固醇",
              "低密度脂蛋白",
              "总蛋白",
              "白蛋白",
              "尿素",
              "尿酸",
              "tPSA",
              "fPSA",
              "病史",
              "高血压",
              "PV1")
T_data <- X_data[,new_vars]

# 定义特征列和目标列
continuous_cols <- c("BMI", "年龄", "腰围", "总胆固醇", "低密度脂蛋白", 
                     "总蛋白", "白蛋白", "尿素", "尿酸", "tPSA", "fPSA") # 连续型特征
cate_cols       <- c("病史", "高血压") # 二分类特征
target_col      <- "PV1" # 结局变量：0=无增生，1=有增生


# 检查缺失值
cat("各列缺失值数量：\n")
print(colSums(is.na(T_data)))


# 连续型特征：中位数填充
for(col in continuous_cols){
  T_data[[col]] <- ifelse(is.na(T_data[[col]]), median(T_data[[col]], na.rm = T), T_data[[col]])
}

# 分类型特征：众数填充
get_mode <- function(x) {
  tab <- table(x)
  names(tab)[which.max(tab)]
}
for(col in cate_cols){
  T_data[[col]] <- ifelse(is.na(T_data[[col]]), get_mode(T_data[[col]]), T_data[[col]])
  T_data[[col]] <- as.factor(T_data[[col]])
}

# 定义异常值替换函数
replace_outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = T)
  q3 <- quantile(x, 0.75, na.rm = T)
  iqr <- q3 - q1
  lower <- q1 - 1.5*iqr
  upper <- q3 + 1.5*iqr
  x[x < lower | x > upper] <- median(x, na.rm = T)
  return(x)
}

# 仅对连续型特征处理异常值
T_data[continuous_cols] <- lapply(T_data[continuous_cols], replace_outliers)

#画图看看
boxplot(T_data$BMI, horizontal = TRUE, col = "lightgreen",
        main = "前列腺体积箱线图")



# 标准化函数：(值-均值)/标准差
scale_fun <- function(x) {
  (x - mean(x, na.rm = T)) / sd(x, na.rm = T)
}

# 仅对连续型特征标准化
T_data[continuous_cols] <- lapply(T_data[continuous_cols], scale_fun)

# 标准化函数：(值-均值)/标准差
scale_fun <- function(x) {
  (x - mean(x, na.rm = T)) / sd(x, na.rm = T)
}

# 仅对连续型特征标准化
T_data[continuous_cols] <- lapply(T_data[continuous_cols], scale_fun)




T_data <- T_data %>%
  mutate(
    PV1 = factor(PV1, levels = c(0,1)),
    病史 = factor(病史, levels = c(0,1)),
    高血压 = factor(高血压, levels = c(0,1))
  )

str(T_data)
##两个同名数据出现，要用上面的数据要提前运行一次
library(caret)
set.seed(123) # 设置随机种子，结果可复现
train_index <- createDataPartition(T_data[[target_col]], p = 0.7, list = F)
train_data  <- T_data[train_index, ] # 训练集 70%
test_data   <- T_data[-train_index, ] # 测试集 30%

cat("训练集样本数：",nrow(train_data),"\n")
cat("测试集样本数：",nrow(test_data),"\n")
cat("训练集正负样本分布：\n")
print(table(train_data[[target_col]]))

str(train_data)
train_data[[target_col]] <- as.factor(train_data[[target_col]])

set.seed(123)
set.seed(123)
install.packages("DMwR")
install.packages("smotefamily")
library(smotefamily)
library(smotefamily)
library(dplyr)



smote_train <- upSample(x = train_data %>% select(-all_of(target_col)),
                        y = train_data[[target_col]],
                        yname = target_col)

cat("平衡后训练集分布：\n")
print(table(smote_train[[target_col]])) 



str(train_data)
train_data$PV1 <- as.factor(train_data$PV1)
smote_train <- train_data

a
######有待考究-----------------------------
library(rpart)

dt_model <- rpart(
  PV1 ~ ., 
  data = smote_train, 
  method = "class"
)




library(randomForest)
rf_model <- randomForest(
  PV1 ~ ., 
  data = smote_train,
  ntree = 500
)


library(e1071)
svm_model <- svm(
  PV1 ~ ., 
  data = smote_train,
  probability = TRUE
)




dt_pred  <- predict(dt_model, test_data, type = "prob")[,2]
rf_pred  <- predict(rf_model, test_data, type = "prob")[,2]
svm_pred <- attr(
  predict(svm_model, test_data, probability = TRUE),
  "probabilities"
)[,2]

stack_data <- data.frame(
  dt = dt_pred,
  rf = rf_pred,
  svm = svm_pred,
  y = test_data$PV1
)



meta_model <- glm(
  y ~ ., 
  data = stack_data,
  family = binomial
)

stack_prob <- predict(meta_model, type = "response")



library(pROC)
roc_stack <- roc(stack_data$y, stack_prob)
auc(roc_stack)

pred_class <- ifelse(stack_prob > 0.5, 1, 0)

confusionMatrix(
  factor(pred_class),
  factor(stack_data$y)
)



ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

















#数据存案
save(smote_train, file = "D:/A学习资料/大四下/毕设/数据/smote_train(过采样平衡测试集)0121.RData")
save(test_data, file = "D:/A学习资料/大四下/毕设/数据/test_data0121.RData")


save(smote_train, file = "D:/A学习资料/大四下/毕设/数据/smote_train(非平衡测试集)调整PV0121.RData")


###------------------尝试复现原参考文献---------------------------
library(caret)
library(rpart)
library(randomForest)
library(e1071)
library(pROC)
library(ggplot2)

set.seed(2024)
folds <- createFolds(smote_train$PV1, k = 5)
#初始化预测矩阵
oof_pred <- data.frame(
  dt  = rep(NA, nrow(smote_train)),
  rf  = rep(NA, nrow(smote_train)),
  svm = rep(NA, nrow(smote_train))
)

#逐步训练
for (i in 1:5) {
  
  cat("Processing fold:", i, "\n")
  
  valid_idx <- folds[[i]]
  train_fold <- smote_train[-valid_idx, ]
  valid_fold <- smote_train[ valid_idx, ]
  
  ## ---- DT ----
  dt_model <- rpart(
    PV1 ~ ., 
    data = train_fold, 
    method = "class"
  )
  oof_pred$dt[valid_idx] <- predict(
    dt_model, valid_fold, type = "prob"
  )[,2]
  
  ## ---- RF ----
  rf_model <- randomForest(
    PV1 ~ ., 
    data = train_fold,
    ntree = 500
  )
  oof_pred$rf[valid_idx] <- predict(
    rf_model, valid_fold, type = "prob"
  )[,2]
  
  ## ---- SVM ----
  svm_model <- svm(
    PV1 ~ ., 
    data = train_fold,
    probability = TRUE
  )
  oof_pred$svm[valid_idx] <- attr(
    predict(svm_model, valid_fold, probability = TRUE),
    "probabilities"
  )[,2]
}


###构建meta_learner
meta_train <- data.frame(
  oof_pred,
  y = smote_train$PV1
)

meta_model <- glm(
  y ~ .,
  data = meta_train,
  family = binomial
)


dt_full  <- rpart(PV1 ~ ., smote_train, method = "class")
rf_full  <- randomForest(PV1 ~ ., smote_train, ntree = 500)
svm_full <- svm(PV1 ~ ., smote_train, probability = TRUE)




#构造stacking
test_stack <- data.frame(
  dt = predict(dt_full, test_data, type = "prob")[,2],
  rf = predict(rf_full, test_data, type = "prob")[,2],
  svm = attr(
    predict(svm_full, test_data, probability = TRUE),
    "probabilities"
  )[,2]
)



stack_prob <- predict(
  meta_model,
  newdata = test_stack,
  type = "response"
)

stack_class <- ifelse(stack_prob > 0.5, 1, 0)


#汇总
roc_dt <- roc(test_data$PV1, test_stack$dt)
roc_rf <- roc(test_data$PV1, test_stack$rf)
roc_svm <- roc(test_data$PV1, test_stack$svm)
roc_stack <- roc(test_data$PV1, stack_prob)



auc_df <- data.frame(
  Model = c("DT","RF","SVM","Stacking"),
  AUC = c(
    auc(roc_dt),
    auc(roc_rf),
    auc(roc_svm),
    auc(roc_stack)
  )
)

auc_df <- data.frame(
  Model = c("DT","RF","SVM","Stacking"),
  AUC = c(
    pROC::auc(roc_dt),  # 明确用 pROC 包的 auc
    pROC::auc(roc_rf),
    pROC::auc(roc_svm),
    pROC::auc(roc_stack)
  )
)

auc_df

ggroc(
  list(
    DT = roc_dt,
    RF = roc_rf,
    SVM = roc_svm,
    Stacking = roc_stack
  )
) +
  theme_minimal() +
  labs(
    title = "ROC Curves of Different Models",
    x = "False Positive Rate",
    y = "True Positive Rate"
  )




#------------加模型-------------------------------------------------------------------------

library(caret)
library(rpart)
library(randomForest)
library(e1071)
library(nnet)
library(xgboost)
library(pROC)
library(ggplot2)
library(rms)
library(rmda)

set.seed(2024)
folds <- createFolds(smote_train$PV1, k = 5)

smote_train$PV1 <- factor(smote_train$PV1, levels = c(0,1))
test_data$PV1   <- factor(test_data$PV1, levels = c(0,1))


oof_pred <- data.frame(
  dt  = rep(NA, nrow(smote_train)),
  rf  = rep(NA, nrow(smote_train)),
  svm = rep(NA, nrow(smote_train)),
  nn  = rep(NA, nrow(smote_train))
)

for (i in 1:5) {
  
  cat("Processing fold:", i, "\n")
  
  valid_idx <- folds[[i]]
  train_fold <- smote_train[-valid_idx, ]
  valid_fold <- smote_train[ valid_idx, ]
  
  ## ---- DT ----
  dt_model <- rpart(PV1 ~ ., data = train_fold, method = "class")
  oof_pred$dt[valid_idx] <- predict(dt_model, valid_fold, type = "prob")[,2]
  
  ## ---- RF ----
  rf_model <- randomForest(PV1 ~ ., data = train_fold, ntree = 500)
  oof_pred$rf[valid_idx] <- predict(rf_model, valid_fold, type = "prob")[,2]
  
  ## ---- SVM ----
  svm_model <- svm(PV1 ~ ., data = train_fold, probability = TRUE)
  oof_pred$svm[valid_idx] <- attr(
    predict(svm_model, valid_fold, probability = TRUE),
    "probabilities"
  )[,2]
  
  ## ---- NN ----
  nn_model <- nnet(
    PV1 ~ ., data = train_fold,
    size = 5, decay = 0.01,
    maxit = 500, trace = FALSE
  )
  oof_pred$nn[valid_idx] <- predict(nn_model, valid_fold, type = "raw")
}


##-----------元学习器
meta_train <- data.frame(
  oof_pred,
  y = smote_train$PV1
)

meta_model <- glm(
  y ~ .,
  data = meta_train,
  family = binomial
)

##------------
dt_full  <- rpart(PV1 ~ ., smote_train, method = "class")
rf_full  <- randomForest(PV1 ~ ., smote_train, ntree = 500)
svm_full <- svm(PV1 ~ ., smote_train, probability = TRUE)

nn_full <- nnet(
  PV1 ~ ., smote_train,
  size = 5, decay = 0.01,
  maxit = 500, trace = FALSE
)


test_stack <- data.frame(
  dt = predict(dt_full, test_data, type = "prob")[,2],
  rf = predict(rf_full, test_data, type = "prob")[,2],
  svm = attr(
    predict(svm_full, test_data, probability = TRUE),
    "probabilities"
  )[,2],
  nn = predict(nn_full, test_data, type = "raw")
)



stack_prob <- predict(
  meta_model,
  newdata = test_stack,
  type = "response"
)

stack_class <- ifelse(stack_prob > 0.5, 1, 0)


roc_dt  <- roc(test_data$PV1, test_stack$dt)
roc_rf  <- roc(test_data$PV1, test_stack$rf)
roc_svm <- roc(test_data$PV1, test_stack$svm)
roc_nn  <- roc(test_data$PV1, test_stack$nn)
roc_stack <- roc(test_data$PV1, stack_prob)


auc_df <- data.frame(
  Model = c("DT","RF","SVM","NN","Stacking"),
  AUC = c(
    auc(roc_dt),
    auc(roc_rf),
    auc(roc_svm),
    auc(roc_nn),
    auc(roc_stack)
  )
)

auc_df

###------后续的图-----------------------


ggroc(
  list(
    DT = roc_dt,
    RF = roc_rf,
    SVM = roc_svm,
    NN = roc_nn,
    Stacking = roc_stack
  )
) +
  theme_minimal() +
  labs(
    title = "ROC Curves of Different Models",
    x = "False Positive Rate",
    y = "True Positive Rate"
  ) +
  scale_color_brewer(palette = "Set1")




ggroc(
  list(
    DT = roc_dt,
    RF = roc_rf,
    SVM = roc_svm,
    NN = roc_nn,
    Stacking = roc_stack
  )
) +
  theme_minimal() +
  labs(title = "ROC Comparison", x = "FPR", y = "TPR")

ggplot(auc_df, aes(x = Model, y = AUC, fill = Model)) +
  geom_bar(stat = "identity", width = 0.6) +
  theme_minimal() +
  ylim(0.5,1)

varImpPlot(rf_full)

dd <- datadist(test_data)
options(datadist = "dd")

cal_model <- lrm(PV1 ~ stack_prob, data = test_data, x=TRUE, y=TRUE)
cal <- calibrate(cal_model, method = "boot", B = 1000)

plot(cal)


##----各个结果----


# ==========================================
# 1. 提取最原始的真实标签，并转为字符型再转因子
# 这样可以避免任何隐藏的 factor levels 冲突
# ==========================================
truth <- as.factor(as.character(test_data$PV1))

# 获取真实标签的类别（看看到底是 "0"和"1"，还是 "No"和"Yes"）
lvls <- levels(truth) 

# 打印出来检查一下，确保这里不再是 NA！
cat("真实标签的类别是:", lvls, "\n")
print(table(truth, useNA = "ifany"))

# 设定正类（通常是 levels 的第二个，比如 "1"）
positive_class <- lvls[2] 

# ==========================================
# 2. 将概率严格转换为与 truth 完全一致的因子
# ==========================================
dt_class    <- factor(ifelse(test_stack$dt > 0.5, lvls[2], lvls[1]), levels = lvls)
rf_class    <- factor(ifelse(test_stack$rf > 0.5, lvls[2], lvls[1]), levels = lvls)
svm_class   <- factor(ifelse(test_stack$svm > 0.5, lvls[2], lvls[1]), levels = lvls)
nn_class    <- factor(ifelse(test_stack$nn > 0.5, lvls[2], lvls[1]), levels = lvls)
stack_class <- factor(ifelse(stack_prob > 0.5, lvls[2], lvls[1]), levels = lvls)

# ==========================================
# 3. 定义评估函数 (基于 caret 包，非常严谨)
# ==========================================
library(caret)
get_metrics <- function(pred_class, actual_class, model_name) {
  # 计算混淆矩阵，明确指定阳性类别
  cm <- confusionMatrix(pred_class, actual_class, positive = positive_class)
  
  # 提取各项指标
  acc  <- cm$overall["Accuracy"]
  sens <- cm$byClass["Sensitivity"]
  spec <- cm$byClass["Specificity"]
  prec <- cm$byClass["Pos Pred Value"] 
  f1   <- cm$byClass["F1"]
  
  # 组合为 Dataframe
  return(data.frame(
    Model       = model_name,
    Accuracy    = round(acc, 4),
    Sensitivity = round(sens, 4),
    Specificity = round(spec, 4),
    Precision   = round(prec, 4),
    F1          = round(f1, 4)
  ))
}

# ==========================================
# 4. 汇总所有模型的指标
# ==========================================
metrics_df <- rbind(
  get_metrics(dt_class, truth, "DT"),
  get_metrics(rf_class, truth, "RF"),
  get_metrics(svm_class, truth, "SVM"),
  get_metrics(nn_class, truth, "NN"),
  get_metrics(stack_class, truth, "Stacking")
)

# 去除多余的行名并打印最终结果
rownames(metrics_df) <- NULL
print(metrics_df)




metrics_df



metrics_df





#####条图---------------------------------
ggplot(auc_df, aes(x = Model, y = AUC, fill = Model)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = round(AUC, 2)), vjust = -0.3, size = 3.5) + # 添加数值标签
  theme_minimal() +
  labs(
    title = "AUC Comparison of Models",
    y = "AUC"
  ) +
  ylim(0.5, 1)
ggplot(auc_df, aes(x = Model, y = AUC, fill = Model)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  geom_text(aes(label = round(AUC, 2)), vjust = -0.3, size = 3.5) +
  theme_minimal() +
  labs(title = "AUC Comparison of Models", y = "AUC")



# 按AUC值从高到低排序
auc_df <- auc_df[order(-auc_df$AUC), ]

# 绘制优化后的图表
ggplot(auc_df, aes(x = Model, y = AUC, fill = Model)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  geom_text(aes(label = round(AUC, 2)), vjust = -0.3, size = 3.5) +
  theme_minimal() +
  labs(
    title = "AUC Comparison of Models",
    y = "AUC Value",
    x = "Model Name"
  ) +
  scale_fill_brewer(palette = "Set2") + # 使用更专业的配色
  ylim(0, 1.1) # 从0开始显示y轴，更直观





#######修正？------------------------------------------


install.packages("MLmetrics")
library(caret)
library(rpart)
library(randomForest)
library(e1071)
library(pROC)
library(ggplot2)
library(MLmetrics) # 计算Prec/Rec/F1的核心包


smote_train$PV1 <- factor(smote_train$PV1, levels = c(0,1)) # 固定标签水平：0=阴性，1=阳性
test_data$PV1 <- factor(test_data$PV1, levels = c(0,1))

# ===================== 原代码的修正+补注释=====================
set.seed(2024)
folds <- createFolds(smote_train$PV1, k = 5) # 分层5折，完美匹配论文

#初始化预测矩阵：存储5折Oof预测概率
oof_pred <- data.frame(
  dt  = rep(NA, nrow(smote_train)),
  rf  = rep(NA, nrow(smote_train)),
  svm = rep(NA, nrow(smote_train))
)

#逐步训练：5折交叉验证+Oof预测（核心堆叠步骤）
for (i in 1:5) {
  cat("Processing fold:", i, "\n")
  valid_idx <- folds[[i]]
  train_fold <- smote_train[-valid_idx, ]
  valid_fold <- smote_train[ valid_idx, ]
  
  ## ---- DT 决策树 ----
  dt_model <- rpart(PV1 ~ ., data = train_fold, method = "class")
  oof_pred$dt[valid_idx] <- predict(dt_model, valid_fold, type = "prob")[,2] # 1的概率
  
  ## ---- RF 随机森林 ----
  rf_model <- randomForest(PV1 ~ ., data = train_fold, ntree = 500) # 论文指定ntree=500
  oof_pred$rf[valid_idx] <- predict(rf_model, valid_fold, type = "prob")[,2]
  
  ## ---- SVM 支持向量机----
  svm_model <- svm(PV1 ~ ., data = train_fold, probability = TRUE)
  svm_prob_mat <- attr(predict(svm_model, valid_fold, probability = TRUE), "probabilities")
  oof_pred$svm[valid_idx] <- svm_prob_mat[, "1"] # 直接取标签=1的概率，无视列顺序
}

###构建meta_learner 逻辑回归元学习器
meta_train <- data.frame(
  oof_pred,
  y = smote_train$PV1 # 真实标签
)
meta_model <- glm(y ~ ., data = meta_train, family = binomial) # 二分类逻辑回归

# 全量训练基模型（修正：加随机种子，保证结果可复现，论文要求）
set.seed(2024)
dt_full  <- rpart(PV1 ~ ., smote_train, method = "class")
rf_full  <- randomForest(PV1 ~ ., smote_train, ntree = 500)
svm_full <- svm(PV1 ~ ., smote_train, probability = TRUE)

#构造测试集的堆叠特征矩阵
test_stack <- data.frame(
  dt = predict(dt_full, test_data, type = "prob")[,2],
  rf = predict(rf_full, test_data, type = "prob")[,2],
  svm = attr(predict(svm_full, test_data, probability = TRUE), "probabilities")[, "1"] # 同步修正
)

# 堆叠模型最终预测
stack_prob <- predict(meta_model, newdata = test_stack, type = "response")
stack_class <- ifelse(stack_prob > 0.5, 1, 0) # 概率阈值0.5，论文标准

# ===================== 补全：论文核心4指标+AUC=====================
# 提取真实标签（转为数值型0/1，方便计算）
y_true <- as.integer(as.character(test_data$PV1)) 

# 计算单一模型的分类结果（用于对比）
dt_class  <- ifelse(test_stack$dt>0.5,1,0)
rf_class  <- ifelse(test_stack$rf>0.5,1,0)
svm_class <- ifelse(test_stack$svm>0.5,1,0)

# 构建【论文标准结果表】：Accuracy/Precision/Recall/F1/AUC 全部包含
model_metrics <- data.frame(
  Model = c("DT","RF","SVM","Stacking"),
  Accuracy = round(c(Accuracy(dt_class,y_true),Accuracy(rf_class,y_true),Accuracy(svm_class,y_true),Accuracy(stack_class,y_true))*100,2),
  Precision = round(c(Precision(dt_class,y_true),Precision(rf_class,y_true),Precision(svm_class,y_true),Precision(stack_class,y_true))*100,2),
  Recall = round(c(Recall(dt_class,y_true),Recall(rf_class,y_true),Recall(svm_class,y_true),Recall(stack_class,y_true))*100,2),
  F1_Score = round(c(F1_Score(dt_class,y_true),F1_Score(rf_class,y_true),F1_Score(svm_class,y_true),F1_Score(stack_class,y_true))*100,2),
  AUC = round(c(pROC::auc(pROC::roc(y_true, test_stack$dt)),
                pROC::auc(pROC::roc(y_true, test_stack$rf)),
                pROC::auc(pROC::roc(y_true, test_stack$svm)),
                pROC::auc(pROC::roc(y_true, stack_prob))),4)
)

# 输出结果表
print(model_metrics)

# ===================== 你的ROC曲线绘图代码（无修改）=====================
roc_dt <- roc(y_true, test_stack$dt)
roc_rf <- roc(y_true, test_stack$rf)
roc_svm <- roc(y_true, test_stack$svm)
roc_stack <- roc(y_true, stack_prob)

ggroc(
  list(DT = roc_dt,RF = roc_rf,SVM = roc_svm,Stacking = roc_stack)
) + theme_minimal() +
  labs(title = "ROC Curves of Different Models",x = "False Positive Rate",y = "True Positive Rate")















#-----加模型

# 安装缺失包
required_new_packages <- c("xgboost", "nnet", "rmda", "gridExtra", "corrplot", "ggplot2", "pROC", "caret")
new_packages <- required_new_packages[!(required_new_packages %in% installed.packages()[,"Package"])]
if(length(new_packages) > 0) install.packages(new_packages)

# 加载所有包（含现有代码已加载的包）
library(tidyverse)
library(caret)
library(randomForest)
library(rpart)
library(e1071)
library(pROC)
library(MLmetrics)
library(xgboost)       # 新增XGBoost模型
library(nnet)          # 新增神经网络（MLP）模型
library(rmda)          # 决策曲线分析（DCA）
library(gridExtra)     # 多图组合可视化
library(corrplot)      # 特征相关性辅助分析
library(rms)           # 校准曲线

# 提取特征列和目标列（与现有代码一致）
continuous_cols <- c("BMI", "年龄", "腰围", "总胆固醇", "低密度脂蛋白", 
                     "总蛋白", "白蛋白", "尿素", "尿酸", "tPSA", "fPSA")
cate_cols       <- c("病史", "高血压")
target_col      <- "PV1"

# 构建建模用特征矩阵（含连续型+分类型变量）
# 分类型变量转为哑变量，连续型已标准化（现有代码已处理）
train_x <- model.matrix(~ . -1, data = smote_train[, c(continuous_cols, cate_cols)])
test_x  <- model.matrix(~ . -1, data = test_data[, c(continuous_cols, cate_cols)])
train_y <- as.integer(as.character(smote_train[[target_col]]))  # 0/1数值型
test_y  <- as.integer(as.character(test_data[[target_col]]))

# ===================== 1. 加载修正后的依赖包 =====================
# 安装缺失包
required_new_packages <- c("xgboost", "nnet", "rmda", "gridExtra", "ggplot2", "pROC", "caret","rms","dplyr","tidyr")
new_packages <- required_new_packages[!(required_new_packages %in% installed.packages()[,"Package"])]
if(length(new_packages) > 0) install.packages(new_packages, dependencies = TRUE)

# 加载包
library(tidyverse)
library(caret)
library(randomForest)
library(rpart)
library(e1071)
library(pROC)
library(MLmetrics)
library(xgboost)       
library(nnet)          
library(rmda)          
library(gridExtra)     
library(rms)           
library(dplyr)
library(tidyr)

# 固定随机种子（保证结果可复现）
set.seed(123)

# ===================== 2. 数据准备（完全沿用你的变量名） =====================
continuous_cols <- c("BMI", "年龄", "腰围", "总胆固醇", "低密度脂蛋白", 
                     "总蛋白", "白蛋白", "尿素", "尿酸", "tPSA", "fPSA")
cate_cols       <- c("病史", "高血压")
target_col      <- "PV1"

# 处理特征矩阵（修复因子/哑变量报错）
train_data <- smote_train[, c(continuous_cols, cate_cols, target_col)]
test_data <- test_data[, c(continuous_cols, cate_cols, target_col)]

# 目标变量转为因子（caret强制要求）
train_data[[target_col]] <- factor(train_data[[target_col]], levels = c(0,1), labels = c("No","Yes"))
test_data[[target_col]] <- factor(test_data[[target_col]], levels = c(0,1), labels = c("No","Yes"))

# 特征/标签分离
train_x <- model.matrix(~ . -1, data = train_data[, c(continuous_cols, cate_cols)])
test_x  <- model.matrix(~ . -1, data = test_data[, c(continuous_cols, cate_cols)])
train_y <- train_data[[target_col]]
test_y  <- test_data[[target_col]]
test_y_num <- as.integer(test_y) - 1  # 转为0/1数值（用于ROC/校准）

# ===================== 3. 新增模型训练（XGBoost+MLP，变量名不变） =====================
# 3.1 XGBoost
xgb_grid <- expand.grid(nrounds = c(100,200), max_depth = c(3,5), eta = c(0.1,0.3),
                        gamma = 0, colsample_bytree = 0.8, subsample = 0.8)
# 修正后的XGBoost参数网格（补全min_child_weight，匹配caret要求）
xgb_grid <- expand.grid(
  nrounds = c(100, 200),
  max_depth = c(3, 5),
  eta = c(0.1, 0.3),
  gamma = 0,
  colsample_bytree = 0.8,
  min_child_weight = 1,  # 新增：caret的xgbTree必须包含此参数
  subsample = 0.8
)
xgb_cv <- train(x = train_x, y = train_y, method = "xgbTree",
                trControl = trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = twoClassSummary),
                tuneGrid = xgb_grid, metric = "ROC", verbose = FALSE)

xgb_pred_prob <- predict(xgb_cv, newdata = test_x, type = "prob")[, "Yes"]
xgb_pred_class <- predict(xgb_cv, newdata = test_x)

# 3.2 神经网络MLP
mlp_grid <- expand.grid(size = c(5,10), decay = c(0.01,0.1))
mlp_cv <- train(x = train_x, y = train_y, method = "nnet",
                trControl = trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = twoClassSummary),
                tuneGrid = mlp_grid, metric = "ROC", maxit = 1000, trace = FALSE)
mlp_pred_prob <- predict(mlp_cv, newdata = test_x, type = "prob")[, "Yes"]
mlp_pred_class <- predict(mlp_cv, newdata = test_x)

# ===================== 4. 整合所有模型性能（变量名不变） =====================
# 假设你原有模型：DT/RF/SVM/Stacking，对应变量：dt_pred_prob, rf_pred_prob, svm_pred_prob, stack_prob
# 原有分类结果：dt_pred_class, rf_pred_class, svm_pred_class, stack_class

# 计算所有模型指标
get_metrics <- function(prob, pred, y_true){
  acc <- Accuracy(pred, y_true)
  prec <- Precision(pred, y_true, positive = "Yes")
  rec <- Recall(pred, y_true, positive = "Yes")
  f1 <- F1_Score(pred, y_true, positive = "Yes")
  auc <- auc(roc(as.integer(y_true)-1, prob, quiet=TRUE))
  return(data.frame(Accuracy=round(acc*100,2), Precision=round(prec*100,2),
                    Recall=round(rec*100,2), F1_Score=round(f1*100,2), AUC=round(auc,4)))
}

# 原有模型 + 新模型指标（替换为你真实的预测变量名）
model_metrics_full <- rbind(
  # 你原有模型（取消注释即可）
  # get_metrics(dt_pred_prob, dt_pred_class, test_y) %>% mutate(Model="决策树"),
  # get_metrics(rf_pred_prob, rf_pred_class, test_y) %>% mutate(Model="随机森林"),
  # get_metrics(svm_pred_prob, svm_pred_class, test_y) %>% mutate(Model="SVM"),
  # get_metrics(stack_prob, stack_class, test_y) %>% mutate(Model="Stacking"),
  get_metrics(xgb_pred_prob, xgb_pred_class, test_y) %>% mutate(Model="XGBoost"),
  get_metrics(mlp_pred_prob, mlp_pred_class, test_y) %>% mutate(Model="MLP")
) %>% select(Model, everything()) %>% arrange(desc(AUC))

print("所有模型性能对比：")
print(model_metrics_full)

# ===================== 5. 性能可视化（修复所有绘图报错） =====================
# 5.1 AUC柱状图
ggplot(model_metrics_full, aes(x = reorder(Model, -AUC), y = AUC, fill = Model)) +
  geom_bar(stat = "identity", width=0.7, color="black", alpha=0.8) +
  geom_text(aes(label=round(AUC,3)), vjust=-0.3, size=4, fontface="bold") +
  scale_fill_brewer(palette="Set2") +
  labs(title="各模型AUC值对比", x="模型", y="AUC值") +
  theme_minimal() + theme(plot.title=element_text(hjust=0.5), axis.text.x=element_text(angle=30,hjust=1))
ggsave("AUC对比图.png", width=10, height=6, dpi=300)

# 5.2 多模型ROC曲线
roc_list <- list(
  # 原有模型（取消注释）
  # "决策树" = roc(test_y_num, dt_pred_prob, quiet=TRUE),
  # "随机森林" = roc(test_y_num, rf_pred_prob, quiet=TRUE),
  # "SVM" = roc(test_y_num, svm_pred_prob, quiet=TRUE),
  # "Stacking" = roc(test_y_num, stack_prob, quiet=TRUE),
  "XGBoost" = roc(test_y_num, xgb_pred_prob, quiet=TRUE),
  "MLP" = roc(test_y_num, mlp_pred_prob, quiet=TRUE)
)
ggroc(roc_list, aes(color=name, linetype=name)) +
  geom_abline(slope=1, intercept=0, linetype="dashed", color="gray50") +
  labs(title="多模型ROC曲线对比", x="1-特异度", y="灵敏度") +
  theme_minimal() + theme(plot.title=element_text(hjust=0.5), legend.position="bottom")
ggsave("ROC曲线对比.png", width=10, height=7, dpi=300)

# ===================== 6. 特征重要性（修复报错，兼容你的模型） =====================
# 随机森林特征重要性（直接用你训练好的rf_cv）
rf_importance <- varImp(train(getModelInfo("rf")[[1]], x=train_x, y=train_y), scale=TRUE)
rf_importance_df <- as.data.frame(rf_importance$importance) %>% rownames_to_column("特征") %>%
  arrange(desc(Overall)) %>% head(10)

ggplot(rf_importance_df, aes(x=reorder(特征,Overall), y=Overall, fill=特征)) +
  geom_bar(stat="identity", alpha=0.8) + coord_flip() +
  labs(title="随机森林Top10特征重要性", x="特征", y="重要性得分") +
  theme_minimal() + theme(plot.title=element_text(hjust=0.5), legend.position="none")
ggsave("RF特征重要性.png", width=10, height=6, dpi=300)

# XGBoost特征重要性
xgb_imp <- xgb.importance(feature_names=colnames(train_x), model=xgb_cv$finalModel)
xgb.ggplot.importance(xgb_imp, top_n=10, measure="Gain") +
  labs(title="XGBoost特征重要性") + theme_minimal()
ggsave("XGBoost特征重要性.png", width=10, height=6, dpi=300)

# ===================== 7. 校准曲线（核心修复！rms包正确用法） =====================
# 绘制最优模型校准曲线（以XGBoost为例，可替换为Stacking）
calib_data <- data.frame(y = test_y_num, prob = xgb_pred_prob)
lrm_model <- lrm(y ~ prob, data = calib_data, x=TRUE, y=TRUE)
calib <- calibrate(lrm_model, method="boot", B=100)

plot(calib, main="XGBoost模型校准曲线", xlab="预测概率", ylab="实际发生率")
abline(0,1,col="red",lty=2)
dev.copy(png, "校准曲线.png", width=10, height=6, res=300)
dev.off()

# ===================== 8. 决策曲线分析DCA（修复中文变量报错） =====================
dca_df <- data.frame(
  y = test_y_num,
  stacking = ifelse(exists("stack_prob"), stack_prob, 0),
  xgboost = xgb_pred_prob,
  rf = if(exists("rf_pred_prob")) rf_pred_prob else 0
)

dca_result <- decision_curve(
  y ~ stacking + xgboost + rf,
  data = dca_df,
  thresholds = seq(0,1,0.01),
  bootstraps = 100,
  confidence.intervals = 0.95
)

plot_decision_curve(dca_result, curve.names = c("Stacking","XGBoost","随机森林"),
                    col=c("blue","red","green"), lwd=2, legend.position="bottomright",
                    main="模型决策曲线分析(DCA)", xlab="风险阈值", ylab="净收益")
dev.copy(png, "DCA曲线.png", width=10, height=7, res=300)
dev.off()

# ===================== 9. 结果保存（论文可用） =====================
write.csv(model_metrics_full, "模型性能指标.csv", row.names=FALSE, fileEncoding="UTF-8")
saveRDS(xgb_cv, "最优模型_XGBoost.rds")
