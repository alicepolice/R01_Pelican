stores <- read.table("./data/PelicanStores.csv",
  header = TRUE, row.names = "Customer", sep = ","
)

res1 <- data.frame(stores)
library(dplyr)
res <- res1 %>% select_if(~ !any(is.na(.)))

print(summary(res))

View(res)

# ---------------------------------------------------------------------------- #

# ^ 百分数频数分布
# @ 客户类型
typeTable1 <- table(res$Type.of.Customer)
typeTable1 <- prop.table(typeTable1) * 100
print(typeTable1)
# @ 支付方法
typeTable2 <- table(res$Method.of.Payment)
typeTable2 <- prop.table(typeTable2) * 100
print(typeTable2)
# @ 销售额频率分组
typeTable3 <- within(res, {
  group1 <- NA
  group1[Net.Sales >= 13 & Net.Sales < 68] <- "13.0~67.9"
  group1[Net.Sales >= 68 & Net.Sales < 123] <- "68.0~122.9"
  group1[Net.Sales >= 123 & Net.Sales < 178] <- "123~177.9"
  group1[Net.Sales >= 178 & Net.Sales < 233] <- "178~222.9"
  group1[Net.Sales >= 233 & Net.Sales < 288] <- "223~287.9"
})
# print(head(sales))
typeTable4 <- table(typeTable3$group1)
typeTable4 <- prop.table(typeTable4) * 100
print(typeTable4)

# ---------------------------------------------------------------------------- #

# ^ 支付方式条形图
png(file = "typeTable2_barplot.png")
par(mar = c(10, 4, 4, 0))
barplot(typeTable2,
  main = "100个顾客付款方法数量条形图",
  xlab = "", ylab = "频数", las = 2
)
dev.off()
# ^ 支付方式圆饼图
png(file = "typeTable2_pie.png")
colors <- c("#4286f4", "#bb3af2", "#ed2f52", "#efc023", "#ea7441")
pie(typeTable2,
  main = "100个顾客付款方法数量圆饼图",
  col = colors, init.angle = 180, clockwise = TRUE
)
dev.off()

# ---------------------------------------------------------------------------- #

# ^ 顾客类型与净销售额的交叉分组表
crossTable <- with(typeTable3, table(Type.of.Customer, group1))
View(crossTable)
# @ 每个顾客类型的行百分比
crossTable <- round(prop.table(crossTable, 1) * 100, 2)
crossTable <- cbind(crossTable, sum = rowSums(crossTable[, 1:5]))
View(crossTable)

# ---------------------------------------------------------------------------- #

# ^净销售额与顾客年龄关系的散点图

png(file = "res_scatterplot.png")

plot(
  x = res$Net.Sales, y = res$Age,
  xlab = "净销售额",
  ylab = "年龄",
  xlim = c(10, 300),
  ylim = c(20, 80),
  main = "净销售额与顾客年龄关系的散点图"
)

dev.off()
