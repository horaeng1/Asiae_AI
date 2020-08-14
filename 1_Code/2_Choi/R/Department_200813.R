library(plyr)
library(dplyr)
library(MASS)
library(ggplot2)
library(corrplot)

main_data <- read.csv("X_train.csv",
                      header = TRUE, sep = ',',
                      stringsAsFactors = FALSE,
                      strip.white = TRUE,
                      na.strings = c(',', '?', "NA")
)


ctg_names <- c('str_nm', 'corner_nm', 'part_nm', 'buyer_nm', 'team_nm')
ctg <- main_data[ctg_names]
ctg
ctg_main <- subset(ctg, str_nm == "본점")
ctg_muyuk <- subset(ctg, str_nm == "무역점")
ctg_shinchon <- subset(ctg, str_nm == "신촌점")
ctg_chunho <- subset(ctg, str_nm == "천호점")

ctg_main # 본점: 299735, 무역점: 284226, 무역점: 249603, 천호점: 203089

ctg_team_food <- subset(ctg, team_nm == "식품팀")
ctg_team_fashion <- subset(ctg, team_nm == "의류패션팀")
ctg_team_ect <- subset(ctg, team_nm == "잡화가용팀")


team_table <- table(ctg$team_nm, ctg$str_nm)
part_table <- table(ctg$part_nm, ctg$str_nm)

team_table <- prop.table(team_table, 2)
part_table <- prop.table(part_table, 2)
par(mfrow=c(1,2))
barplot(team_table,
        main = "지점별 팀별 총 매출",
        xlab = "지점별",
        ylab = "구매갯수",
        las = 1,
        col = rainbow(12),
        legend = names(ctg$team_nm))

barplot(part_table,
        main = "지점별 파트 총 매출",
        xlab = "지점별",
        ylab = "구매갯수",
        las = 1,
        col = rainbow(12),
        legend = names(ctg$part_nm))
