#LOAD DATA 

achievements = read.csv("D:/Binus/ExtraClass (2)/extraClass/achievements.csv",
                        fileEncoding = "UTF-8-BOM",na.strings = c("","N/A"))
achievements_category = read.csv("D:/Binus/ExtraClass (2)/extraClass/achievements-category.csv",
                        fileEncoding = "UTF-8-BOM",na.strings = c("","N/A"))

merged <- merge(achievements,achievements_category, by ="Unique.Entry.ID")

category_counts <- table(merged$Category)
top5_category <- head(sort(category_counts, decreasing = TRUE),5)

# Visualization
pie(
  top5_category,
  col = rainbow(5),
  labels = paste(names(top5_category),top5_category),
  main = "Top 5 Achievement Categories"
)

sequential_counts <- table(merged$Sequential)

barplot(
  sequential_counts,
  ylab = "Frequency",
  col = rainbow(2),
  main = "Frequency of Sequential Achievement"
)

rewardTier1 <- merged$Reward.Tier.1
Large <- sum(rewardTier1 > 820)
Normal <- sum(rewardTier1 > 480 & rewardTier1 <= 820)
Small <- sum(rewardTier1 <= 480 )

barplot(
  c(Large,Normal,Small),
  col = rainbow(3),
  names.arg = c("Large","Normal","Small"),
  xlab = "Reward Categories",
  ylab = "Frequency",
  main = "Total Number of Achievement based on Reward Tier"
)

#association rule analysis using the Apriori algorithm :
new_achievements<- merged[
  merged$Category != 'Money' &
  merged$Category != 'Communication' &
  merged$Category != 'LandMaking' &
  merged$Sequential != 'No' &
  merged$Num.of.Tiers != '6' ,
]

splitted_data <- split(new_achievements$Category,new_achievements$Num.of.Tiers)
library(arules)
transactions <- as(splitted_data,"transactions")

frequent_achievement <- apriori(transactions,
                 parameter = list(
                   support = 0.6,
                   target = "frequent itemsets"
                 )) 
inspect(frequent_achievement)

frequent_achievement <- ruleInduction(frequent_achievement,
                                      confidence = 0.8)
