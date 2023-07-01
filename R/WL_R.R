

score <- read.csv('1. Introduction/Score.csv',fileEncoding = 'UTF-8-BOM', na.strings = c('N/A', ''))
score_weight <- read.csv('1. Introduction/ScoreWeight.csv',fileEncoding = 'UTF-8-BOM', na.strings = c('N/A', ''))

colnames(score_weight)[0] <- 'Course.Code'

score <- score[complete.cases(score),]
score_weight <- score_weight[complete.cases(score_weight),]

score_combined <- merge(score, score_weight, by <- "Course.Code")

assignment <- score_combined$Assignment.x * score_combined$Assignment.y
mid_exam <- score_combined$Mid.Exam.x * score_combined$Mid.Exam.y
final_exam <- score_combined$Final.Exam.x * score_combined$Final.Exam.y


total <- assignment + mid_exam + final_exam

for (i in 1:length(total)) {
  if (total[i] <= 65) {
    letter_grades[i] <- "D"
  } else if (total[i] <= 70) {
    letter_grades[i] <- "C"
  } else if (total[i] <= 75) {
    letter_grades[i] <- "B-"
  } else if (total[i] <= 80) {
    letter_grades[i] <- "B"
  } else if (total[i] <= 85) {
    letter_grades[i] <- "B+"
  } else if (total[i] <= 90) {
    letter_grades[i] <- "A-"
  } else if (total[i] <= 100) {
    letter_grades[i] <- "A"
  } 
}

final_result <- cbind(score$Student.Name, total, letter_grades)

colnames(final_result) <- c("Student Name", "Total Score", "Grade")

write.csv(final_result, file = "result.csv", row.names = FALSE)


#######################################################################################


anime <- read.csv('2. Data Visualization/anime.csv',na.strings = c('N/A', ''))

anime <- anime[complete.cases(anime),]


anime_type <- table(anime$status)

anime_type <- as.data.frame(anime_type)

pie(anime_type$Freq, labels = anime_type$Var1)




anime_rating <- table(anime$rating)

anime_rating <- as.data.frame(anime_rating)

pie(anime_rating$Freq, labels = anime_rating$Var1)



anime_source <- table(anime$source)

anime_source <- as.data.frame(anime_source)

barplot(anime_source$Freq, col = rainbow(length(anime_source$Var1)))
legend(x = "topright", legend = anime_source$Var1, col = rainbow(length(anime_source$Var1)), pch = 16, bty = "n", cex=0.5)


anime_duration <- table(anime$duration)

anime_duration <- as.data.frame(anime_duration)

barplot(anime_duration$Freq, col = rainbow(length(anime_duration$Var1)))
legend(x = "topright", legend = anime_duration$Var1, col = rainbow(length(anime_duration$Var1)), pch = 16, bty = "n", cex=0.5)


#######################################################################################

header <- read.csv('3. Data Description and Frequent Pattern Analysis/Header.csv',na.strings = c('N/A', ''))
detail <- read.csv('3. Data Description and Frequent Pattern Analysis/Detail.csv',na.strings = c('N/A', ''))
items <- read.csv('3. Data Description and Frequent Pattern Analysis/items.csv',na.strings = c('N/A', ''))

header <- header[complete.cases(header),]
colnames(header)[1] <- 'transaction_id'
header
detail <- detail[complete.cases(detail),]
items <- items[complete.cases(items),]
colnames(items)[1] <- 'item_id'

header_detail <- merge(header, detail, by = 'transaction_id')
header_detail_items <- merge(header_detail, items, by = 'item_id')


split_data <- split(header_detail_items$name, header_detail_items$transaction_id)



install.packages('arules')
library('arules')

freq_itemset <- apriori(split_data,
                        parameter = list(supp = 0.05, target = 'frequent itemset'))
inspect(freq_itemset)


assoc_rule <- ruleInduction(freq_itemset, confidence = 0.5)
inspect(assoc_rule)
