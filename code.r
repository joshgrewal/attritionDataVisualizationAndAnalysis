> churn <- read.csv("/Users/joshgrewal/Desktop/churn.txt")
> library(tidyverse)
> library(datasets)
> library(e1071)
> install.packages("corrplot")
> library(corrplot)
> library(ggplot2)
> churnTibble <- as_tibble(churn)

> churn$Churn <- tolower(churn$Churn) == "true" // Converts Column to boolean datatype // needed for certain charts

#ONE
> summary(churnTibble)

#TWO
> true_churn_by_state <- churn %>% group_by(State) %>% summarise(True_Churn_Count = sum(Churn))

#THREE
> ggplot(churnTibble, aes(y = State, fill = State)) + geom_bar()

#FOUR
> churnDf <- as.data.frame(churn)
> ggplot(churnDf, aes(x = Night.Calls, fill = State)) + geom_density(alpha = .8) + labs(title = "Density of Night Calls by State",x = "Night Calls", y = "Density") + theme_minimal()

#FIVE
> totalCharges <- churn %>% mutate(Total.Charge = Day.Charge + Eve.Charge + Night.Charge + Intl.Charge)
> charge_comparison <- ggplot(totalCharges, aes(x = Churn, y = Total.Charge, fill = Churn)) + geom_boxplot() + labs(title = "Total Charges by Churn",x = "Churn",y = "Total Charge",fill = "Churn") + scale_fill_manual(values = c("TRUE" = "pink", "FALSE" = "lightblue")) + theme_minimal()
> print(charge_comparison)

#SIX
> totalMinutes <- churn %>% mutate(Total.Mins = Day.Mins + Eve.Mins + Night.Mins + Intl.Mins)
> Minutes_comparison <- ggplot(totalMinutes, aes(x = Churn, y = Total.Mins, fill = Churn)) + geom_boxplot() + labs(title = "Total Minutes by Churn", x = "Churn", y = "Total Minutes", fill = "Churn") + scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "salmon")) + theme_minimal()
> print(Minutes_comparison)

#SEVEN
> df <- churn %>% select(-State, -Account.Length,-VMail.Message ,-Area.Code, -Phone, -Int.l.Plan, -VMail.Plan)
> df <- transform(df, Churn = as.numeric(as.factor(Churn)))
> df$Churn <- factor(df$Churn., levels = c(2, 1), labels = c("True", "False"))
> set.seed(1234)
> train <- sample(nrow(df), 0.7 * nrow(df))
> df.train <- df[train, ]
> df.validate <- df[-train, ]
> print(table(df.train$Churn.))

> print(table(df.validate$Churn.))

> library(rpart)
> decisiontree <- rpart(Churn. ~ ., data = df.train, method = "class")
> print(decisiontree$cptable)

> decisiontree.pruned <- prune(decisiontree, cp = 0.01)
> library(rpart.plot)
> print(prp(decisiontree.pruned, type = 2, extra = 104, main = "Decision Tree"))

> decisiontree.pred <- predict(decisiontree.pruned, df.validate, type = "class")
> dtdecisiontreeree.perf <- table(df.validate$Churn., decisiontree.pred, dnn = c("Actual", "Predicted"))
> print(decisiontree.perf)

> tn <- decisiontree.perf[1, 1]
> fp <- decisiontree.perf[1, 2]
> fn <- decisiontree.perf[2, 1]
> tp <- decisiontree.perf[2, 2]
> accuracy <- (tp + tn) / (tp + tn + fp + fn)
> error_rate <- (fp + fn) / (tp + tn + fp + fn)
> sensitivity <- tp / (tp + fn)
> specificity <- tn / (tn + fp)
> precision <- tp / (tp + fp)
> recall <- tp / (tp + fn)
> f_measure <- (2 * precision * recall) / (precision + recall)
> print(accuracy)
[1] 0.906
> print(error_rate)
[1] 0.094
> print(sensitivity)
[1] 0.9792148
> print(specificity)
[1] 0.4328358
> print(precision)
[1] 0.9177489
> print(recall)
[1] 0.9792148
> print(f_measure)
[1] 0.947486

#EIGHT
> churn_summary <- churn %>% group_by(Churn) %>% summarise(Count = n())
> churn_summary <- mutate(churn_summary, Percent = (Count / sum(Count)) * 100)
> pie_chart <- ggplot(churn_summary, aes(x = "", y = Percent, fill = factor(Churn))) + geom_bar(width = 1, stat = "identity") + coord_polar("y", start = 0) + labs(title = "Percentage of Churn Being True or False", fill = "Churn", y = NULL) + theme_void()
> print(pie_chart)

#NINE
> nb.model <- naiveBayes(Churn.~.,data = df.train)
> nb.pred <- predict(nb.model, df.validate)
> nb.perf <- table(df.validate$Churn., nb.pred, dnn=c("Actual", "Predicted"))
> print(nb.perf)

> tn <- nb.perf[1, 1]
> fp <- nb.perf[1, 2]
> fn <- nb.perf[2, 1]
> tp <- nb.perf[2, 2]
> accuracy <- (tp + tn) / (tp + tn + fp + fn)
> error_rate <- (fp + fn) / (tp + tn + fp + fn)
> sensitivity <- tp / (tp + fn)
> specificity <- tn / (tn + fp)
> precision <- tp / (tp + fp)
> recall <- tp / (tp + fn)
> f_measure <- (2 * precision * recall) / (precision + recall)
> print(accuracy)
[1] 0.88
> print(error_rate)
[1] 0.12
> print(sensitivity)
[1] 0.960739
> print(specificity)
[1] 0.358209
> print(precision)
[1] 0.9063181
> print(recall)
[1] 0.960739
> print(f_measure)

#TEN
> churn <- read.csv("/Users/joshgrewal/Desktop/churn.txt") // reset the churn dataset
> churn <- transform(churn, Churn = as.numeric(as.factor(Churn)))
> churn <- transform(churn, State = as.numeric(as.factor(State)))
> churn <- transform(churn,  Int.l.Plan = as.numeric(as.factor( Int.l.Plan)))
> churn <- transform(churn,  VMail.Plan = as.numeric(as.factor( VMail.Plan)))
> View(churn)
> churn <- transform(churn,  Phone = as.numeric(as.factor(Phone)))
> correlation_matrix <- cor(churn)
> title("Correlation Matrix Plot")
> corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust", tl.col = "black", tl.srt = 45, col = colorRampPalette(c("blue", "white", "red"))(100))
> title("Correlation Matrix Plot")
