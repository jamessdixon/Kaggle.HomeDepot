library(readr)
cat("Reading data\n")
train <- read_csv('F:/Git/Kaggle.HomeDepot/ProjectSearchRelevanceR/input/train.csv')
test <- read_csv('F:/Git/Kaggle.HomeDepot/ProjectSearchRelevanceR/input/test.csv')
desc <- read_csv('F:/Git/Kaggle.HomeDepot/ProjectSearchRelevanceR/input/product_descriptions.csv')

cat("Merge description with train and test data \n")
train <- merge(train,desc, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE)
test <- merge(test,desc, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE)

t <- Sys.time()
word_match <- function(words,title,desc){
  n_title <- 0
  n_desc <- 0
  words <- unlist(strsplit(words," "))
  nwords <- length(words)
  for(i in 1:length(words)){
    pattern <- paste("(^| )",words[i],"($| )",sep="")
    n_title <- n_title + grepl(pattern,title,perl=TRUE,ignore.case=TRUE)
    n_desc <- n_desc + grepl(pattern,desc,perl=TRUE,ignore.case=TRUE)
  }
  return(c(n_title,nwords,n_desc))
}

cat("Get number of words and word matching title in train\n")
train_words <- as.data.frame(t(mapply(word_match,train$search_term,train$product_title,train$product_description)))
train$nmatch_title <- train_words[,1]
train$nwords <- train_words[,2]
train$nmatch_desc <- train_words[,3]

cat("Get number of words and word matching title in test\n")
test_words <- as.data.frame(t(mapply(word_match,test$search_term,test$product_title,test$product_description)))
test$nmatch_title <- test_words[,1]
test$nwords <- test_words[,2]
test$nmatch_desc <- test_words[,3]

rm(train_words,test_words)

cat("A simple linear model on number of words and number of words that match\n")
glm_model <- glm(relevance~nmatch_title+nmatch_desc+nwords,data=train)
test_relevance <- predict(glm_model,test)
test_relevance <- ifelse(test_relevance>3,3,test_relevance)
test_relevance <- ifelse(test_relevance<1,1,test_relevance)

submission <- data.frame(id=test$id,relevance=test_relevance)
write_csv(submission,"benchmark_submission.csv")
print(Sys.time()-t)
