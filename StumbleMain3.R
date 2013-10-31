#StumbleUpon Challenge
#Gettin' pro
#Ver 3.0

#########################
#Init
rm(list=ls(all=TRUE))
#Set Working Directory
DATA_PATH <- "D:/Wacax/Kaggle Data Analysis/StumbleUpon Evergreen/"
setwd(DATA_PATH)

###########################
trainData <- read.table('train.tsv', header = TRUE, sep = "\t", stringsAsFactors = FALSE)
testData <- read.table('test.tsv', header = TRUE, sep = "\t", stringsAsFactors = FALSE)

mTrain <- length(trainData$boilerplate)
mTest <- length(testData$boilerplate)

#Labels corresponding to sampled data
y <- trainData$label

#Alchemy classes creation
index <- as.numeric(as.factor(c(trainData$alchemy_category, testData$alchemy_category)))
identityMatrix <- diag(max(index))
categories <- t(sapply(index, anonFun <- function(x, identityMatrix){identityMatrix[x, ]}, identityMatrix))

categoriesTrain <- categories[1:mTrain, ]
categoriesTest <- categories[(mTrain+1):(mTrain+mTest), ]

#Process boilerplates' text
source(paste0(DATA_PATH, 'score.sentiment.R'))
source(paste0(DATA_PATH, 'text2Matrix.R'))

#Sentiment Analysis
inquirer <- read.csv('inquirerbasic.csv')
negativeIndices <- inquirer[, 4] %in% 'Negativ'
positiveIndices <- inquirer[, 3] %in% 'Positiv'
negativeWords <- tolower(inquirer[negativeIndices, 1])
positiveWords <- tolower(inquirer[positiveIndices, 1])

require(RJSONIO)
boilerplate <- lapply(trainData$boilerplate, fromJSON, simply = FALSE, USE.NAMES = FALSE, simplifyWithNames = FALSE)
boilerplate <- lapply(boilerplate, function(x){return(dummy <- c(x$body))})
boilerplatesSentimentTrain <- sapply(boilerplate, score.sentiment, positiveWords, negativeWords)
nullIndexesTrain <- which(sapply(boilerplatesSentimentTrain, class, USE.NAMES = FALSE) == 'logical')
boilerplatesSentimentTrain[nullIndexesTrain] <- 0
boilerplatesSentimentTrain <- unlist(boilerplatesSentimentTrain)
boilerplate <- lapply(testData$boilerplate, fromJSON, simply = FALSE, USE.NAMES = FALSE, simplifyWithNames = FALSE)
boilerplate <- lapply(boilerplate, function(x){return(dummy <- c(x$body))})
boilerplatesSentimentTest <- sapply(boilerplate, score.sentiment, positiveWords, negativeWords)
nullIndexesTest <- which(sapply(boilerplatesSentimentTest, class, USE.NAMES = FALSE) == 'logical')
boilerplatesSentimentTest[nullIndexesTest] <- 0
boilerplatesSentimentTest <- unlist(boilerplatesSentimentTest)

#SPARSE MATRIX PREPROP
#Extract a simple_triplet_matrix from text 
bpTrainAndTestSparse <- text2Matrix(c(trainData$boilerplate, testData$boilerplate), 1, Inf)

#Transform simple_triplet_matrix to a simple disperse
require(glmnet)
bpTrainAndTestSparse <- sparseMatrix(i=bpTrainAndTestSparse$i, j=bpTrainAndTestSparse$j, x=bpTrainAndTestSparse$v, dims=c(bpTrainAndTestSparse$nrow, bpTrainAndTestSparse$ncol))
inherits(bpTrainAndTestSparse,"sparseMatrix")

# Big matrix + LSA, currently not working
#LSAbpTrainAndTestSparse <- lsa(bpTrainAndTestSparse)

boilerplatesTrain <- bpTrainAndTestSparse[1:mTrain, ]
boilerplatesTest <- bpTrainAndTestSparse[(mTrain+1):(mTrain+mTest), ]

#Save data for laterz
save(boilerplatesSentimentTrain, file = 'boilerplatesSentimentTrain.RData')
save(boilerplatesSentimentTest, file = 'boilerplatesSentimentTest.RData')
save(boilerplatesTrain, file = 'boilerplatesTrain.RData')
save(boilerplatesTest, file = 'boilerplatesTest.RData')
save(y, file = 'y.RData')


############################################
#3-fold Cross validation
require(glmnet)

indexTrain <- sample(1:mTrain, floor(mTrain*0.60))
indexCV <- sample((1:mTrain)[!(1:mTrain %in% indexTrain)], floor(mTrain * 0.2))
indexTest <- (1:mTrain)[!((1:mTrain) %in% c(indexTrain, indexCV))]

###########################################
#Test distributions
require(glmnet)
require(gbm)
distributions <- c("gaussian","binomial","poisson","multinomial")

rocs <- rep(0, length(distributions))

for (i in 1:length(distributions)){
  model <- glmnet(boilerplatesTrain[indexTrain, ], y[indexTrain], family = distributions[i])
  prediction <- predict(model, boilerplatesTrain[indexCV, ], s=c(0.01))
  rocs[i] <- gbm.roc.area(y[indexCV], prediction)  
  
}

plot(rocs)

#Test alpha values
alphas <- c(0.01, 0.03, 0.1, 0.3, 1)

rocs <- rep(0, length(alphas))

for (i in 1:length(distributions)){
  model <- glmnet(boilerplatesTrain[indexTrain, ], y[indexTrain], alpha = alphas[i])
  prediction <- predict(model, boilerplatesTrain[indexCV, ], s=c(0.01))
  rocs[i] <- gbm.roc.area(y[indexCV], prediction)  
  
}

plot(rocs)


#Cross Validate best features
#features <- c('predictionTrain', 'boilerplatesSentimentTrain', 'categories', 'trainData$linkwordscore', 'log(trainData$numberOfLinks)')
#combinations <- list('C(1, 2)', 'C(1, 3)', 'C(1, 4)', 'C(1, 5)', 
#                     'C(1, 2, 3)', 'C(1, 2, 4)', 'C(1, 2, 5)', 
#                     'C(1, 2, 3, 4, 5)', 'C(1, 2, 3, 4)')

#featuresTry = list(cbind(predictionTrain, boilerplatesSentimentTrain, categories, trainData$linkwordscore, log(trainData$numberOfLinks)), 
#                    (cbind(predictionTrain, boilerplatesSentimentTrain, categories, trainData$linkwordscore)))
  
#rocs <- rep(0, length(featuresTry))

#Test lambda values, doin' it wrong
cvglmnetModel1 <- cv.glmnet(boilerplatesTrain[indexTrain, ], y[indexTrain], nfolds = 10)
predictionTrain <- predict(cvglmnetModel1$glmnet.fit, boilerplatesTrain[indexCV, ])

cvglmnetModel2 <- cv.glmnet(cbind(predictionTrain, boilerplatesSentimentTrain[indexCV], categoriesTrain[indexCV, ], trainData$linkwordscore[indexCV], log(trainData$numberOfLinks[indexCV])), trainData$commonlinkratio_1, trainData$commonlinkratio_2, trainData$commonlinkratio_3, trainData$commonlinkratio_4
                        y[indexCV], nfolds = 10)
  
cvglmnetModel3 <- cv.glmnet(boilerplatesTrain[c(indexTrain, indexCV), ], y[c(indexTrain, indexCV)], nfolds = 10)

predictionTrain <- predict(cvglmnetModel3$glmnet.fit, boilerplatesTrain[indexTest, ]) 

prediction <- predict(cvglmnetModel2$glmnet.fit, 
                        cbind(predictionTrain, boilerplatesSentimentTrain[indexTest], categoriesTrain[indexTest, ], trainData$linkwordscore[indexTest], log(trainData$numberOfLinks[indexTest])))
prediction <- apply(prediction, 1, mean)


roc <- gbm.roc.area(y[indexTest], prediction)  

###############################################
#Full Model Training with best parameters + sentiment analysis + categories + linkwordscore (cor -17%)
#Cross Validation
indexTrain <- sample(1:mTrain, floor(mTrain*0.50))
indexCV <- (1:mTrain)[!(1:mTrain %in% indexTrain)]

#Model

cvglmnetModel1 <- cv.glmnet(boilerplatesTrain[indexTrain, ], y[indexTrain], nfolds = 200)
predictionTrain <- predict(cvglmnetModel1$glmnet.fit, boilerplatesTrain[indexCV, ])

cvglmnetModel2 <- cv.glmnet(cbind(predictionTrain, boilerplatesSentimentTrain[indexCV], categoriesTrain[indexCV, ], trainData$linkwordscore[indexCV], log(trainData$numberOfLinks[indexCV])), trainData$commonlinkratio_1, trainData$commonlinkratio_2, trainData$commonlinkratio_3, trainData$commonlinkratio_4
                            y[indexCV], nfolds = 200)

cvglmnetModel3 <- cv.glmnet(boilerplatesTrain, y, nfolds = 200)

predictionTrain <- predict(cvglmnetModel3$glmnet.fit, boilerplatesTest) 

prediction <- predict(cvglmnetModel2$glmnet.fit, 
                      cbind(predictionTrain, boilerplatesSentimentTest, categoriesTest, testData$linkwordscore, log(testData$numberOfLinks)))
prediction <- apply(prediction, 1, mean)


source(paste0(DATA_PATH, 'sigmf.R'))
label <- sigmf(prediction, -11, mean(prediction))
label <- round(label)

label[label < 0] <- 0
label[label > 1] <- 1

################################################
#Save Data
urlid <- testData$urlid
write.csv(cbind(urlid, label), file = "predictionXXVI.csv", row.names = FALSE, col.names = c('urlid', 'label'))

sapply(names(trainData), anonFun <- function(x, y){
  cor(y, trainData$paste0(x))
  }, y)
