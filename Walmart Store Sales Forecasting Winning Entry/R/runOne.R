setwd("D:/RProgram/random_code/Walmart Store Sales Forecasting Winning Entry/R")
source('util.R')
setwd("D:/RProgram/random_code/Walmart Store Sales Forecasting Winning Entry/R")
source('grouped.forecast.R')
setwd("D:/RProgram/random_code/Walmart Store Sales Forecasting Winning Entry/R")
source('postprocess.R')

train <- raw.train()
test <- raw.test()

# Best single model from the competition
pred <- grouped.forecast(train, test, 'stlf.svd', model.type = 'ets', n.comp = 12)
pred <- postprocess(train, pred, shift = 2.5)
snum <- write.submission(pred)
