library(ISLR)
library(caTools)
library(neuralnet)
set.seed(101)

data("College")
print(head(College,2))
maxs <- apply(College[,2:18], 2, max)
mins <- apply(College[,2:18], 2, min)
scaled.data <- as.data.frame(scale(College[,2:18],center = mins, scale = maxs - mins))
print(head(scaled.data,2))

Private = as.numeric(College$Private)-1
data = cbind(Private,scaled.data)

# Create Split (any column is fine)
split = sample.split(data$Private, SplitRatio = 0.70)

# Split based off of split Boolean Vector
train = subset(data, split == TRUE)
test = subset(data, split == FALSE)

feats <- names(scaled.data)

# Concatenate strings
f <- paste(feats,collapse=' + ')
f <- paste('Private ~',f)

# Convert to formula
f <- as.formula(f)

f

nn <- neuralnet(f,train,hidden=c(10,10,10),linear.output=FALSE)

# Compute Predictions off Test Set
predicted.nn.values <- compute(nn,test[2:18])

# Check out net.result
print(head(predicted.nn.values$net.result))

predicted.nn.values$net.result <- sapply(predicted.nn.values$net.result,round,digits=0)
table(test$Private,predicted.nn.values$net.result)
plot(nn)

