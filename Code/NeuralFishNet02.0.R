library(keras)

data <- read.csv('../Data/CWM_yearsub.csv')


# Some things to define & play with
nyears <- 5 # Number of years-back to include
LabelsColumn <- data$Trophic.level # Column to use as label
nquants <- 5 # Number of quantiles to use as categories


for (i in 4:11) # normalize/standardize data by column
{
  #data[,i] <- scale(data[,i], center = mean, scale = std) #This didn't seem to scale the same way...need to check
  data[,i] <- (data[,i]-mean(data[,i]))/sd(data[,i])
  #data[is.na(data[,i]),i] <- 0
  #data[,i] <- data[,i] - min(data[,i])
  #data[,i] <- data[,i] / max(data[,i])
  #data$Subarea[i] <- substr(as.character(data$YearSubarea[i]),6,11)
}
LabelsColumn <- (LabelsColumn-mean(LabelsColumn))/sd(LabelsColumn)
subareas <- unique(data$Subarea)
years <- unique(data$Year)
qu <- quantile(LabelsColumn,probs=seq(1/nquants,1,1/nquants))



images <- matrix(data=NA,
                 nrow=length(subareas)*length(min(years):(max(years)-nyears)),
                 ncol=nyears*8)
labels <- matrix(data=NA,
                 nrow=length(subareas)*length(min(years):(max(years)-nyears)))
idx <- 0
for (i in 1:length(subareas))
{
  for (j in min(years):(max(years)-nyears))
  {
    idx <- idx + 1
    m <- matrix(data=NA, nrow=nyears, ncol=8)
    for (k in 0:(nyears-1))
    {
      m[k+1,]=as.double(
        data[data$Subarea==subareas[i] & data$Year==j+k,4:11])
      if (is.na(m[k+1,1]))
      {
        m[k+1,]=as.double(colMeans(data[,4:11]))
      }
    }
    images[idx,1:(nyears*8)] <- array_reshape(m,nyears*8)
    labels[idx] <- max(1,sum(LabelsColumn[data$Subarea==subareas[i] & data$Year==j+k+1]<=qu))-1
  }
}


labels <- to_categorical(labels)

# Indices for testing and training
Itrain <- 1:floor(dim(labels)[1]*.8)
Itest <- (floor(dim(labels)[1]*.8)+1):dim(labels)[1]


network <- keras_model_sequential() %>%
  layer_dense(units = 512, activation = "relu", input_shape = c(nyears*8)) %>%
  layer_dense(units = 512, activation = "relu") %>%
  layer_dense(units = nquants, activation = "softmax")

network %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

history <- network %>% fit(images[Itrain,], 
                labels[Itrain,], 
                epochs = 20, 
                batch_size = 128)

metrics <- network %>% evaluate(images[Itest,], labels[Itest,])


predictions <- network %>% predict(images[Itest,])
predicted <- matrix(NA,nrow=dim(predictions)[1])
measured <- matrix(NA,nrow=dim(predictions)[1])
for (i in 1:dim(predictions)[1])
{
  predicted[i] <- which.max(predictions[i,])+runif(1)*.4-.4
  measured[i] <- which.max(labels[Itest[i],])+runif(1)*.4-.4
}
plot(predicted,measured)
