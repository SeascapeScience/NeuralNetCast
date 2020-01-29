library(keras)

data <- read.csv('../Data/CWM_yearsub.csv')
for (i in 4:11)
{
  data[,i] <- (data[,i]-mean(data[,i]))/sd(data[,i])
  data[is.na(data[,i]),i] <- 0
  data[,i] <- data[,i] - min(data[,i])
  data[,i] <- data[,i] / max(data[,i])
}
#data[is.na(data)] <- 0
subareas <- unique(data$Subarea)
years <- unique(data$Year)
q <- quantile(data$Trophic.level)

nyears <- 5 # Number of years-back to include

#images <- matrix(data=NA,nrow=length(subareas)*length(min(years):(max(years)-nyears)),ncol=nyears*8)
images <- array(data=100, dim=c(length(subareas)*length(min(years):(max(years)-nyears)),nyears,8))
labels <- matrix(data=NA,nrow=length(subareas)*length(min(years):(max(years)-nyears)))
idx <- 0
for (i in 1:length(subareas))
{
  for (j in min(years):(max(years)-nyears))
  {
    idx <- idx + 1
    m <- matrix(data=NA, nrow=nyears, ncol=8)
    for (k in 0:(nyears-1))
    {
      m[k,]=as.double(
        data[data$Subarea==subareas[i] & data$Year==j+k,4:11])
    }
    #images[idx,1:(nyears*8)] <- array_reshape(m,nyears*8)
    images[idx,1:nyears,1:8] <- m
    labels[idx] <- sum(data$Trophic.level[data$Subarea==subareas[i] & data$Year==j+k+1]<q)
  }
}

labels <- to_categorical(labels)

network <- keras_model_sequential() %>%
  layer_dense(units = 512, activation = "relu", input_shape = c(nyears,8)) %>%
  layer_dense(units = 5, activation = "softmax")

network %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

network %>% fit(images[1:3000,,], 
                labels[1:3000,], 
                epochs = 5, 
                batch_size = 128)

metrics <- network %>% evaluate(images[3001:3969,], labels[3001:3969,])
