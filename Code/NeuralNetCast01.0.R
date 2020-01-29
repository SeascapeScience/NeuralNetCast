# Functions for neural network based forecast of species
library(keras)

speciesLoad <- function(fname = '../Data/CWM_yearsub.csv')
{
  data <- read.csv(fname)
  return(data)
}

standardizeData <- function(data = NA)
{
  # Standardizes each column to mean zero, standard dev 1
  for (i in 1:dim(data)[2]) # normalize/standardize data by column
  {
    data[,i] <- (data[,i]-mean(data[,i]))/sd(data[,i])
    return(data)
  }
}
arrangeImages <- function(data = NA, times=NA, areas=NA,
                          labelcol = NA, 
                          ntimes = 5, nquants = 5)
{
  # data is the input data table
  # times is the time step associated with each row
  # areas is the location associated with each row
  # nyears is the number of time steps back to include in each image
  # nquants is the number of quantiles to use for categories
  #    nquants=0 uses continuous prediction (not working yet)
  # labelcol is the outcome label
  # Standardizes each column to mean zero, standard dev 1
  for (i in 1:dim(data)[2]) # normalize/standardize data by column
  {
    data[,i] <- (data[,i]-mean(data[,i]))/sd(data[,i])
  }
  
  subareas <- unique(areas)
  subtimes <- unique(times)
  nspecies <- dim(data)[2]
  qu <- quantile(labelcol,probs=seq(1/nquants,1,1/nquants))
  
  images <- matrix(data=NA,
                   nrow=length(subareas)*length(min(subtimes):(max(subtimes)-ntimes)),
                   ncol=ntimes*8)
  label <- matrix(data=NA,
                   nrow=length(subareas)*length(min(subtimes):(max(subtimes)-ntimes)))
  labelarea <- label
  labeltime <- label
  idx <- 0
  for (i in 1:length(subareas))
  {
    for (j in min(subtimes):(max(subtimes)-ntimes))
    {
      idx <- idx + 1
      m <- matrix(data=NA, nrow=ntimes, ncol=nspecies)
      for (k in 0:(ntimes-1))
      {
        m[k+1,]=as.double(
          data[areas==subareas[i] & times==j+k,])
        if (is.na(m[k+1,1])) # Use overall mean for missing values
        {
          m[k+1,]=as.double(colMeans(data[,]))
        }
      }
      images[idx,1:(ntimes*nspecies)] <- array_reshape(m,ntimes*nspecies)
      label[idx] <- max(1,sum(labelcol[areas==subareas[i] & times==j+k+1]<=qu))-1
      labelarea[idx] <- as.character(areas[i])
      labeltime[idx] <- times[i]
    }
  }
  output <- list(images = images, labels = label,
                 labelarea = labelarea,
                 labeltime = labeltime)
  return(output)
}

# Run neural network forecast model
netForecast <- function(images = NA, label = NA)
{
  label <- to_categorical(label)
  
  # Indices for testing and training (right now, 80%)
  Itrain <- 1:floor(dim(label)[1]*.8)
  Itest <- (floor(dim(label)[1]*.8)+1):dim(label)[1]
  
  
  network <- keras_model_sequential() %>%
    layer_dense(units = 512, activation = "relu", input_shape = c(dim(images)[2])) %>%
    layer_dense(units = 512, activation = "relu") %>%
    layer_dense(units = dim(label)[2], activation = "softmax")
  
  network %>% compile(
    optimizer = "rmsprop",
    loss = "categorical_crossentropy",
    metrics = c("accuracy")
  )
  
  history <- network %>% fit(images[Itrain,], 
                             label[Itrain,], 
                             epochs = 20, 
                             batch_size = 128)
  
  metrics <- network %>% evaluate(images[Itest,], label[Itest,])
  
  predictions <- network %>% predict(images[Itest,])
  predicted <- matrix(NA,nrow=dim(predictions)[1])
  measured <- matrix(NA,nrow=dim(predictions)[1])
  for (i in 1:dim(predictions)[1])
  {
    predicted[i] <- which.max(predictions[i,])+runif(1)*.4-.4
    measured[i] <- which.max(label[Itest[i],])+runif(1)*.4-.4
  }
  plot(predicted,measured)
  
  return(network)

}