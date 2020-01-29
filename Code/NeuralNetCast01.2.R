# Functions for neural network based forecast of species
library(keras)

# TODO
# - Make data cleanup a function
# - Make index selection for train/test a function
#      - choose based on year(s), area(s), or percent (random)

speciesLoad <- function(fname = '../Data/CWM_yearsub.csv')
{
  data <- read.csv(fname)
  return(data)
}

standardizeData <- function(data = NA) # Not working
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
                           ntimes = 5)
{
  # data is the input data table
  # times is the time step associated with each row
  # areas is the location associated with each row
  # nyears is the number of time steps back to include in each image
  # labelcol is the outcome label
  
  # Standardizes each column to mean zero, standard dev 1
  for (i in 1:dim(data)[2]) # normalize/standardize data by column
  {
    data[,i] <- (data[,i]-mean(data[,i],na.rm = T))/sd(data[,i],na.rm = T)
  }
  subareas <- unique(areas)
  subtimes <- unique(times)
  nspecies <- dim(data)[2]
  
  images <- matrix(data=NA,
                   nrow=length(subareas)*(length(subtimes)-ntimes),
                   ncol=ntimes*nspecies)
  label <- matrix(data=NA,
                  nrow=length(subareas)*(length(subtimes)-ntimes))
  labelarea <- label
  labeltime <- label
  
  idx <- 0 # This index counts the images added
  
  for (i in 1:length(subareas))
  {
    for (j in (ntimes+1):length(subtimes))
    {
      I <- which(areas==subareas[i] & times==subtimes[j])
      if (length(I)>0)
      {
        J <- which(areas==subareas[i] & times<subtimes[j] 
                   & times>=subtimes[j]-ntimes)
        m <- as.vector(as.matrix(data[J,]))
        if (length(m)==ntimes*nspecies)
        {
          idx <- idx + 1
          images[idx,] <- m
          label[idx] <- labelcol[I]
          labelarea[idx] <- subareas[i]
          labeltime[idx] <- subtimes[j]
        }
      }
    }
  }
  output <- list(images = images, labels = label,
                 labelarea = labelarea,
                 labeltime = labeltime)
  return(output)
}

arrangeImagesOld <- function(data = NA, times=NA, areas=NA,
                          labelcol = NA, 
                          ntimes = 5)#, nquants = 5)
{
 # -- NOT WORKING ---
  
  # Standardizes each column to mean zero, standard dev 1
  for (i in 1:dim(data)[2]) # normalize/standardize data by column
  {
    data[,i] <- (data[,i]-mean(data[,i]))/sd(data[,i])
  }
  
  subareas <- unique(areas)
  subtimes <- unique(times)
  nspecies <- dim(data)[2]
  #qu <- quantile(labelcol,probs=seq(1/nquants,1,1/nquants))
  
  images <- matrix(data=NA,
                   nrow=length(subareas)*length(min(subtimes):(max(subtimes)-ntimes)),
                   ncol=ntimes*nspecies)
  label <- matrix(data=NA,
                   nrow=length(subareas)*length(min(subtimes):(max(subtimes)-ntimes)))
  labelarea <- label
  labeltime <- label
  idx <- 0 # This index counts the images added
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
        #print(as.double(data[areas==subareas[i] & times==j+k,]))
        if (is.na(m[k+1,1])) # Use overall mean for missing values
        {
          m[k+1,]=as.double(colMeans(data[,]))
        }
      }
      #print(dim(images))
      images[idx,1:(ntimes*nspecies)] <- array_reshape(m,ntimes*nspecies)
      #label[idx] <- max(1,sum(labelcol[areas==subareas[i] & times==j+k+1]<=qu))-1
      print(c(subareas[i],i,j,k,idx))
      print(as.double(labelcol[areas==subareas[i] & times==j+k]))
      label[idx] <- as.double(labelcol[areas==subareas[i] & times==j+k])
      labelarea[idx] <- as.character(areas[i])
      labeltime[idx] <- j+k+1
    }
  }
  output <- list(images = images, labels = label,
                 labelarea = labelarea,
                 labeltime = labeltime)
  return(output)
}



# Run neural network forecast model
netForecast <- function(trainimages = NA, trainlabels = NA,
                        testimages = NA, testlabels = NA,
                        nepochs = 20, nunits=512)
{
  trainlabels <- to_categorical(trainlabels)
  testlabels <- to_categorical(testlabels)
  
  network <- keras_model_sequential() %>%
    layer_dense(units = nunits, activation = "relu", 
                input_shape = c(dim(trainimages)[2])) %>%
    layer_dense(units = nunits, activation = "relu") %>%
    layer_dense(units = dim(trainlabels)[2], activation = "softmax")
  
  network %>% compile(
    optimizer = "rmsprop",
    loss = "categorical_crossentropy",
    metrics = c("accuracy")
  )
  
  history <- network %>% fit(trainimages, 
                             trainlabels, 
                             epochs = nepochs, 
                             batch_size = 128)
  
  metrics <- network %>% evaluate(testimages, testlabels)
  return(network)
}

netPlot <- function(network = NA, testimages = NA, testlabels = NA)
{
  testlabels <- to_categorical(testlabels)
  
  predictions <- network %>% predict(testimages)
  predicted <- matrix(NA,nrow=dim(predictions)[1])
  measured <- matrix(NA,nrow=dim(predictions)[1])
  for (i in 1:dim(predictions)[1])
  {
    predicted[i] <- which.max(predictions[i,])+runif(1)*.4-.4
    measured[i] <- which.max(testlabels[i,])+runif(1)*.4-.4
  }
  plot(predicted,measured)
  
  return(predictions)
  #comparison <- list("predicted" = predicted, "measured" = measured)
  #return(comparison)
}