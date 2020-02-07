# Functions for neural network based forecast of species
library(keras)
library(ggplot2)

# TODO
# - Make data cleanup a function
# - Make index selection for train/test a function
#      - choose based on time(s), area(s), or percent (random)

speciesLoad <- function(fname = '../Data/CWM_yearsub.csv', 
                        cleanup = T,
                        cleanupindex = 1)
{
  data <- read.csv(fname)
  cI <- cleanupindex:ncol(data)
  if (cleanup) {
    data[is.nan(as.matrix(data))] <- NA
    I <- which(colSums(data[,cI],na.rm=T)>0)
    data[,cI] <- data[,(I+cleanupindex-1)]
    I <- which(rowSums(data[,cI],na.rm=T)!=0)
    data <- data[I,] 
  }
  return(data)
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

imageCleanup <- function(inputImage = NA)
{
  I <- which(!is.na(inputImage$labels))
  inputImage$images <- inputImage$images[I,]
  inputImage$labels <- inputImage$labels[I]
  inputImage$labelarea <- inputImage$labelarea[I]
  inputImage$labeltime <- inputImage$labeltime[I]
  return(inputImage)
}
# Get training and testing indices
getIndices <- function(labelinfo = NA, testoption = NA, 
                       randomize = T, trainrandom = .6)
{
  if (randomize) {
    r <- runif(length(labelinfo))
    tr <- which(r>=trainrandom)
    te <- which(r<trainrandom)
  } else {
    te <- which(labelinfo==testoption)
    tr <- which(labelinfo!=testoption)
  }
  index <- list("train" = tr, "test" = te)
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
    predicted[i] <- which.max(predictions[i,])#+runif(1)*.4-.4
    measured[i] <- which.max(testlabels[i,])#+runif(1)*.4-.4
  }
  #plot(predicted,measured)
  confusion_matrix <- as.data.frame(table(predicted,measured))
  thisplot <- ggplot(data = confusion_matrix,
         mapping = aes(x = predicted,
                       y = measured)) +
    geom_tile(aes(fill = Freq)) +
    geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
    scale_fill_gradient(low = "blue",
                        high = "red",
                        trans = "log")
  print(thisplot)
  return(predictions)
}