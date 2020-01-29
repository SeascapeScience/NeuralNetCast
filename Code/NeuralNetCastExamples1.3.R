# NerualNetCastExamples


# Basic Example w/ trait data
source('NeuralNetCast01.3.R')
data <- speciesLoad('../Data/CWM_yearsub.csv',cleanup = F)
im <- arrangeImages(data = data[,4:11],
                    times = data$Year,
                    areas = data$Subarea,
                    labelcol = data$Trophic.level,
                    ntimes = 5)#,
                    #nquants = 5)

# Remove NAs
im <- imageCleanup(inputImage = im)

# Convert labels to quantiles
im$labelsorig <- im$labels
nquants <- 5
im$labels <- as.matrix(as.numeric(cut(im$labelsorig,
                                      breaks=quantile(im$labelsorig, probs=seq(0,1, by=1/nquants)),
                                      include.lowest=TRUE,
                                      labels = 1:nquants))) - 1



# Divide into training and testing indices
index <- getIndices(im$labeltime,trainoption = .6, randomize = T)

network<-netForecast(trainimages=im$images[index$train,],
                     trainlabels=im$labels[index$train],
                     testimages=im$images[index$test,],
                     testlabels=im$labels[index$test],
                     nepochs=20,
                     nunits = 128)

predictions <- netPlot(network=network,
                       testimages=im$images[index$test,],
                       testlabels=im$labels[index$test])


#
#
#
#
#
#
#
#

# EXAMPLE WITH ECOMON DATA
#
source('NeuralNetCast01.3.R')
data <- speciesLoad('../Data/ECOMON/ECOMONmanyregionsAUTUMN.csv')
data[,3:ncol(data)] <- log10(1+data[,3:ncol(data)])

# Calculate diversity index to use as label
H <- matrix(NA,nrow=dim(data)[1])
for (i in 1:length(H)) {
  Pi <- data[i,3:ncol(data)]/sum(data[i,3:ncol(data)],na.rm = T)
  H[i] <- -sum(Pi*(log(Pi)),na.rm=T)
}

# Arrange the data that goes into the NN
im <- arrangeImages(data = data[,3:ncol(data)],
                    times = data$Year,
                    areas = data$Region,
                    labelcol = H,#data$ctyp,
                    ntimes = 5)#,
# Remove NAs
im <- imageCleanup(inputImage = im)

# Convert labels to quantiles
im$labelsorig <- im$labels
nquants <- 5
im$labels <- as.matrix(as.numeric(cut(im$labelsorig,
                                      breaks=quantile(im$labelsorig, probs=seq(0,1, by=1/nquants)),
                                      include.lowest=TRUE,
                                      labels = 1:nquants))) - 1

# Divide into training and testing indices
index <- getIndices(im$labeltime,trainoption = .6, randomize = T)

network<-netForecast(trainimages=im$images[index$train,],
                     trainlabels=im$labels[index$train],
                     testimages=im$images[index$test,],
                     testlabels=im$labels[index$test],
                     nepochs=20,
                     nunits = 128)

predictions <- netPlot(network=network,
                       testimages=im$images[index$test,],
                       testlabels=im$labels[index$test])


# EXAMPLE WITH GULF OF MAINE LANDINGS
# 201 columns of species
# Years from 1970-2012
# ...but just one "subarea"
source('NeuralNetCast01.3.R')
data <- speciesLoad('~/Work/Data/LandingsR.csv') 
data[is.na(data)] <- 0 # I'm assuming NA means landings were 0

im <- arrangeImages(data = data[1,2:ncol(data)],
                    times = data$X,
                    areas = 0*data$X+1,
                    labelcol = data$COD..ATLANTIC,
                    ntimes = 5)
# Remove NAs
im <- imageCleanup(inputImage = im)

# Convert labels to quantiles
im$labelsorig <- im$labels
nquants <- 5
im$labels <- as.matrix(as.numeric(cut(im$labelsorig,
                                      breaks=quantile(im$labelsorig, probs=seq(0,1, by=1/nquants)),
                                      include.lowest=TRUE,
                                      labels = 1:nquants))) - 1

# Divide into training and testing indices
index <- getIndices(im$labeltime,trainoption = .6, randomize = T)

network<-netForecast(trainimages=im$images[index$train,],
                     trainlabels=im$labels[index$train],
                     testimages=im$images[index$test,],
                     testlabels=im$labels[index$test],
                     nepochs=20,
                     nunits = 128)

predictions <- netPlot(network=network,
                       testimages=im$images[index$test,],
                       testlabels=im$labels[index$test])
