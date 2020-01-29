# NerualNetCastExamples


# Basic Example w/ trait data
source('NeuralNetCast01.1.R')
data <- read.csv('../Data/CWM_yearsub.csv')
im <- arrangeImages(data = data[,4:11],
                    times = data$Year,
                    areas = data$Subarea,
                    labelcol = data$Trophic.level,
                    ntimes = 5)#,
                    #nquants = 5)

# Remove NAs
I <- which(!is.na(im$labels))
im$images <- im$images[I,]
im$labels <- im$labels[I]
im$labelarea <- im$labelarea[I]
im$labeltime <- im$labeltime[I]

# Convert labels to quantiles
nquants <- 5
im$labelsorig <- im$labels
im$labels <- as.matrix(as.numeric(cut(im$labelsorig,
                                      breaks=quantile(im$labelsorig, probs=seq(0,1, by=1/nquants)),
                                      include.lowest=TRUE,
                                      labels = 1:nquants))) - 1


# Divide into training and testing indices
Itrain <- 1:floor(dim(im$labels)[1]*.8)
Itest <- (floor(dim(im$labels)[1]*.8)+1):dim(im$labels)[1]

network<-netForecast(trainimages=im$images[Itrain,],
                     trainlabels=im$labels[Itrain],
                     testimages=im$images[Itest,],
                     testlabels=im$labels[Itest],
                     nepochs=20,
                     nunits = 512)

predictions <- netPlot(network=network,
                       testimages=im$images[Itest,],
                       testlabels=im$labels[Itest])

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
data <- read.csv('../Data/ECOMONmanyregionsFebJun.csv')
I <- which(colSums(data,na.rm=T)>0)
data <- data[,I]
I <- which(rowSums(data[,3:ncol(data)],na.rm=T)!=0)
data <- data[I,]
data[,3:ncol(data)] <- log10(1+data[,3:ncol(data)])
H <- matrix(NA,nrow=dim(data)[1])
for (i in 1:length(H)) {
  Pi <- data[i,3:ncol(data)]/sum(data[i,3:ncol(data)],na.rm = T)
  H[i] <- -sum(Pi*(log(Pi)),na.rm=T)
}
im <- arrangeImages(data = data[,3:ncol(data)],
                    times = data$Year,
                    areas = data$Region,
                    labelcol = H,#data$ctyp,
                    ntimes = 3)#,
# Remove NAs
I <- which(!is.na(im$labels))
im$images <- im$images[I,]
im$labels <- im$labels[I]
im$labelarea <- im$labelarea[I]
im$labeltime <- im$labeltime[I]
# Convert labels to quantiles
im$labelsorig <- im$labels
nquants <- 4
im$labels <- as.matrix(as.numeric(cut(im$labelsorig,
                                      breaks=quantile(im$labelsorig, probs=seq(0,1, by=1/nquants)),
                                      include.lowest=TRUE,
                                      labels = 1:nquants))) - 1
# Divide into training and testing indices
Itrain <- 1:floor(dim(im$labels)[1]*.6)
Itest <- (floor(dim(im$labels)[1]*.6)+1):dim(im$labels)[1]

network<-netForecast(trainimages=im$images[Itrain,],
                     trainlabels=im$labels[Itrain],
                     testimages=im$images[Itest,],
                     testlabels=im$labels[Itest],
                     nepochs=10,
                     nunits = 128)

predictions <- netPlot(network=network,
                       testimages=im$images[Itest,],
                       testlabels=im$labels[Itest])


# EXAMPLE WITH GULF OF MAINE LANDINGS
# 201 columns of species
# Years from 1970-2012
# ...but just one "subarea"
source('NeuralNetCast01.1.R')
data <- read.csv('~/Work/Data/LandingsR.csv') 
data[is.na(data)] <- 0 # I'm assuming NA means landings were 0
im <- arrangeImages(data = data[1,2:ncol(data)],
                    times = data$X,
                    areas = 0*data$X,
                    labelcol = data$COD..ATLANTIC,
                    ntimes = 5)
# Remove NAs (actually don't need to here)
I <- which(!is.na(im$labels))
im$images <- im$images[I,]
im$labels <- im$labels[I]
im$labelarea <- im$labelarea[I]
im$labeltime <- im$labeltime[I]

nquants <- 5
im$labelsorig <- im$labels
im$labels <- as.matrix(as.numeric(cut(im$labelsorig,
                                      breaks=quantile(im$labelsorig, probs=seq(0,1, by=1/nquants)),
                                      include.lowest=TRUE,
                                      labels = 1:nquants))) - 1
# Divide into training and testing indices
Itrain <- 1:floor(dim(im$labels)[1]*.5)
Itest <- (floor(dim(im$labels)[1]*.5)+1):dim(im$labels)[1]

network<-netForecast(trainimages=im$images[Itrain,],
                     trainlabels=im$labels[Itrain],
                     testimages=im$images[Itest,],
                     testlabels=im$labels[Itest],
                     nepochs=20,
                     nunits = 64)

predictions <- netPlot(network=network,
                       testimages=im$images[Itest,],
                       testlabels=im$labels[Itest])


# EXAMPLE WITH LOOP
source('NeuralNetCast01.1.R')
data <- read.csv('../Data/CWM_yearsub.csv')
im <- arrangeImages(data = data[,4:11],
                    times = data$Year,
                    areas = data$Subarea,
                    labelcol = data$Trophic.level,
                    ntimes = 5)
im$labelsorig <- im$labels

ACC <- matrix(NA,nrow=10)
# Loop through number of quantiles
# Convert labels to quantiles
for (nquants in 3:10)
{

im$labels <- as.matrix(as.numeric(cut(im$labelsorig,
                                      breaks=quantile(im$labelsorig, probs=seq(0,1, by=1/nquants)),
                                      include.lowest=TRUE,
                                      labels = 1:nquants))) - 1


# Divide into training and testing indices
Itrain <- 1:floor(dim(im$labels)[1]*.8)
Itest <- (floor(dim(im$labels)[1]*.8)+1):dim(im$labels)[1]

network<-netForecast(trainimages=im$images[Itrain,],
                     trainlabels=im$labels[Itrain],
                     testimages=im$images[Itest,],
                     testlabels=im$labels[Itest],
                     nepochs=20,
                     nunits = 512)

#metrics <- network %>% evaluate(im$images[Itest,], im$labels[Itest])
ACC[nquants] <- metrics$accuracy # not getting metrics right yet
}
