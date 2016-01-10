#' Creates a new ARTMAP network.
#' 
#' This function creates a new ARTMAP network with the specified number
#' features and classes. The network is created to expand the number of 
#' categories as needed. The vigilance parameter defaults to 0.75.
#' The initial number of categories is set to 1. The maximum number
#' of categories defaults to 100. The bias defaults to 0.000001, the
#' number of epochs defaults to 100, and the learning rate defaults 
#' to 1.0 (fast-learning).
#' @title ARTMAP_Create_Network
#' @param numFeatures Number of features that the network expects of the input data. Must be a positive integer.
#' @param numClasses Number of classes that exist for the supervisory signal. Must be a positive integer greater than 1.
#' @param maxNumCategories Maximum number of categories that can be activated during the training process. Defaults to 1000.
#' @param vigilance Vigilance parameter that defines the minimum similarity allowed between the input pattern and the weights. Defaults to 0.75.
#' @param bias Constant that is used to differentiate between very similar category activation values. Defaults to 0.000001
#' @param numEpochs Maximum number of training iterations allowed. Defaults to 100.
#' @param learningRate Learning rate. Defaults to 1.
#' @return Structure that holds all of the information for the network. It must be passed into both ARTMAP_LEARN() and 
#' ARTMAP_CLASSIFY(). The fields of this structure are numFeatures, numCategories, maxNumCategories, numClasses, weight 
#' (an initialized weight vector with the right dimensions), mapField (an initialized map field with the right dimensions), 
#' vigilance, bias, numEpochs, neededEpochs (the number of epochs needed in the last training) and learningRate.
#' @export
ARTMAP_Create_Network=function(numFeatures, numClasses,
                               maxNumCategories = 1000, vigilance=0.75, 
                                bias=0.000001, numEpochs=100, learningRate=1.0)
{
if(is.null(numFeatures) || is.null(numClasses))
{
  stop('You must specify the number of features and the number of classes.');
}

# Check the ranges of the input parameters.
numFeatures = round(numFeatures);
if(numFeatures < 1)
{
  stop('The number of features must be a positive integer.');
}
numClasses = round(numClasses);
if(numClasses < 2)
{
  stop('The number of classes must be a positive integer greater than 1.');
}

# Create and initialize the weight matrix.
weight = matrix(1,numFeatures,0);

# Create and initialize the map field.
mapField = matrix(0,1,0);

# Create the structure and return.
artmap_network = list("numFeatures"= numFeatures, "numCategories" = 0, "maxNumCategories" = maxNumCategories, 
                        "numClasses"=numClasses, "weight"=weight, "mapField"=mapField, 
                        "vigilance"=vigilance, "bias"=bias, "numEpochs"=numEpochs, 
                        "neededEpochs"=0,"learningRate"=learningRate);

return(artmap_network);
}