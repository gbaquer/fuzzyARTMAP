#' Activates the categories in an ART network
#' 
#' This function returns a vector of category activations for the given 
#' input vector, weight matrix, and bias value
#' @title ART_Activate_Categories
#' @param input A vector of size NumFeatures that contains the input signal into the network
#' @param weight Matrix of size NumFeatures-by-NumCategories which holds the weights of the network
#' @param bias Constant that is used to differentiate between very similar category activation values
#' @return Vector of size NumCategories that holds the activation value for each category
#' @details The length of the INPUT vector must equal the number of rows in the WEIGHT matrix, 
#' and the BIAS value must be within the range [0, 1] (although values very near 0 are best).

ART_Activate_Categories = function(input, weight, bias)
{
  
# Make sure the user supplied the required parameters.
if(nargs() != 3)
{
  stop('You must specify the 3 input parameters.');
}

# Check the size and range of the parameters.
numFeatures=nrow(weight);
numCategories= ncol(weight);

if(length(input)!=numFeatures)
{
  stop('The length of the input and rows of the weights do not match.');
}
if((bias < 0) | (bias > 1))
{
  stop('The bias must be within the range [0, 1].');
}

# Set up the return variable.
categoryActivation = matrix(1,1,numCategories);

# Calculate the activation for each category.
# This is done according to the following equation:
#        Activation(j) = |Input^Weight(j)| / (bias + |Weight(j)|)
for (j in 1:numCategories)  
{
  matchVector = pmin(input, weight[, j]);
  weightLength = sum(weight[, j]);
  categoryActivation[1, j] = sum(matchVector) / (bias + weightLength);
}

return (categoryActivation);
}