#' Calculates the match value of an input to a category.
#' 
#' This function returns a value which represents the amount of match
#' between the given input and the given category.
#' @title ART_Calculate_Match
#' @param input Vector of size NumFeatures that contains the input signal into the network
#' @param weightVector Matrix of size NumFeatures which holds the weights of the network for a given category
#' @return Measure of the degree of match between the input and the current category
#' @details The length of the INPUT vector must equal the length of the WEIGHTVECTOR.
#' 
ART_Calculate_Match = function(input, weightVector)
{
  # Initialize the local variables.
  match = 0;
  numFeatures = length(input);
  
  # Make sure the weight vector is appropriate for the input.
  if(numFeatures != length(weightVector))
  {
    stop('The input and weight vector lengths do not match.');
  }
  
  # Calculate the match between the given input and weight vector.
  # This is done according to the following equation:
  #       Match = |Input^WeightVector| / |Input|
  matchVector = pmin(input, weightVector);
  inputLength = sum(input);
  if(inputLength == 0)
  {
    match = 0;
  }
  else
  {
    match = sum(matchVector) / inputLength;
  }
  return (match);
}