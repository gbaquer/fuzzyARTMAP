#' Complement codes the data for use with an ART network.
#' 
#' This function complement codes the given data where the complement of x is 1-x.
#' @title ART_Complement_Code
#' @param data Matrix of size NumFeatures-by-NumSamples that holds the data to be complement coded.
#' @return Data that has been complement coded. It is a matrix of size 2*NumFeatures-by-NumSamples.
#' @export
ART_Complement_Code = function(data)
{
  # Determine the size of the data.
  numFeatures=ncol(data);
  numSamples=nrow(data);
  
  # Create the return variable.
  complementCodedData = matrix(1,numSamples,2*numFeatures);
  
  # Do the complement coding for each sample.
  for (j in 1:numSamples)
  {
    count = 1;
    for (i in seq(1,(2*numFeatures),2))
    {
      complementCodedData[j,i] = data[j,count];
      complementCodedData[j,i + 1] = 1 - data[j,count];
      count = count + 1;
    }
  }
  
  return (complementCodedData);
}