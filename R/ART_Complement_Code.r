#' Complement codes the data for use with an ART network.
#' 
#' This function complement codes the given data where the complement of x is 1-x.
#' @title ART_Complement_Code
#' @param data Matrix of size NumFeatures-by-NumSamples that holds the data to be complement coded.
#' @return Data that has been complement coded. It is a matrix of size 2*NumFeatures-by-NumSamples.
#' @export
ART_Complement_Code = function(data)
{
# ART_Complement_Code    Complement codes the data for use with an ART network.
#    COMPLEMENTCODEDDATA = ART_Complement_Code(DATA)
#    This function complement codes the given data where the complement
#    of x is 1-x. For example, suppose the data were as follows:
  #           data = 0.3  0.2  0.6
#                  0.4  0.1  0.8
#    Then the complement coding would be as follows:
  #           complementCodedData = 0.3  0.2  0.6
#                                 0.7  0.8  0.4
#                                 0.4  0.1  0.8
#                                 0.6  0.9  0.2
# 
#    The input parameters are as follows:
  #    The DATA is a matrix of size NumFeatures-by-NumSamples that holds
#    the data to be complement coded. The complement coding works on
#    each column (i.e., each sample).
#
#    The return parameter is as follows:
  #    The COMPLEMENTCODEDDATA is the data that has been complement coded.
#    It is a matrix of size 2*NumFeatures-by-NumSamples.


# Determine the size of the data.
numFeatures=nrow(data);
numSamples=ncol(data);

print(numFeatures);
print(numSamples);
# Create the return variable.
complementCodedData = matrix(1,2*numFeatures, numSamples);

# Do the complement coding for each sample.
for (j in 1:numSamples)
{
  count = 1;
  for (i in seq(1,(2*numFeatures),2))
  {
    complementCodedData[i, j] = data[count, j];
    complementCodedData[i + 1, j] = 1 - data[count, j];
    count = count + 1;
  }
}

return (complementCodedData);
}