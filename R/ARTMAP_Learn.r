#' Trains an ARTMAP network on the given input data.
#' 
#' This function trains an ARTMAP network on the given input data. Each sample
#' of the data is presented to the network, which categorizes each sample
#' and compares that category's entry in the map field to the supervisor signal.
#' If the map field value and the supervisor signal do not match, match-tracking
#' is induced until a category is found to code the input correctly. This
#' category then learns the input vector.
#' 
#' The function returns a new ARTMAP network which has learned the input data
#' according to the supervisor signal. If the maximum number of categories 
#' is reached and an appropriate categorization of the input cannot be made,
#' no learning occurs. The program prints out a warning message that the 
#' maximum category limit has been reached and begins to process the next
#' input vector.
#' @title ARTMAP_Learn
#' @param artmap_network ARTMAP network to be trained It should be created with ARTMAP_Create_Network().
#' @param data Training data to be presented to the network. It is a matrix of size NumFeatures-by-NumSamples.
#' @param supervisor the correct classification for each input vector. It is a matrix of size 1-by-NumSamples.
#' @return New ARTMAP network which has learned the input data.
#' @export
ARTMAP_Learn=function(artmap_network, data, supervisor)
{
  # Make sure the user specifies the input parameters.
  if(nargs() != 3)
  {
    stop('You must specify all 3 input parameters.');
  }
  
  # Make sure that the data is appropriate for the given network.
  numFeatures = ncol(data);
  numSamples = nrow(data);
  if(numFeatures != artmap_network$numFeatures)
  {
    stop('The data does not contain the same number of features as the network.');
  }
  
  # Make sure the vigilance is within the (0, 1] range.
  if((artmap_network$vigilance <= 0) | (artmap_network$vigilance > 1))
  {
   stop('The vigilance must be within the range (0, 1].');
  }
  
  # Make sure that the number of epochs is a positive integer.
  if(artmap_network$numEpochs < 1)
  {
   stop('The number of epochs must be a positive integer.');
  }
  
  # Go through the data once for every epoch.
  for (epochNumber in 1:artmap_network$numEpochs)
  {
    # This variable will allow us to keep up with the total
    # network change due to learning.
    # Initialize the number of changes to 0.
    numChanges = 0;
    
    # Classify and learn on each sample.
    for (sampleNumber in 1:numSamples)
    {  
      # Get the current data sample.
      currentData = data[sampleNumber,];
      
      # Get the current supervisory signal.
      currentSupervisor = supervisor[sampleNumber,1];
      
      # Create a new category if this supervisory signal
      # has never been seen before.
      if((length(artmap_network$mapField)==0) | (length(which(artmap_network$mapField == currentSupervisor)))==0)
      {
        auxList = ARTMAP_Add_New_Category(artmap_network$weight, artmap_network$mapField);
        resizedWeight=auxList[[1]];
        resizedMapField=auxList[[2]];
        auxList = ART_Update_Weights(currentData, resizedWeight, length(resizedMapField), artmap_network$learningRate);
        resizedWeight = auxList[[1]];
        weightChange = auxList[[2]];
        artmap_network$weight = resizedWeight;
        artmap_network$numCategories = artmap_network$numCategories + 1;
        resizedMapField[length(resizedMapField),1] = currentSupervisor;
        artmap_network$mapField = resizedMapField;
        numChanges = numChanges + 1;
        next;    
      }
      else
      {    
        # Activate the categories for this sample.
        bias = artmap_network$bias;
        categoryActivation = ART_Activate_Categories(currentData, artmap_network$weight, bias);
        
        # Rank the activations in order from highest to lowest.
        # This will allow us easier access to step through the categories.
        auxList = sort(-categoryActivation,index.return=TRUE);
        sortedActivations = auxList[[1]];
        sortedCategories = auxList[[2]];
        
        # Go through the process of locating the highest activated category
        # with the correct value in the map field for the supervisory signal.
        matchTracking = 1;
        vigilance = artmap_network$vigilance;
        
        # Go through each category in the sorted list looking for the best match.
        resonance = 0;
        match = 0;
        numSortedCategories = length(sortedCategories);
        currentSortedIndex = 1;
        while(!resonance)
        {    
          # Get the current category based on the sorted index.
          currentCategory = sortedCategories[currentSortedIndex];
          
          # Get the current weight vector from the sorted category list.
          currentWeightVector = artmap_network$weight[currentCategory,];
          
          # Calculate the match given the current data sample and weight vector.
          match = ART_Calculate_Match(currentData, currentWeightVector);
          
          # Check to see if the match is less than the vigilance.
          if(match < vigilance)
          {
            # If so, choose the next category in the sorted category list.
            # If the current category is the last in the list, make sure that
            # the maximum number of categories have not been reached. If the 
            # maximum has not been reached, create a new category for the input, 
            # update the weights, and induce resonance.
            if(currentSortedIndex == numSortedCategories)
            {
              if(currentSortedIndex == artmap_network$maxNumCategories)
              {
                print('WARNING: The maximum number of categories has been reached.\n');
                resonance = 1;
              }
              else
              {
                auxList = ARTMAP_Add_New_Category(artmap_network$weight, artmap_network$mapField);
                resizedWeight = auxList[[1]];
                resizedMapField = auxList[[2]];
                auxList = ART_Update_Weights(currentData, resizedWeight, currentSortedIndex + 1, artmap_network$learningRate);
                resizedWeight = auxList[[1]];
                weightChange = auxList[[2]];
                artmap_network$weight = resizedWeight;
                artmap_network$numCategories = artmap_network$numCategories + 1;
                resizedMapField[currentSortedIndex + 1,1] = currentSupervisor;
                artmap_network$mapField = resizedMapField;
                
                # Increment the number of changes since we added a new category.
                numChanges = numChanges + 1;            
                resonance = 1;
              }
            }
            else
            {
              currentSortedIndex = currentSortedIndex + 1;
            }  
          }
          else 
          {
            # Otherwise, check the category's value in the map field.
            if(artmap_network$mapField[currentCategory,1] == currentSupervisor)
            {
              # If they're equal, the category should code the input.
              # Therefore, we should update the weights and induce resonance.
              auxList = ART_Update_Weights(currentData, artmap_network$weight, currentCategory, artmap_network$learningRate);
              artmap_network$weight = auxList[[1]];
              weightChange = auxList[[2]];
              if(weightChange == 1)
              {
                numChanges = numChanges + 1;
              }          
              resonance = 1;
            }
            else
            {
              # If they're not equal, we must find another category by
              # inducing match-tracking. This means that we should increase
              # the vigilance by the current match plus epsilon and then 
              # continue through the category list until another match is
              # found.
              vigilance = match + 0.000001;
              if(currentSortedIndex == numSortedCategories)
              {
                if(currentSortedIndex == artmap_network$maxNumCategories)
                {            
                  print('WARNING: The maximum number of categories has been reached.\n');
                  resonance = 1;
                }          
                else
                {
                  auxList = ARTMAP_Add_New_Category(artmap_network$weight, artmap_network$mapField);
                  resizedWeight = auxList[[1]];
                  resizedMapField = auxList[[2]];
                  auxList = ART_Update_Weights(currentData, resizedWeight, currentSortedIndex + 1, artmap_network$learningRate);
                  resizedWeight = auxList[[1]];
                  weightChange = auxList[[2]];
                  artmap_network$weight = resizedWeight;
                  artmap_network$numCategories = artmap_network$numCategories + 1;
                  resizedMapField[currentSortedIndex + 1,1] = currentSupervisor;
                  artmap_network$mapField = resizedMapField;
                
                  # Increment the number of changes since we added a new category.
                  numChanges = numChanges + 1;
                  
                  resonance = 1;
                }
              }
              else
              {
                currentSortedIndex = currentSortedIndex + 1;
                resonance = 0;
              }
            }
          }
        }
      }
    }
  
    # If the network didn't change at all during the last epoch,
    # then we've reached equilibrium. Thus, we can stop training.
    if(numChanges == 0)
    {
      break;
    }
  }
  artmap_network$neededEpochs=epochNumber
  print(sprintf('The number of epochs needed was %d\n', epochNumber));
  
  # Fill the new network with the appropriate values.
  new_artmap_network = artmap_network;
  
  return (new_artmap_network);
}