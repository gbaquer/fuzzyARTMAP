library(mlbench)

generate_circle_in_the_square<-function(nExemplars=100){
  #Generate input and supervisory data
  trainingDataset = mlbench.circle(nExemplars,2)
  input = trainingDataset$x
  
  #Shift and scale the axis so all values input_j are in [0,1]
  input=t(input+matrix(1,dim(input)[1],dim(input)[2]))/2
  sup = t(matrix(as.integer(trainingDataset$classes)-1))
  
  #Sort input and supervisory data in the same random order
  rand <- sample(nExemplars)
  input = input[,rand]
  sup = t(matrix(sup[,rand]))
  
  return(t(rbind(input,sup)))
}



#Store Training .csv
training_data<-generate_circle_in_the_square(200)
write.csv(training_data,"circle_in_the_square_training.csv")


#Store Operation .csv
operation_data<-generate_circle_in_the_square(20)
write.csv(operation_data[,-3],"circle_in_the_square_operation.csv")
