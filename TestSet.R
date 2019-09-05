#******************************************************
#
#   	Test Set
#
#******************************************************

#Create test and training sets 
# Arguments
# data		a data frame 
# size		the fraction of data for the test set

# Value
# A list in which the first element is the test set 
# the second element is the training set.

test.set <- function(data, size)
{
  i <- sample(1:nrow(data), round(size*nrow(data)))
  j <- setdiff(1:nrow(data), i)
  list(test =data[i,], train = data[j,])
}


mse <- function(p, r)
{
  ((p-r)%*%(p-r))/length(p)
  
}