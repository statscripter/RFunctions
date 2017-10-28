name = 'Michael Archibeque'

############################ Header Comments #############################
#
# This function performs 1-D discrete convolution of two vectors, x and y,
# applicable to discrete probability and signal processing
#
# Function:
#
# conv()
#
# Input parameters:
#
# x, a numeric vector of length m where m > 1
# y, a numeric vector of length n where n > 1
#
# Output:
# 
# one numeric vector of length m + n - 1
#
# Syntax:
#
# conv(x,y)
#
# Example:
#
# Discrete Probability Distribution Application:
# The Distribution of the Sum of Two IID Bernoulli RV, p = 0.6
#
# x <- c(0.4,0.6)
# y <- c(0.4,0.6)
#
# conv(x,y)
# [1] 0.16 0.48 0.36
#
# dbinom(0:2,2,0.6)
# [1] 0.16 0.48 0.36
#
# Demonstration of the properties of the function:
#
# w <- c(1,2,1,3)
# c <- c(2,0,1)
#
# conv(c(1,2,1,3),c(2,0,1))
# [1] 2 4 3 8 1 3
#
# conv(c(2,0,1),c(1,2,1,3))
# [1] 2 4 3 8 1 3
#
# conv(w,c)
# [1] 2 4 3 8 1 3
#
# conv(c,w)
# [1] 2 4 3 8 1 3
#
# h <- conv(w,c)
# print(h)
# [1] 2 4 3 8 1 3
#
# is.vector(h)
# [1] TRUE
#
# Note: The output will be the same regardless of which vector is
# assigned to which parameter. Thus, for those using this function
# for signal processing, either x or y can be the kernel while the
# remaining parameter will be the input image.
#######################################################################

conv <- function(x,y){
  
  # I assign the parameters of the function to private variables
  # within the function. I do this because this is what I learned
  # to do in Java and C++ when creating objects because it is more 
  # secure. However, I recognize an R function is not precisely an 
  # object in the same sense. In future functions I will not do this, 
  # but I already wrote the function before I saw your comments on
  # my previous projects, and I do not want to ruin something that
  # already works in order to save a little bit of memory.
  
  vectorx <- x
  vectory <- y
  
  # Testing if the first parameter, x, is a numeric vector
  
  if((is.numeric(vectorx)) == FALSE ||
     (is.vector(vectorx)) == FALSE ||
     (length(vectorx)) < 2){
    
    return("The first input parameter, x, should be a numeric vector.")
  }
  
  # Testing if the second parameter, y, is a numeric vector
  
  if((is.numeric(vectory)) == FALSE ||
     (is.vector(vectory)) == FALSE ||
     (length(vectory)) < 2){
    return("The second input parameter, y, should be a numeric vector.")
  }
  
  # Reverse the order of the elements of the y parameter vector 
  # for use within the convolution algorithm
  
  revvectory <- rev(vectory)
  
  # Create variables containing the length of the parameters, which
  # will be later used in the For Loop that will execute the convolution
  # vector calculation algorithm
  
  lengthx <- length(vectorx)
  lengthrevy <- length(revvectory)
  convlength <- lengthx + lengthrevy - 1
  
  # Create the vector where the elements of the convolution vector will
  # be stored
  
  convolution <- numeric(convlength)
  
  
  # This For Loop creates a series of two vectors of equal length based
  # on the x and reverse y vectors but with zeros on either side of the 
  # elements of the original vectors; the zeros are generated so that 
  # two vectors are of equal size but the elements within the vector based
  # on the reverse y vector shift so the correct arithmetic can be 
  # performed on the elements of the two vectors
  
  for(i in 1:convlength){
    
    xmultipliervector <- c(0,
                           rep(0,convlength-1),
                           rep(0,(lengthrevy)),
                           vectorx,
                           rep(0,convlength+1))
    
    ymultiplervector <- c(rep(0,i),
                          rep(0,convlength),
                          revvectory, 
                          rep(0,(lengthx)),
                          rep(0,convlength-i),
                          0)
    
    # The two vectors based on the x and reverse y vectors are multipled
    # by element, then summed. The result is stored in an element of the
    # convolution vector
    
    convolution[i] <- sum(xmultipliervector*ymultiplervector)
    
  }
  
  # The convoltuion vector is returned
  
  return(convolution)   
  
}