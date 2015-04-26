##These functions will accept an argument containing values as input to a matrix,
##create a cache of the inverse of the matrix if matrix values are new, 
##or return an existing cache of the inverse of the matrix if matrix matches cache.

makeCacheMatrix <- function(x = matrix()){ ##function that will cache inverse
  
  ##Assignment instructions state to consider any matrix as invertible
  ##However, to help avoid getting an error, a square matrix is created
  if(length(x)>=4){
    my_matrix <- matrix(x, nrow=2, ncol=2) ##put values of x into matrix
  } else {
      my_matrix <- matrix(x, nrow=1, ncol=1) ##put values of x into matrix
    }
  
  inv_my_matrix <- cacheSolve(my_matrix) ##get inverse
  
  cachedMatrix <<- x ##store current matrix for comparison next time
  cachedInvMatrix <<- inv_my_matrix ##store current inverse for next time
  
  cachedInvMatrix ##return inverse  
  
}



cacheSolve <- function(x, ...){ ##check for cached inverse/return inverse
  ## Set variables
  getCachedMatrix <- NULL
  getCachedInvMatrix <- NULL
  
  if(exists("cachedMatrix")){  ##check to see if matrix variable exists
    getCachedMatrix <- cachedMatrix ##if exists, get last cached matrix
  } else {
    cachedMatrix <- NULL ##if does not exists, set variable
    }
  
  if(exists("cachedInvMatrix")){ ##check to see if inverse variable exists
    getCachedInvMatrix <- cachedInvMatrix ##if exists, get cached inverted matrix
  } else {
    cachedInvMatrix <- NULL ##if does not exists, set variable
    }
  
  if(is.null(getCachedInvMatrix)){ ##check to see if an inverse is cached
    ret_inv_my_matrix <- solve(x) ##if no inverse is cached, create inverse
    
  } else{ ##if an inverse is cached...
    if(identical(getCachedMatrix, x)){ ##check to see if matrix has changed
      ret_inv_my_matrix = getCachedInvMatrix ##if matrices are the same, return cached inverse
    } else{
      ret_inv_my_matrix <- solve(x) ##if matrices are not the same, create inverse
      
      }
    }
  
  ret_inv_my_matrix ##return inverse
}