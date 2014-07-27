## This program presents 2 functions that work to reduce the
## time of computing the inverse of a matrix which is time
## consuming. Reducing the time is done via cashing; if the
## content of the matrix did not change, there is no need to
## compute the inverse from scratch but an older cashed value
## is returned.


## makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # The value of the inverse is set to NULL
  set <- function(y){
    x <<- y # <<- is used to access x in higher environment
    inv <<- NULL # Inverse is still NULL (not calculated yet)
  }
  get <- function() x #function to return matrix x
  setInverse <- function(inverse) inv<<- inverse 
  getInverse <- function() inv
  list (set=set, get=get, setInverse=setInverse, 
        getInverse=getInverse)

}


## cacheSolve is a function that will calculate the 
## inverse of the matrix using R's built in Solve()
## function and saves the results, in case the matrix 
## inverse was already calculated, it will use the
## cached (saved) value for the inverse. 

cacheSolve <- function(x, ...) {
  inv <- x$getInverse() #function to get the cashed inverse 
  if(!is.null(inv)){ # if its is found:
    message("getting cashed data")
    return(inv) #cashed inverse is returned here
  }
  data <- x$get() #else, the matrix data is fetched
  inv <- solve(data, ...) #inverse is calculated by Solve()
  x$setInverse(inv) #new inverse is cashed
  inv # Return a matrix that is the inverse of 'x'
}

