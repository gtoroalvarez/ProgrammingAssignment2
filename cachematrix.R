## I created the makeCacheMatrix based on the exaple given for the vector - used a matrix and inverse function instead. 
##to make it run I used the following input
## testmatrix <-makeCacheMatrix()  to store the function in a matrix variable
## testmatrix$set(matrix(1:4,2,2)) to create a square matrix
##I could have done  testmatrix <- makeCacheMatrix(matrix(1:4,2,2)) instead but I rather dis it in 2 steps
## then run cacheSolve(testmatrix)

## Write a short comment describing this function
## x is a square invertible matrix - per assignment assumption

makeCacheMatrix <- function(x = matrix()) {
 m = NULL
  set = function(y) {
    x <<- y
    m <<- NULL
    ## I used <<- operator to assign the value in an environment that is different from the current environment
    ## as described on assignment instructions
  }
  get = function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
   # if the inverse is already calculated, use cache
  if(!is.null(m)){
    message("getting cached data")
    return(m)  
  }
  ## ELSE, calculate it
  data <- x$get()
  m <- solve(data, ...)
  ## since  X is a square invertible matrix, I used solve(X) to calculate inverse
  x$setinverse(m)
  return(m)
        ## Return a matrix that is the inverse of 'x'
}
