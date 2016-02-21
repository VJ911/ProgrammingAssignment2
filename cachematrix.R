# ASSUMPTION: the matrix supplied is always invertible
# TASK: adapt demonstrated caching functionality but instead apply solve() to Matrix input
# SOLUTION: replace all references of "mean" with "solve"

# The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
#   1. set the value of the matrix
#   2. get the value of the matrix
#   3. set the value of the inverse matrix
#   4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


# This second function calculates the inverse of the special "matrix" created with the above function.
# However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse 
# from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

#This third function demonstrates another way to attack the same problem using only just one function
customCache <- function(x = matrix()) {
  
  if (exists("cachedResult")){        #check that we have run this function before
    if (identical(x, cachedInput)){   #check the input operand hasn't changed from last time
      message("getting cached data")
      return(cachedResult)            
    }
  }
  message("new operand, recalculating")
  cachedInput <<- x
  cachedResult <<- solve(x)
  return(cachedResult)
}

# Testing the requested solution:
test <- matrix(1:4,nrow=2,ncol=2)
testC <- makeCacheMatrix(test)

message("inital run:")
result <-cacheSolve(testC)
print(result)

message("second run:")
result <-cacheSolve(testC)
print(result)
