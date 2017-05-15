
## This function is going to take an existing matrix
## and convert it into a special matrix enclosure
## where the x and the inv are seperated from the 
## global environment. As a result, they can store
## values for an extended period of time. The extended
## storage means you can cache values inside the function
## and recall them later without having to compute them.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function() inv <<- solve(x)
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function is just an additional
## layer to the Solve() function.
## It accesses the cached matrix
## and tests to see if the getinverse
## is null or has a value. If it has a value
## it returns the value otherwise it performs
## the computation solve. Once the computation
## has completed it will then store the inverse
## from the solve function in our Cached matrix from above

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("Retrieving cached data.")
    return(inv)
  }
  theMatrix <- x$get()
  inv <- solve(theMatrix, ...)
  x$setInverse(inv)
  inv
}
