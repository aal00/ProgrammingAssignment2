## Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverseCache <- NULL  # This will store the cached inverse

  # Setter function to update the matrix and clear any previously cached inverse
  set <- function(y) {
    x <<- y
    inverseCache <<- NULL  # Clear cache because the matrix has changed
  }

  # Getter function to retrieve the original matrix
  get <- function() x

  # Setter function to store the inverse in the cache
  setInverse <- function(inverse) inverseCache <<- inverse

  # Getter function to retrieve the cached inverse
  getInverse <- function() inverseCache

  # Return a list of the above four functions to interact with the object
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Function to compute the inverse of the special matrix returned by makeCacheMatrix
## If the inverse is already cached, it retrieves it instead of computing again
cacheSolve <- function(x, ...) {
  # Attempt to get the cached inverse
  inverseCache <- x$getInverse()

  # If cached inverse exists, return it to avoid recomputation
  if (!is.null(inverseCache)) {
    message("getting cached data")
    return(inverseCache)
  }

  # If not cached, retrieve the matrix and compute the inverse
  matrixData <- x$get()
  inverseCache <- solve(matrixData, ...)  # Compute the inverse using solve()

  # Cache the newly computed inverse for future use
  x$setInverse(inverseCache)

  # Return the computed inverse
  inverseCache
}
