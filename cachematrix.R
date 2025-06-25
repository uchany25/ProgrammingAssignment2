makeCacheMatrix <- function(x = matrix()) {
  # m will store the cached inverse of the matrix x.
  m <- NULL
  
  # set: Function to set a new matrix and invalidate the inverse cache.
  # When a new matrix 'y' is provided, the existing matrix 'x' is updated
  # (using '<<-' for superassignment to modify 'x' in the parent environment).
  # The cached inverse 'm' is then reset to NULL, forcing re-computation
  # if the inverse is needed again for the new matrix.
  set <- function(y) {
    x <<- y       # Update the matrix
    m <<- NULL    # Invalidate the cached inverse
  }
  
  # get: Function to retrieve the current matrix 'x'.
  get <- function() x
  
  # setinverse: Function to store a calculated inverse 'inverse' into the cache 'm'.
  # This is typically called by cacheSolve after computing the inverse.
  setinverse <- function(inverse) m <<- inverse
  
  # getinverse: Function to retrieve the cached inverse 'm'.
  # If no inverse has been cached or the matrix has changed, this will return NULL.
  getinverse <- function() m
  
  # Return a list of these functions. This list acts as the interface
  # for the "special matrix" object, allowing interaction with 'x' and 'm'.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  # Get the cached inverse from the special matrix object 'x'.
  # 'x$getinverse()' calls the getinverse function from the list returned by makeCacheMatrix.
  m <- x$getinverse()
  
  # Check if the inverse is already cached (i.e., m is not NULL).
  if (!is.null(m)) {
    # If it's cached, print a message and return the cached inverse immediately.
    message("getting cached data")
    return(m)
  }
  
  # If the inverse is not cached (cache miss):
  # 1. Get the matrix itself from the special object 'x'.
  #    'x$get()' calls the get function from the list.
  data <- x$get()
  
  # 2. Compute the inverse of the matrix using R's 'solve()' function.
  #    This is the potentially costly computation we want to cache.
  m <- solve(data, ...) # The '...' allows passing additional arguments to solve() if needed.
  
  # 3. Cache the newly computed inverse using the 'setinverse' function of the object.
  #    'x$setinverse(m)' stores the result for future use.
  x$setinverse(m)
  
  # 4. Return the computed inverse.
  return(m)
}
