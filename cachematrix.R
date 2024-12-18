## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse cache to NULL
  
  # Function to set the value of the matrix
  set <- function(y) {
    x <<- y  # Use <<- to assign value to x in the parent environment
    inv <<- NULL  # Reset the inverse cache to NULL
  }
  
  # Function to get the value of the matrix
  get <- function() x
  
  # Function to set the value of the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  # Function to get the value of the inverse
  getinverse <- function() inv
  
  # Return a list of the four functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# Function to compute the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()  # Get the cached inverse
  
  # If the inverse is already cached, return the cached value
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If the inverse is not cached, calculate it
  data <- x$get()  # Get the matrix data
  inv <- solve(data, ...)  # Calculate the inverse
  x$setinverse(inv)  # Cache the calculated inverse
  inv  # Return the inverse
}

# Create a special "matrix" object
special_matrix <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2))

# Compute and cache the inverse
cacheSolve(special_matrix)  # This will calculate and cache the inverse

# Retrieve the cached inverse
cacheSolve(special_matrix)  # This will use the cached inverse and print "getting cached data"
