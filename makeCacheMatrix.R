## Put comments here that give an overall description of what your functions do
## Note: I'm assuming that we need a matrix object such as the vector object of the 
## example. So, there's no need of too many changes on the code. 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## We create a sort of matrix object with `makeCacheMatrix`, which has information 
  ## about the matrix and it's inverse along with set/get operators for each one 
  
}

## `makeCacheMatrix`: receives a matrix (or not) and sets it's inverse as NULL.
## It returns a list of 4 functions with the 4 operators: 
## 1. `set`: Changes the value of the matrix and erases the old inverse value
## 2. `get`: Returns the matrix
## 3. `setinverse`: Sets the value of the inverse matrix
## 4. `getinverse`: Returns the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  #Initial value of the inverse
  inverse <- NULL
  #1. Set
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  #2. Get
  get <- function() x
  #3. Set inverse
  setinverse <- function(inv) {
    inverse <<- inv
  }
  #4. Get inverse
  getinverse <- function() inverse
  #Returns the list
  list(set = set, get = get, setinverse = setinverse,getinverse = getinverse)
}

## Write a short comment describing this function

## `cache Solve`: receives a cache matrix object (result of the previous function) and
## tries to get the inverse matrix. If the matrix object has information of its inverse
## the function returns it. Otherwise, the function gets the main matrix and calculates
## its inverse and then sets that value on the cache matrix object. 
## Finally, in this case, the function returns the inverse matrix.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #Inverse of the object
  inverse <- x$getinverse()
  #If it's not null, just return that inverse
  if(!is.null(inverse)) {
    return(inverse)
  }
  #Else
  data <- x$get()
  inverse <- solve(data,...)
  x$setinverse(inverse)
  inverse
}
m <- matrix(rnorm(16),4,4)
m1 <- makeCacheMatrix(m)
cacheSolve(m1)