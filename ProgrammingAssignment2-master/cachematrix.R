## Put comments here that give an overall description of what your
## functions do
  ## These functions are written part of Coursera R Programming Assignment : Week 3
  ##

## Write a short comment describing this function
  ## This function creates special matrix object to cache its inverse
makeCacheMatrix <- function(x = matrix()) { ## defining argument with default "matrix"
      inv <- NULL                           ## initialise inv as NULL and will hold value of matrix inv
      set <- function(y){                   ## defining set function to assign new
          x <<- y                           ## To consider value from parent environment
          inv <<- NULL                      ## For new matrix,reset inv to NULL
      }
      get <- function ()x                   ## defining get function to return value of the matrix argument
      setinverse <- function(inverse) inv <<- inverse ## assigning value of inv in parent environment
      getinverse <- function ()inv          ## gets the value of inv where called
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) ## we need this to refer functions with $ operator 
}

## Write a short comment describing this function
## This function computes inverse of "matrix" returned by makeCacheMatrix
## If inverse has already been calculated (matrix remains same)
## then cacheSolve function will retrive the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  }
