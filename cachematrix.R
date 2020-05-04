## Function makeCacheMatrix takes an invertible matrix as an argument and creates
## a special "vector", which is a list containing 4 functions below:
##
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
    ivs <- NULL
    set <- function(y) {
      x <<- y
      ivs <<- NULL
    }
    get <- function() x
    setivs <- function(invers) ivs <<- invers
    getivs <- function() ivs
    list(set = set, get = get,
         setivs = setivs,
         getivs = getivs)

}


## Function cacheSolve checks if an inverse of the matrix has been calculated.
## If yes, get it from the cache. Else, calculates the inverse of the matrix 
## created with the above function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ivs <- x$getivs()
    if(!is.null(ivs)) {
      message("getting cached data")
      return(ivs)
    }
    data <- x$get()
    ivs <- solve(data, ...)
    x$setivs(ivs)
    ivs
}
