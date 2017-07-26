## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve:This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache


makeCacheMatrix <- function(x = matrix()) {
        cache <- NULL
        setMatrix <- function(newValue){
                x <<- newValue
                cache <<- NULL
        }
        getMatrix <- function() x
        setInverse <- function(solve) cache <<- solve
        getInverse <- function() cache
        list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- y$getInverse()
        ## Check for a cached value
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        ## Calculate inverse
        data <- y$getMatrix()
        inverse <- solve(data)
        y$setInverse(inverse)
        ## Value return
        inverse
}
