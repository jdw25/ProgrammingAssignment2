## Minimize calls to solve() by caching the inverse of a given matrix


## makeCacheMatrix creates a special "matrix" object that can return and set the values for 
## a matrix, as well as return and set (i.e., cache) its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    
    setInverse <- function(inverseNewVal) inverse <<- inverseNewVal
    getInverse <- function() inverse
        
    list(
        set = set, 
        get = get,
        setInverse = setInverse,
        getInverse = getInverse
        )
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}
