## The functions bellow allow the creation of an special matrix that 
## caches its inverse (function makeCacheMatrix), what is done using
## the cacheSolve funcion.
## We take the assumption that the passed matrix is square and inversible.

## makeCacheMatrix creates a special "matrix" which is able to cache 
## its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## inv stores the inverted matrix x
    inv <- NULL
    
    ## sets a new matrix and clears the inverted matrix cached
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## returns the cached matrix
    get <- function() x
    
    ## stores the inverted matrix
    setinverse <- function(inverse) inv <<- inverse
    
    ## returns the inverted matrix
    getinverse <- function() inv
    
    ## special matrix operations 
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## cacheSolve is able to compute the inverse of the special "matrix" which
## is created using makeCacheMatrix. 
## If this operation has already been performed, then the cached value is 
## returned

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## Getting the inverse
    inv <- x$getinverse()
    
    ## If a cached inverse was discovered, then return this cache
    if(!is.null(inv)) {
        message("Getting cached data")
        return(inv)
    }
    
    ## Otherwise calculate the inverse of the matrix
    data <- x$get()
    ## Check if there is a matrix in x
    if(is.null(data)) {
        message("Cached Matrix is empty")
        return(NULL)
    }
    inv <- solve(data, ...)
    
    ## Storing the calculated value
    x$setinverse(inv)
    
    ## Returning inverted matrix
    inv
}
