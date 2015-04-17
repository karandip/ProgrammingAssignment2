## Caching the inverse of the matrix

## Functions used :

## makeCacheMatrix <- function(x = matrix())
##      -- creates an enviroment where the matrix, its inverse and all the 
##         handling functions are stored
##      -- returns list of matrix-inverse handling functions
##              * set(y)
##              * get()
##              * setinverse(inverse)
##              * getinverse()

## cacheSolve <- function(x, ...)
##      -- checks if the inverse matrix is cached otherwise calculates it
##      -- returns inverse matrix

#############################################################################


##     Creates an enviroment where the matrix, its inverse and all the 
##     handling functions are stored

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inverse <<- inverse
        getinverse <- function() inverse
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}


## Calualate the inverse matrix if not cached 

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, diag(dim(data)[1]),...)
        x$setinverse(inverse)
        inverse
}
