## Functions will calculate the inverse of a matrix and set it in the cache
## unless the inverse matrix has already been set in the cache 
## at which point it will get the cached matrix inverse instead of calculating it

## Function creates a special "matrix" object that can cache its inverse;  
## assumes that matrix is always invertible

makeCacheMatrix <- function(x = matrix()) {
        inve <- NULL
        set <- function (y) {
                x <<- y
                inve <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Calculates the matrix inverse and sets it in cache
## unless it is already stored in the cache, in which case it retreives it

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
                
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
        
}