makeCacheMatrix <- function(x = matrix()){
        ## This function creates a special "matrix" object that can cache its inverse.
        ## I copied the provided example for means of a vector, and made adjustments
        ## to inverse a matrix using solve function instead of mean
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


cacheSolve <- function(x, ...){
        ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
        ## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
        ## should retrieve the inverse from the cache.
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
