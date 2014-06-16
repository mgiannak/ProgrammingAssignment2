## Create an object that will speed up the delivery of the invert
## of a matrix using lexical scoping.
## Usage : v <- makeCacheMatrix
##         v$set(my_matrix)
##         cacheSolve(v)

## Create the matrix cache object, which is basically the cached computation
## of a matrix + a set of 4 functions: a getter and a setter, and cache getter 
## and setter for the cacheSolve function
makeCacheMatrix <- function(x = matrix()) {
        ## the cache
        cachedMatrix <- NULL
        ## setter
        set <- function(y) {
                x <<- y
                ## reset the cache
                ## we could also recompute directly the cache
                cachedMatrix <<- NULL
        }
        ## getter
        get <- function() x
        ## update the cache content
        setCachedMatrix <- function(cachedMatrix_) {
                cachedMatrix <<- cachedMatrix_
        }
        ## return the cache content
        getCachedMatrix <- function() cachedMatrix
        ## list of available functions
        list(set = set, get = get, 
             setCachedMatrix = setCachedMatrix,
             getCachedMatrix = getCachedMatrix)
}


## Returns the invert of a 'x ' matrix, taking into parameter an object as 
## created by makeCacheMatrix. It either returns directly the cached 
## computation (if already computed and original matrix didn't change meanwhile) or 
## or computes the invert and stores it for further usage. 
cacheSolve <- function(x, ...) {
        ## Look for a cached computation
        cachedMatrix <- x$getCachedMatrix()
        if (!is.null(cachedMatrix)) {
                ## if cached computation is available, returns it
                message("getting cached matrix")
                return(cachedMatrix)
        }
        ## if cached computation is not available, get the original matrix
        data <- x$get()
        ## verify this is a square (invertible) matrix
        if (nrow(data) != ncol(data)){
                warning("matrix is not invertible")
                stop()
        }
        ## compute the inverted matrix
        cachedMatrix <- solve(data)
        ## store the inverted matrix
        x$setCachedMatrix(cachedMatrix)
        ## return the inverted matrix
        cachedMatrix
}