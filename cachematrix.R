## Create an object that will speed up the delivery of the invert
## of a matrix using lexical scoping.
## Usage : 
# > v <- makeCacheMatrix()
# > v$set(matrix(rnorm(1000000),1000,1000))
# > inverted_m <- cacheSolve(v)
# Computed inverted matrix in 2.169 (secs)
# > inverted_m <- cacheSolve(v)
# getting cached matrix in 0 (secs)


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
                ## we could also recompute directly the cache..
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
        start_time <- Sys.time()
        ## Look for a cached computation
        cachedMatrix <- x$getCachedMatrix()
        if (!is.null(cachedMatrix)) {
                ## if cached computation is available, returns it
                end_time <- Sys.time()
                message(paste("getting cached matrix in",
                              round(end_time-start_time,3),
                              "(secs)"))
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
        ## benchmark it
        end_time <- Sys.time()
        message(paste("Computed inverted matrix in",
                      round(end_time-start_time,3),
                      "(secs)"))
        ## return the inverted matrix
        cachedMatrix
}