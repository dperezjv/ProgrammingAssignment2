## Computing the inverse of a square matrix and remain it in cache-
## 

## Make a special "Matrix",which is really a list containing a function to

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the its inverse
## 4. get the value of the its inverse
##

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Return the inverse of a square Matrix. First look at if the inverse has already
## been computed. If so, return a chache Matrix. Otherwise, calcute the inverse and
## set in the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
