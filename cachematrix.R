## These two functions allows to store inverse matrix (if it was counted once) and use it when someone tries to count inverce once more time

## This function creates an object for the matrix, which allows to store inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function counts the inverse if it haven't been counted yet. Otherwise it retutns inverse from the cache of the object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <-  solve(data)
        x$setinverse(m)
        m

}
