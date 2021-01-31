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
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}

##Verification example
#Matrix
# A <- rbind(c(1, 1 ,0), c( 1,0 ,1), c( 0 ,1, 0))
##Creates a special matrix object that can cache its inverse
#B <-makeCacheMatrix (A)
## Calculate the inverse and save it in cache 
#cacheSolve(B)
##Verification
#cacheSolve(B) == solve(A)
