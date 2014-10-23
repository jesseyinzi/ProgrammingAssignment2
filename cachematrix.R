## a function to calculate the inverse of a matrix and avoids 
## computation if the result already exsits

## a function that set up four functions

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) m <<- inverse
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## calculate the inverse of a matrix

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        library(MASS)
        m <- ginv(data)
        x$setinv(m)
        m

}
