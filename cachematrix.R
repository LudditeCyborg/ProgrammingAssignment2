## These two functions can be used to cache matrix solutions
## This allows matrix solutions to only be calculated once

## The function below takes a matrix and outputs a special object 
## The special object is able to cache the matrix solution

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinvert <- function(invert) m <<- invert
        getinvert <- function() m
        list(set = set, get = get,
             setinvert = setinvert,
             getinvert = getinvert)
}


## The below function takes the special object created by makeCacheMatrix
## It attempts to retrieve the cached solution if it exists
## Performs the solution if no cached solution already exists

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinvert()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinvert(m)
        m
}
