## This function creates a special "matrix" object that can cache its inverse
## which is computed with cacheSolve function

## This function allows caching of matrix 'x' - must be squared matrix

makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                ## <<- this operator is used to assign a value to 
                ##an object in an environment that is different from 
                ##the current environment
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## This function computes inverse matrix

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) { #testing if matrix m had been computed
                message("getting cached data")
                return(m) #if so, getting cached data
        }
        data <- x$get()
        m <- solve(data, ...) #matrix inversion
        x$setsolve(m)
        m
}
