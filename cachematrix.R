## "makeCacheMatrix" is a function that creates and stores a matrix

## "cacheSolve" is a function that gets the inverse of a matrix either
## from a cache if one exist or through the solve function


## to create a matrix with "makeCacheMatrix" first call it then use $set
## to add the matrix
## example: a <- makeCacheMatrix() ; a$set(matrix(1:4,2,2)) 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function()m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## to get the inverse of a matrix stored in the above function simply
## call it (ex. cachesolve(a)) and the function will return the inverse
## either from the cache or through the solve function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {return(m)}
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}