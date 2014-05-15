## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    get <- function() x
    setinverse <- function(inverseMatrix) im <<- inverseMatrix
    getinverse <- function() im
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The second function calulates inverse matrix, that is stored
## in object function above, or returns cached inverse of the matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    im <- x$getinverse()
    if(!is.null(im)) {
        message("getting cached matrix")
        return(im)
    }
    data <- x$get()
    im <- solve(data)
    x$setinverse(im)
    im
}
