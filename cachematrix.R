## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    # setting initial inverse as NULL
    inv <- NULL

    # create function to set a matrix
    set <- function(y) {
        # setting value of x in parent function to y
        x <<- y

        # clearing cached inverse
        inv <<- NULL
    }

    # creating getter
    get <- function() x
    
    # creating setter for inverse
    setinverse <- function(inverse) inv <<- inverse

    # creating getter for inverse
    getinverse <- function() inv

    # packing all the functions into a list object
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Computes the inverse of the special matrix returned by makeCacheMatrix. If the inverse
## is cached, it will returned the cached value, otherwise it will compute the inverse
## and cache it.
cacheSolve <- function(x, ...) {
    # getting cached inverse
    inv <- x$getinverse()

    # checking if cached value exists
    if(!is.null(inv)) {
        
        # returning cached value
        return(inv)
    }

    # calculating inverse since cache is empty
    data <- x$get()
    inv <- solve(data, ...)

    # setting inverse
    x$setinverse(inv)

    # returning inverse
    inv
}