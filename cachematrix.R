

makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    get <- function() x
    stinverse<- function(inverse) inv_x <<-inverse
    gtinverse <- function() inv_x
    list(set = set, get = get,
         stinverse = stinverse,
         gtinverse = gtinverse)
}

cacheSolve <- function(x, ...) {
    inv_x <- x$gtinverse()
    if (!is.null(inv_x)) {
        message("getting cached inverse matrix")
        return(inv_x)
    } else {
        inv_x <- solve(x$get())
        x$stinverse(inv_x)
        return(inv_x)
    }
}
