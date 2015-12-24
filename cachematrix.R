## These functions enable quick calculation of matrix inverses by making 
##use of cache memory. 

# makeCacheMatrix makes a list of functions: 
# 1. set: sets the value of the matrix in the makeCacheMatrix environment
# 2. get: gets the value of the matrix from the makeCacheMatrix environment
# 3. setinver: sets the value of the inverse of the matrix
# 4. getinver: gets the value of the inverse of the matirx


makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setmean <- function(inverse) inver <<- inverse
        getmean <- function() inver
        list(set = set, get = get,
             setinver = setinver,
             getinver = getinver)
}


## cacheSolve calculates the inverse of the matrix with the 
## above function.  If the inverse has already been calculated
## it returns the mean from the cache and skips the computation.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver <- x$getinver()
        if(!is.null(inver)) {
                message("getting cached data")
                return(inver)
        }
        data <- x$get()
        inver <- solve(data, ...)
        x$setinver(inver)
        inver
}
