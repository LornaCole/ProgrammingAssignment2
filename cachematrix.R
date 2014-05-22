## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##MakeCacheMatrix creates a location to store the results of cacheSolve
##cacheSolve creates the inverse of the matrix and stores it in the cache so 
##next time the cacheSolve function is run it obtains the data from
##makeCacheMatrix directly rather than recalculating.


makeCacheMatrix <- function(x = matrix()) {
            m <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setmatrix <- function(solve) m <<- solve
            getmatrix <- function() m
            list(set = set, get = get,
                 setmatrix = setmatrix,
                 getmatrix = getmatrix)
}
                              
    cacheSolve <- function(x, ...) {
            m <- x$getmatrix()
            if(!is.null(m)) {
                    message("SUCCESS! Getting cached data")
                    return(m)
            }
            data <- x$get()
            m <- solve(data, ...)
            x$setmatrix(m)
            m
    }


##cache matrix called MatrixA using the makeCacheMatrix command 

MatrixA<-makeCacheMatrix(matrix(c(4,3,3,2), nrow=2, ncol=2))

##Try to obtain matrix from the working directory output indicates where stored

MatrixA

##Try to obtain the matrix from the Cache and the value is NULL as not calculated
##yet by cacheSolve
MatrixA$getmatrix()

##Use cacheSolve on MatrixA and it returns the inverse of the matrix
cacheSolve(MatrixA)

##Now when we try and retrieve MatrixA using MatrixA$getmatrix the inverse is returned
MatrixA$getmatrix()

##NOW when we run cacheSolve(MatrixA) the function returns the cached inverse m
##atrix data displaying the message "SUCCESS! Getting cached data" 

cacheSolve(MatrixA)

##create cache for MatrixB using makeCacheMatrix for a different matrix
MatrixB<-makeCacheMatrix(matrix(c(10, 9, 8, 7), nrow=2, ncol=2))

##Use cacheSolve on MatrixB and it returns the inverse of the matrix
cacheSolve(MatrixB)

##NOW when we run cacheSolve(MatrixA) the function returns the cached inverse
##of MatrixA nd when we run cacheSolve(MatrixB) the function returns the 
##cached inverse of MatrixB both are returned from cached data as displayed
##by the message. "SUCCESS! Getting cached data"  WOOO HOOO!

cacheSolve(MatrixA)
cacheSolve(MatrixB)

