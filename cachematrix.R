## The functions are dedicated to avoid time consuming inverse matric xomputations. 
## 1st once create a mtrix that can cache its inverse and
## 2nd one can calculate the inverse but skip if the inverse have already been calculated

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { # create a function to make new matrix
        m <- NULL
        set <- function(y) {    # creating "set" function to set the value of x
                x <<- y
                m <<- NULL
        }
        get <- function() x                     # creating "get" function to get the value of x
        setinverse <- function(solve) m <<- solve # creating "setinverse" function to assing value to m
        getinverse <- function() m              # creating "getinverse" function to get the value of inverse matrix 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {        # creating cacheSolve function
        m <- x$getinverse()             # assigning value of m from getinverse funtion created in pervious function
        if(!is.null(m)) {               # if the value of m exists, get the value from chached data
                message("getting cached data")
                return(m)
        }
        data <- x$get()                 # If the value of m does not exists, get the value of x using "get" function and assign that value to data
        m <- solve(data, ...)           # calculate the inverse of data using solve funtion
        x$setinverse(m)                 # assign the calculated inverse matrix value to m using "setinverse" function
        m
}
