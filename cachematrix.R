## The first function takes the input x, which is a matrix, and creates a list, 
## which is used as the input for the second function. This list includes set, 
## get, setsolve, and getsolve. Then the second function, cacheSolve, takes the 
## the first function as an input, and either calculates the inverse of the matrix
## x or retrieves the inverse of x from the cache. If the function retrieves
## the result from the cache, then it displays the message "getting cached data"
## before the result.

## makeCacheMatrix first intializes the inputs x and s. Then it creates the set
## function by reassigning the variables x and s in the parent environment using
## <<-. Then the get function is created, which is just retrieving the variable
## x. The setsolve function assigns s to solve in the parent environment. The
## getsolve function retrieves the variable s. Finally, a list is created, 
## setting each function to a variable, so that the next function is able to 
## subset from the result, using $. 

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve takes the makeCacheMatrix(x) as an input, where x is a matrix.
## Then it assigns the getsolve part of the list from makeCacheMatrix to the 
## variable s. If s is found in the cache, then the function prints "getting 
## cached data" and returns s, which is the inverse of x. If s is not found in
## the cache, then the function assigns the get part of the list from 
## makeCacheMatrix to the variable data. The solve function is performed on 
## data, which finds the inverse of x, which is then printed. 

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
