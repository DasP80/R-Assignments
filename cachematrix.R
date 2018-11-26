# makeCacheMatrix takes argument x (default value as an empty matrix)and assigns NULL to inv object .
# This function contains separate functions like:
# 1. set() function takes argument y , assigns y to the x object and NULL to the inv object in the parent environment using <<. This line of code clears any value of inv that had been cached by a prior execution of cacheSolve().
# 2. get() function uses lexical scoping features in R. Since the symbol x is not defined within get(), R retrieves it from the parent environment of makeCacheMatrix()
# 3. set_inv() function takes the argument and assigns to inv object in the parent environment using <<.
# 4. get_inv() function returns the inv object
# The above 4 functions are added as elements to the list with each element named same as the element itself. This allows use of $ to access the functions by name.


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL

        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

        get <- function() x

        set_inv <- function(z) inv <<- z

        get_inv <- function() inv

        list(set = set, get = get,
             set_inv = set_inv,
             get_inv = get_inv)
}

# cachesolve() function either calculates or retrieves the inverse of a matrix. It needs an input argument of type makeCacheMatrix(). The triple dots allow to pass additional arguments into the function. 
# This function retrieves the inverse from the cache If the inverse has already been calculated (and the matrix has not changed)
# Else computes the inverse of the matrix . 
# solve function in R computes the inverse of the non singular square matrix. 

# First, it calls the get_inv() function and checks for NULL. if the result is not equal to NULL, then returns the cached inverse
# makeCacheMatrix() sets the cached inverse to NULL whenever a new matrix is set into the object. So if result is NULL then it calculates the inverse using solve()
# Then it calls the set_inv() function and returns the result to the parent environment

cacheSolve <- function(x, ...) {
        inv <- x$get_inv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$set_inv(inv)
        inv

}

# The below code is used to execute the script

# > source("cachematrix.R")
# > n2 <- makeCacheMatrix(matrix(9:12,2,2))
# > cacheSolve(n2)
# Output
#     [,1] [,2]
# [1,]   -6  5.5
# [2,]    5 -4.5
# > cacheSolve(n2)
# output
# getting cached data
#     [,1] [,2]
# [1,]   -6  5.5
# [2,]    5 -4.5
# > 

