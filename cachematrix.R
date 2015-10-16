## Caching the inverse of a matrix
## Using functions to calculate and cache the inverse of a matrix in R

## Create makeCacheMatrix to return a special matrix object containing functions to access and manage matrices stored in Cache

makeCacheMatrix <- function(x = matrix()) {
    ## Initialise matrix inverse
    matrix_inv <- NULL
    ## Define set_matrix function to create new matrix in Cache
    set_matrix <- function(y){
        x <<- y
        matrix_inv <<- NULL ## clear exisitng matrix inverse
    }
    ## Define get_matrix to return current matrix stored in Cache
    get_matrix <- function() x 
    ## Define get_inverse to return current inverse stored in Cache
    get_inverse <- function() matrix_inv
    ## Define set_inverse to store calculated inverse into Cache
    set_inverse <- function(inv) matrix_inv <<- inv
    ## Return special function matrix to access matrices stores in Cache
    matrix(list(get_matrix, set_matrix, get_inverse, set_inverse), nrow = 2, ncol = 2, dimnames = list(c("get", "set"), c("matrix", "inverse")))

}


## cacheSolve returns the inverse of a matrix, either from cache if already stored or calculates and stores inverse into the Cache if not available

cacheSolve <- function(x, ...) {
    ## Retrieve cached inverse
    ret_inverse <- x[["get", "inverse"]]()
    ## Check if inverse is already available in Cache and return Cached inverse if available
    if (!is.null(ret_inverse)) {
        message("getting cached inverse")
        return(ret_inverse)
    }
    ## Calculate and store inverse if not available in cache
    
    ## get current matrix from cache
    my_matrix <- x[["get", "matrix"]]()
    ## calculate inverse
    inv <- solve(my_matrix)
    ## store inverse to cache
    x[["set", "inverse"]](inv)
    ## return inverse
    inv
}
