## The goal of this assignment is to use the "<<-" Special Assignment 
## (aka "superassignment") Operator to demonstrate the use of "closures" or
## functions written by another function.
## Some excellent examples of how to approach their use can be found in
## Hadley Wickham's open-source textbook "Advanced R" in the Functional
## Programming section (http://adv-r.had.co.nz/). As is often the case,
## CRAN has us covered, too 
## (https://cran.r-project.org/doc/manuals/R-intro.html#Scope).
## There are some useful R packages that could simplify the process of
## caching and inverting matrices, like SOAR by Bill Venables
## (https://cran.r-project.org/web/packages/SOAR/index.html), memoise
## (https://cran.r-project.org/web/packages/memoise/index.html), and 
## R.cache (https://cran.rstudio.com/web/packages/R.cache/index.html),
## but those shortcuts don't teach us about "<<-" and may be overkill for
## our needs here.

## First, we'll define the "makeCacheMatrix" function to create a special 
## "matrix" object that can cache its inverse.

  makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Next, we'll define the "cacheSolve" function to compute the inverse of the 
## special "matrix" returned by makeCacheMatrix above. If the inverse has  
## already been calculated (and the matrix has not changed), then the  
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}

## Now, we've successfully "cached" the matrix inversion task with "<<-", 
## which modifies existing variables in parent environments rather than
## assigning in the current environment.
