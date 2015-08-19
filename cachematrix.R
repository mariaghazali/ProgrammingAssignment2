## Calculates the Inverse of a Matrix.
## Takes the matrix from cache if available.
## Matrix must be square and invertable.

## step 1. makeCacheMatrix function creates a special "matrix" object 
##      that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y # search through the parents environments for the definition of x
                m<<-NULL # search through the parents environments for the definition of m
        }
        get<-function() x
        setsolve<-function(solve) m<<-solve
        getsolve<-function() m
        list(set=set, get=get,
             setsolve=setsolve, getsolve=getsolve)
}

## step 2. cacheSolve function uses object (x) created with makeCacheMatrix and 
##      returns inverse of the cached matrix. 

##      This function computes the inverse of the special "matrix" returned 
##      by makeCacheMatrix above. If the inverse has already been
##      calculated (and the matrix has not changed), then cacheSolve
##      should retrieve the inverse from the cache.

##      Check for the squareness of the matrix will do solve function.

cacheSolve <- function(x, ...) {
        # ... - for additional args from solve
        m<-x$getsolve()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data, ...)
        x$setsolve(m)
        m
}

# # usage
# matr<-matrix(rnorm(25),5,5)
# x<-makeCacheMatrix(matr)
# cache.solve<-cacheSolve(x)
# cache.solve
# solve(matr)==cache.solve
