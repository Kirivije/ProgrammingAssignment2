## makeCacheMatrix function will make a matrix object that can be cached the inverse
## cacheSolve function will take the matrix object created by the above function
## and inverse it if there is no inversed matrix already cached. 
## If there is an inversed matrix cached, this function will take the inversed matrix
## from cache. 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
                m <- NULL # initialize matrix to null 
                
                set <- function(y) { # this sets new value to the matrix, invalidating cached matrix 
                        
                        x <<- y # set the values in the enclosing environment
                        m <<- NULL #m set to NULL as cached values are no longer valid
                }
                
                
                
                get <- function() x # getting function for underline matrix
                setinverse <- function(inverse) m <<- inverse # this will set the invsersed matrix
                getinverse <- function() m # this will get the inversed matrix
                
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse) # return values of the makeCacheMatrix 
}



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse() # get the inverse matrix created by makeCacheMatrix
        if(!is.null(m)) { # if 'm' is not null get the cached 'm'  
                message("getting cached data")
                return(m)
        }
        data <- x$get() # assign matrix to the variable 'data'
        m <- solve(data, ...) # inverse the matrix and assign it to 'm'
        x$setinverse(m) # cache the inversed matrix
        m # return m
}

################here are the tests that I did test the functions and output###########
### based on an answer by Gregory D. Horne#################

mymetrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
mymetrix$get() # return original matrix 
#output 
#[,1] [,2]
#[1,]    1    3
#[2,]    2    4
cacheSolve(mymetrix)#Computes, caches, and returns    mat
#output
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
mymetrix$getinverse()# Returns matrix inverse
#output
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
cacheSolve(mymetrix)#Returns cached matrix inverse using previously computed matrix inverse
##output 
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
mymetrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2))# Modify existing matrix
cacheSolve(mymetrix) # Computes, caches, and returns new matrix inverse
#output
#[,1] [,2]
#[1,] -0.13333333  0.2
#[2,]  0.01010101  0.0
mymetrix$get() # Returns matrix
#output
#[,1] [,2]
#[1,]    0   99
#[2,]    5   66
mymetrix$getinverse() # Returns matrix inverse
#output
#[,1] [,2]
#[1,] -0.13333333  0.2
#[2,]  0.01010101  0.0
