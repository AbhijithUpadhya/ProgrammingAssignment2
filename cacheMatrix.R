## makeChacheMatrix stores a list that can perform functions: 
##create matrix, return matrix, compute inverse of matrix and return inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#1. set the value of the vector
#2. get the value of the vector
#3. set the value of the mean
#4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {#define some argument with some matrix
    i <- NULL
    set <- function(y) {#set your new variable
        x <<- y #x get value of matrix
        i <<- NULL # if matrix assgin then it will null to eliminate before one
    }
    get <- function() x #get your assgined variable
    setinverse <- function(inverse) i <<- inverse #assign new variable in i
    getinverse <- function() i #get value when it calls
    list(set = set, get = get, # Functions running by list cases
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse() # i gets x value
    if(!is.null(i)) { # if i is null
        message("getting cached data") # the it print this message
        return(i) #Return cached data
    }
    data <- x$get() #data get x value which is matrix
    i <- solve(data) #i get solve value which is inverse matrix of data
    x$setinverse(i) # x get value which is solved
    i #Print i value
}

#order
#source("cachematrix.R")
#x = rbind(c(3,4),c(5,6))
#m = makeCacheMatrix(x)
#m$get()
#Can see the martix
#     [,1] [,2]
#[1,]    3    4
#[2,]    5    6
#cacheSolve(m)
#Can see the solve
#     [,1] [,2]
#[1,] -3.0  2.0
#[2,]  2.5 -1.5
#cacheSolve(m)
#getting cached data
#     [,1] [,2]
#[1,] -3.0  2.0
#[2,]  2.5 -1.5
        }
        get<-function() x
        setmatrix<-function(solve) m<<- solve
        getmatrix<-function() m
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}


## cacheSolve will compute inverse but it checks if it is calculated already 
## if it is calculated it will print "getting chached data" and return calculated matrix

cacheSolve <- function(x, ...) {
        m<-x$getmatrix()
        if(!is.null(m)){
                message("getting cached matrix")
                return(m)
        }
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m
}
