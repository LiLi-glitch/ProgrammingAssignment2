#####
#progamming assignment 2
####

###########
#The first function, makeVector creates a special "vector", which is really a list containing a function to
#set the value of the vector
#get the value of the vector
#set the value of the mean
#get the value of the mean
###########

makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

#################
#calculates the mean of the special vector created
#created with the above functions
#checks if th emean has already been calculated
#if so, it gets the mean from the cache and skips compuation
#otherwise, it calculates the mean of the data 
#sets value of the mean in the cache via setmean function
##################

cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}

############
#makeCacheMatrix: function creates a special "matrix" object that can cache its reverse
############

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

##########
#cacheSolve: function computes inverse of special matrix returned by makecachemartrix
#if the inverse has already been caclulated, then cachesolve should retrieve the inverse from the cache
#########

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting inverted cached data")
        return(inv)
    }
    data<-x$get()
    inv <- inverse(data, ...)
    x$setinverse(inv)
    inv
}
