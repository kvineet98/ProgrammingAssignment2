## the first function( makeCacheMatrix) will retrun the list 
## and store the matrix elements. 
## Function get : To get Matrix elements 
## Function Set : To get Matrix elments. This will be use to verify if elements are same
##			before get the cached Inverse value.  
## Function getinverse : To get the cached Matrix inverse
## Function getinverse : To set the Matrix inverse in cache.


## CacheMatrix to create the matrix. 

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set<-function(y){
		x<<-y
		m<<-NULL
		}
	get <- function() x
	setinverse <-function(inverse) m<<- inverse
	getinverse <-function() m
	list(set = set, 
		get =get, 
	     	setinverse = setinverse, 
		getinverse =getinverse)
}



## Following function will retrun the cache inverse of matrix only if elments are same and
## inverse is not null.otherwise it will calculate the inverse with Solve function 
## and then set the new value in cache again.


cacheSolve <- function(x, newMatrix=matrix(),...) {
      ## Get the cached data to make sure it's same as passed matrix x
	
	cached_data<-x$get()
	
	## the following If statement will check if all elements are same 
	if (all(cached_data==newMatrix)) 
		{	
			## All elements are same and now get the saved inverse.
			m<-x$getinverse()
			if(!is.null(m)){	
				print("Getting Cache inverse of Matrix")
				return(m)
				}
			m<- solve(cached_data)
			x$setinverse(m)
			m
		}
	else
		{	## Elements are not same recalculate inverse 
			## of the Matix elements and set the cache value
			m<- solve(newMatrix, ...)
			x$setinverse(m)
			m
		}
	}
