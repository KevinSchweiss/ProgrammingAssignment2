## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##makeCasheMatrix takes a Matrix as an input, sets the value of the matrix, generates and gets the inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) {	##defines argument
	inv<-NULL						##holds value of Matrix
	set<-function(y){					##assigns new set function
		x<<-y						##assigns the value to something in a different environment 
		inv<<-NULL					##resets inv to NULL if there is a new matrix
	}
	get<-function()x					##defining get function and returns value of matrix arguement
	setinverse<-function(inverse) inv<<-inverse	##set value of invertable matrix
	getinverse<-function()inv				## gets value of that matrix
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

##CacheSolve computes inverse of the matrix returned by makeCacheMatrix. If the matrix is empty is will get the original data. If the matrix has value it will retrive from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	invMatrix<-x$getInverse()
	if(!is.null(invMatrix)){			##checks that inverse matrix is not NULL
		message("Getting cached Matrix")	##Type message
		return(invMatrix)				##returns invertable matrix
	}
	MatrixData<-x$getMatrix()			##gets original data
	invMatrix<-solve(MatrixData,...)		##solves for inverse of matrix
	x$setInverse(InvMatrix)				##sets the matrix
	return(invMatrix)					## retruns the matrix
}

