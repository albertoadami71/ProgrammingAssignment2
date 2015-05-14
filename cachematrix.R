## Matrix inverse calculation is a costly operation, therefore our aim is to 
## construct functions to calculate and store the inverse of a given matrix.
## The stored matrix can be retrieved, avoiding repeated inversion operations.  
## 
## The first function below stores and retrives a given matrix and its inverse. 
## In the second function, we get the stored inverse matrix, if it exists, 
## or calculate the inverse, if it does not exist yet. 




## This function (makeCacheMatrix) stores and retrives a given matrix 
## and its inverse. 
## Input data of makeCacheMatrix is a invertible square matrix "x" 
## and the output data is a list of four functions that:
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse matrix
## 4. get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL
	set<- function(y){
		x<<-y
		inv<<-NULL
	}
	get<-function()x
	setinverse<-function(inverse) inv<<-inverse
	getinverse<-function() inv
	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}




## This function (cacheSolve) get the stored inverse matrix or calculate it.
## Input data is a list created with the makeCacheMatrix function (above).
## Output is the inverse matrix. 
## The function first verify if the inverse already exists (x$getinverse() not NULL), 
## otherwise it calculates the inverse, using the function "solve()"   

cacheSolve <- function(x, ...) {

  ## Getting stored inverse matrix 
	inv<-x$getinverse()
	if(!is.null(inv)){
		## if inv is not NULL, then return stored inverse matrix
		message("getting cached data")
		return(inv)
	}
	## Otherwise, calculate inverse matrix and set inverse
	message("calculating inverse matrix")
	data<-x$get()
	inv<-solve(data)
	x$setinverse(inv)
	inv
        ## Return a matrix that is the inverse of 'x'
}
