##  Se va a escribir una función en R que pueda almacenar
## en caché los cálculos que consumen mucho tiempo.

## Esta función makecachematrix crea un vector especial 
## para poder calcular la inversa de una matriz.

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
 set <- function(y) {
         x <<- y
         inv <<- NULL
         }
         get <- function() (x)
         setInverse <- function(inverse) {inv <<- inverse}
         getInverse <- function() {inv}
         list(set = set, get = get, setInverse = setInverse, 
              getInverse = getInverse)
         }


## Esta función calcula la inversa de una matriz. Aunque 
## primero mira en la memoria cache si esta ya ha sido 
## calculada; si no esta la calcula y la guarda en la 
## memoria cache.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        
        if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
        }
                matr <- x$get()
                inv <- solve(matr, ...)
                x$setInverse(inv)
                inv
                }
   
## Vamos a correr el modelo planteado:

#  source("cachematrix.R")

## Luego vamos a tomar este ejemplo.

 qmatrix <- makeCacheMatrix(matrix(6:9, 
             nrow = 2, ncol = 2))
 
 qmatrix$get()
 
 qmatrix$getInverse()
 
 cacheSolve(qmatrix)
                


