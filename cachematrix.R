# Las siguientes funciones almacenan la inversa de una matriz en cache, evitando
# calcularla en forma repetida.


# La funcion 'makeCacheMatrix' crea una lista (list) que contiene una funcion para:
# 1. Asignar (set) la matriz.
# 2. Obtener (get) el valor de la matriz.
# 3. Asignar (set) la inversa de la matriz.
# 4. Obtener (get)  el valor de la inversa de la matriz 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinversa <- function(inversa) inv <<- inversa
  getinversa <- function() inv
  list(set = set, get = get,
       setinversa = setinversa,
       getinversa = getinversa)
}


# La funcion 'cacheSolve' entrega la inversa de la matriz. Primero verifica si la 
# inversa ya fue calculada. De ser asi, obtiene el resultado y no realiza el 
# calculo. En caso contrario, calcula la inversa, y asigna el resultado  
# a traves del cache a la funcion 'setinversa".

cacheSolve <- function(x, ...) {
  inv <- x$getinversa()
  if(!is.null(inv)) {
    message("Obteniendo Inversa...")
    return(inv)
  }
  datos <- x$get()
  inv <- solve(datos)
  x$setinversa(inv)
  inv
}


# Ejemplo
# x <- matrix(c(1, -2, 20, 500), 2,2)
# M <- makeCacheMatrix(x)

# M$get()
#       [,1] [,2]
# [1,]    1   20
# [2,]   -2  500

# M$getinversa()
# NULL

# cacheSolve(M)
#             [,1]         [,2]
# [1,] 0.925925926 -0.037037037
# [2,] 0.003703704  0.001851852

# cacheSolve(M)
# Obteniendo Inversa...
#             [,1]         [,2]
# [1,] 0.925925926 -0.037037037
# [2,] 0.003703704  0.001851852