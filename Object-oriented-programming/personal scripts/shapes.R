
### Classes
Circle <- function(x)
{

  me <- list(
    radius = x
  )

  ## Set the name for the class
  class(me) <- append(class(me),"Circle")
  return(me)
}

Square <- function(x)
{
  me <- list(
    size_Side = x
  )

  ## Set the name for the class
  class(me) <- append(class(me),"Square")
  return(me)



}



###polymorphism functions

circumference <- function(x)
{
  UseMethod("circumference", x)

}

circumference.default <- function(x)
{
  print("I do not know how to handle this object.")

}

circumference.Circle <- function(x)
{
  return(2*pi*x$radius)

}

circumference.Square <- function(x)
{
  print("Assuming a square inscribed on the circumference")
  print("using the Pythagorean theorem, that the length of a diagonal is sqrt(2)*side and so what you are calling the 'radius' is half the diagonal")
  r<- (sqrt(2)*x$size_Side)/2

  circun <- 2*pi*r
  print("the radius is")
  print(r)
  print("the circumference is")
  print(circun)
}


area <- function(x)
{
  UseMethod("area", x)

}

area.default <- function(x)
{
  print("I do not know how to handle this object.")

}


area.Circle <- function(x)
{
  return(pi*x$radius^2)

}



area.Square <- function(x)
{
  return(x$size_Side^2)

}



##### tests

b<- Square(2)
c <- Circle(7)

circumference(b)
circumference(c)
area(b)
area(c)



