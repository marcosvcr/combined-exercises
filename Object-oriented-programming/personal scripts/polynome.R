Polynome <- function(expression)
{

  thisEnv <- environment()

  me <- list(
    formula = function(x) eval(parse(text=expression)),
    thisEnv = thisEnv,

    ## The Methods for this class normally go here but are discussed
    ## below. A simple placeholder is here to give you a teaser....
    getEnv = function()
    {
      return(get("thisEnv",thisEnv))
    }


  )


  ## Set the name for the class
  class(me) <- append(class(me),"Polynome")
  return(me)
}

evaluate_polynomial <- function(obj, number)
{
  UseMethod("evaluate_polynomial", obj)

}

evaluate_polynomial.default <- function(obj, number)
{
  stop("I do not know how to handle this object.")

}

evaluate_polynomial.Polynome <- function(obj, number)
{
  return(obj$formula(number))

}



uniroot <- function(obj, lower=a, upper=b)
{

  UseMethod("uniroot", obj)

}

uniroot.Polynome <- function(obj, lower=a, upper=b)
{

  #bissec metthod

  if((upper-lower)<10^-4){
    raiz=lower
  }
  k=0

  while((upper-lower)>10^-4){
    M=obj$formula(lower)
    x=(lower+upper)/2
    if(M*obj$formula(x)>0){
      lower = x
    }else{
      upper=x
    }
    k=k+1
  }

  print("root is: ")
  print(x)
  print("number of iterations")
  print(k)

}


### test

a <- Polynome("3*x")
b <- Polynome("4 + 3*x + 7*x^2 + 5*x^3")

evaluate_polynomial(a, 3)
evaluate_polynomial(b,2)
uniroot(b, lower=-7, upper=7)
uniroot(a, lower=-3, upper=3)



