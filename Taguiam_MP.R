#1Define an R function that removes NA values from a vector.
remove_na <- function(values){
  values[!is.na(values)]
}

#Example_Test
values <- c(1,2,3,NA)
remove_na(values)

#2 Define an R function that computes the factorial of given an integer argument. The output should be a vector of length 1.
factorial_int <- function(a){
  fact <- 1
  i <- 1
  while (i <= a){
    fact <- fact*i
    i <- i+1
  }
  return(fact)
}

#Example_Test
factorial_int(5)
  
#6 Create a function to compute for your net pay at work. (MONTHLY)

Net_Pay_Mo <- function(salary){
  Pagibig <- 100
  SSS <- 581.30
  Philhealth <- 550
  Govt_Cont <- Pagibig+SSS+Philhealth
  ProjTaxInc <- 12*(salary-Govt_Cont)
  Annual_Tax <- ((ProjTaxInc-400000)*0.25)+30000
  return(salary-(Annual_Tax/12))
  
}

#Example_Test
Net_Pay_Mo(42660)

#4Define an R function that sorts a given vector in decreasing order. The output should be a vector of the same length. It should accept both numeric or character vectors.
x <- c(1,3,4,5,2)
y <- c("a","c","e","b","g")
dec_sort <- function(x){
  num <- length(x)
  for(i in 1:(num-1)){
    for (j in (i+1):num){
      if(x[i] < x[j]){
        temp <- x[i]
        x[i] <- x[j]
        x[j] <- temp
      }
    }
  }
  return(x)
}

#Example_Test
dec_sort(x)
dec_sort(y)


#5Define an R function that accepts a Date (POSIXct) as argument and outputs the day of the week as characters. Use modulo operator.
# week_day <- function(date){
#   date_input <- as.POSIXct(as.Date("01/01/1970"),"%m/%d/%Y")
#   days <- c("Thursday","Friday","Saturday","Sunday","Monday","Tuesday","Wednesday")
#   return(days$)
# }

#7Create a function that accepts a vector and and integer n and returns nth highest number
highest_num <- function(num,nth){
  return(dec_sort(num)[nth])
}

#Example_Test
highest_num(x,1)

#8Create a function that computes the compound interest of an investment given the rate, time, and initial amount or principal.
compound_int <- function(principal,rate,period){
  return(principal*(((1+rate)^period)-1))
}

#Example_Test
compound_int(50000,0.05,12)

#9Create a function isPrime(n) that accepts an integer and outputs a Boolean value (TRUE or FALSE) depending whether the integer is a prime number or not.

isPrime <- function(n){
  if (n == 2){
    return (TRUE)
  }
  else if (n > 2){
    temp <- 0
    for (i in 2:(n-1)){
      if (n %% i == 0){
        temp <- 1
      }
    }
    if (temp == 0){
      return(TRUE)  
    }
    else if (temp == 1){
      return(FALSE)
    }
  }
  else{
    return(FALSE)
  }  
}

#Example_Test
isPrime(1)
isPrime(2)
isPrime(6)
isPrime(17)




  

