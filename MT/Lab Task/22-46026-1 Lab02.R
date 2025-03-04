print("MD. TAHSIN HASIB 22-46026-1")


print("Here comes conditional Execution")

#if
a <- 5
b <- 10

if (a > b) {
  print("a greater than b")
}
print("b greater than a")



# if else
if (a > b) {
  print("a greater than b")
} else {
  print("b greater than a")
}

# if else if
c <- 10
d <- 10

if (c > d) {
  print("c greater than d")
} else if (c == d) {
  print("c is equal to d")
} else {
  print("d is greater than c")
}


# if-else
score <- 0.1
ifelse(score > 0.5, print("passed"), print("failed"))
outcome <- ifelse(score > 0.5, print("passed"), print("failed"))


# switch
switch(2, "red", "green", "blue")
switch("length", "color"="red", "shape"="square", "length"=5)


switch("sad",
       happy = "i am gladu are happy",
       sad = "cheer up",
       angry = "calm down now"
)

print("Here comes Loop")

#while 
i <- 1
while (i < 6) {
  print(i)
  i <- i + 1
}

# break satement
i <- 1
while (i < 6) {
  print(i)
  i <- i + 1
  if (i == 4) {
    break
  }
}


# next satement
i <- 1
while (i < 6) {
  i <- i + 1
  if (i == 4) {
    next
  }
  print(i)
}

# for
for (x in 1:10) {
  print(x)
}


fruits <- list("apple", "banana", "cherry")

for (x in fruits) {
  print(x)
}



# nested
for (x in 1:2) {
  for (y in 1:3) {
    print(x*y)
  }
}



print("Here comes functions")

# function
my_function <- function() {
  print("Hello function!")
}

my_function()

# function arguments
my_function <- function(x) {
  return (5 * x)
}

print(my_function(3))
print(my_function(5))
print(my_function(9))




# user input
var1 = readline(prompt = "Enter any number : ")
var2 = readline(prompt = "Enter any number : ")

var1 = as.integer(var1)
var2 = as.integer(var2)
print(var1+var2)


# scan
x = scan()
print(x)




