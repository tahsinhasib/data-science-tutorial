message <-"Md. Tahsin Hasib 22-46026-1"
print(message)

a <- 4
b <- 5
c <- a + b
d <- 10
neg <- -6
pi

print(a)
print(b)
print(c)
print(neg)


msg1 <- "my first msg"

print(msg1)
print(strsplit(msg1, split=" "))

print(a * b * c)    # 180
print(a + b + c)    # 18
print(a - b)    
print(d / b)
print(2^3)    # 8
print(2**3)   # 8


print(abs(neg))           # abs of neg
print(sqrt(c))            # sqrt of c
print(ceiling(sqrt(d)))   # ceil of sqrt(d)
print(floor(sqrt(d)))     # floor of sqrt(d)

print(toupper(msg1))      # uppercase
print(tolower("UPPERCASE TO LOWERCASE"))

x <- c(5,6,7,8)           # set numeric
y <- c("a", "b", "c", "d")

print(x)
print(min(x))   # min in a set
print(max(x))   # max in a set
print(sum(x))   # sum
print(mean(x))  # mean of a set
print(mode(x))  # mode of a set
print(sd(x))    # standard deviation
print(range(x))
print(mode(y))  # mode of a set
print(nchar(msg1)) # length of msg1
print(paste(msg1, message)) # merge

str <- "We are proud to be \"Aiubians\", from kuril."
print(cat(str))
print("This is escape character\n") #new line