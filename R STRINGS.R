
#1
X <- c("Bat", "Mat", "ABC", "XYZ", "PQR")
print(grep("at", X))

#2
a <- "A fine day"
print(str_length(a))

#3
#a
A <- "ABC"
B <- "DEF"
print(paste(A, B, sep =""))
#b
print(paste(c(A,B), collapse = ""))

#4
a <- c("ComputerScience", "Math", "Path", "ABC", "XYZ")
print(str_locate(a, "at"))

#5
a <- "A fine day"
print(str_split(a, "in"))

#6
Y <- c("bat", "cat", "xyz", "rat")
regexpr("at", Y)

#7
S <- "AstoryofLeo"
print(str_extract_all(S, "or"))

#8
M <- c("bat", "cat", "xyz", "rat")
print(M[3])  # Vector datastructure
Z <- matrix(c("abc", "def", "ghi", "klm"), nrow = 2)
print(Z[1, ])  # Matrix datastructure

#9
Q <- c("Apple", "Orange", "mango", "jackfruit", "pineapple")
print(str_extract_all(Q, "n."))

#10
Q <- c("Apple", "Orange", "mango", "jackfruit", "pineapple")
print(str_extract_all(Q, "n.."))

#11
T <- c("Apple", "Orange.", "mango", "jackfruit.", "pineapple")
print(grep("\\.", T, value = TRUE))

#12
a <- "A fine day"
str_replace_all(a, " ", "")

#13
library(stringr)
P <- c("A Story on #leo", "A Story on #iron", "A Story on #spaceX")
str_extract(P, "#[a-z]+")

#14
a <- "ComputerScience A fine day"
toupper(a)
tolower(a)

