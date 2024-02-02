#1
F <- factor(c("a", "b", "c", "a", "b", "d", "d", "e", "c", "a"))
F
print(table(F))

#2
X <- factor(c("a", "b", "c", "a", "b", "d", "d", "e", "c", "a"))
X
print(levels(X))

#3
X <- factor(c("a", "b", "c", "a", "b", "d", "d", "e", "c", "a"))
X
print(ordered(X, levels = c("d", "c", "b", "e", "a")))

#4
Y <- factor(c("America", "India", "China", "Korea", "Japan", "India", "Korea", "America"))
Y
print(as.list(levels(Y)))

#5
Q <- factor(c("a", "b", "c", "a", "b", "d", "d", "e", "c", "a"))
Q
levels(Q)[1] = "P"
print(Q)

#6
F <- factor(c("a", "b", "c", "a", "b"))
F
W <-  factor(c("d", "d", "e", "c", "a"))
W
print(factor(c(levels(F), levels(W))))

#7
c1 <- c("X", "B", "Y", "X")
c2 <- c(10, 2, 8, 6)
c3 <- c("A", "C", "P", "P")
c4 <- c(5.7, 8.5, 10.0, 3.5)
c5 <- c("a", "p", "x", "v")
c6 <- c(TRUE, FALSE, TRUE, FALSE)
df <- data.frame(c1, c2, c3, c4, c5, c6)
df$c1 <- factor(df$c1)
df$c2 <- factor(df$c2)
df$c3 <- factor(df$c3)
df$c4 <- factor(df$c4)
df$c5 <- factor(df$c5)
df$c6 <- factor(df$c6)
l$c1 <- levels(df$c1)
l$c2 <- levels(df$c2)
l$c3 <- levels(df$c3)
l$c4 <- levels(df$c4)
l$c5 <- levels(df$c5)
l$c6 <- levels(df$c6)
print(l$c1)
print(l$c2)
print(l$c3)
print(l$c4)
print(l$c5)
print(l$c6)

#8
n <- 5
height <- c(20, 10, 15, 20, 33)
width <- c(100, 12, 45, 12, 100)
age <- c(11, 20, 24, 20, 11)
df <- data.frame(height, width, age)
print(levels(factor(df$height))) # height
print(levels(factor(df$width))) #width
print(levels(factor(df$age))) # age

#9
data("iris")
str(iris)
iris$Species <- as.numeric(as.character(iris$Species))
str(iris)