# Introduction

# Data types ----

# integer
int_value = 6L
class(int_value)

# numeric
num_value = 6.5
class(num_value)

# character
text_value = "hello"
class(text_value)

# logical (TRUE/FALSE or T/F)
logical_value = FALSE
class(logical_value)

# complex
complex_value = 2i
class(complex_value)


# R Programming Operators ----

# Arithmetic Operators

# +
5 + 5

# -
5 - 5

# *
5 * 5

# / 
5 / 5

# ^
5 ^ 5

# Logical Operators

# "=="  means equal to
# "!="  means not equal to
# "<" ; ">" ; "<=" ; ">="
# "&" is "and"
# "|" is "or" 
# "!" is "not"

# | - 'or' 
2 | 2

# & - 'and'
2 & 2

# == 
5 == 2 + 3

# !=
5 != 4
5 != 5

# > and <
(3 > 7) | (3 != 7)             
(3 > 7) & (3 != 7) 

# TRUE and FALSE
# if one of them true, the it returns true because "|" evaluates one of them not both of them.
TRUE | FALSE
FALSE | FALSE
F | T
T & F
T & T


# Miscellaneous operators ----

library(tidyverse) #install.packages("tidyverse")

#Pipe operator %>% (shift+ctrl+m)
sqrt(2)
abs(-65)
paste("1","2")
paste0("1","2")

2 %>% sqrt()
-65 %>% abs()
"1" %>% paste("2")
"1" %>% paste0("2")

# Match operator %in%
2 %in% -5:5 


# Assignment Operators "alt" + "-" ----

hello = 5 + 5
hello_2 <- 5 + 5
5 + 5 -> hello_3 


# Data Structures ----

# Vectors ----

MyFirstVector <- c(3, 45, 56, 732) #combine
MyFirstVector %>% class()

V2 <- c("a", "23", "B7")    
V2 %>% class()

V3 <- c(2, 8, 16, 23, "37,5")    
V3 %>% class()


seq(1, 15) #sequence - 1:15
seq(1, 15, 2)

rep(3, 50) #replicate
rep("a", 5) 

x <- c(80, 20)
y <- rep(x, 10) 


w <- c("a", "b", "c", "d", "e")

w[1]
w[2] ; w[3]
w[-1]                          

w[1:3] ; w[3:5]

w[c(1,3,5)]


c1 <- seq(1:9)
c2 <- -11:-19

cbind(c1,c2)

rbind(c1,c2)


# Tables ----

# Matrix

matrix_1 <- matrix(data = 1:9, ncol = 3, nrow = 3)
matrix_2 <- matrix(data = 1:9, ncol = 3, nrow = 3)
matrix_1
matrix_2

matrix_3 <- cbind(matrix_1,matrix_2)
matrix_4 <- rbind(matrix_1,matrix_2)
matrix_3;matrix_4

matrix_3[2,3]

matrix_3[2,]

matrix_3[,3]

matrix_3[2:3,c(1,6)]

# Data Frame

data_frame <- matrix_4 %>% as.data.frame()

data_frame[2,3]
data_frame[2,]
data_frame[,3]
data_frame[2:3,c(1,3)]

data_frame[2]

data_frame[[2]]

data_frame$V2

data_frame$V2[2]


# List ----

value <- "14"

vector <- c(2,19:26,seq(25,31))

table <- matrix(vector,4,4)

data <- table %>% as.data.frame()

list <- list(value,vector,table,data)

list[[1]]

list[[3]]

list[[2]][5]

list[[4]][3,4]

list[[4]][[3]]
