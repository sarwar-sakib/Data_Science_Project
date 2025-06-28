1+2
a<-2
b<-4
c<-a*b

#this is a comment
#if else
if (a>b){
  print("a is greater than b")
} else if (a==b){
  
}else { #else must start just after if statement, just like this
  print("b is greater")
}

#store <- ifelse(cond, st1, st2)

#switch-case
switch(2, "red","green", "blue")
switch("length", "color"="red", "length"=5)

#while-loop
i<-1

while (i<6){
  print(i)
  i<-i+1
}

#while-break
i<-2
while (i<6){
  print(i)
  i<-i+1
  if(i == 2){
    break
  }
}

#while-next
i<-2
while (i<6){
  i<-i+1
  if(i == 4){
    next
  }
  print(i)
}

#for loop
for (x in 1:10){
  print (x)
}

#nested for loop

for (x in 1:2){
  for (y in 1:3){
    print (x+y)
  }
}

#function

add_num <- function(a,b){
  sum <- a+b
  return (sum)
}
print (add_num(4,5))

#R has 6 type of data structure
#1. Vector
#One dimention array
#One vector can have same type of data, not combined
#i.e 
a<- c(1,2,3,4.5) 
b<- c("one","two")
c<- c(TRUE, FALSE)

print (c)

sort(a, decreasing = TRUE) #sorting a vector


#new lab 
#missed matrix

#Array
myArray<-array(1:18, c(2,3,4)) #1:18 1 to 18, c(2,3,4) rows, columns, num of arrays
myArray [1,2,1] #accesss array elements

#Data Frames
studentID <- c(1,2,3,4)
age <- c(22,23,24,25)
dept <-c("CSE","EEE","IPE","ENG")
cgpa <- c("good","very good","better","best")
studentInfo <- data.frame(studentID,age,dept,cgpa)
#add new column function
newCol <- cbind(studentInfo, attendance = c("irregular","regular","regular","regular"))
#new row
newrow <- rbind(studentInfo, c("studentID", 110, 110))

studentInfo[1]
studentInfo[["age"]]
studentInfo[1:2]

#Factors


#List
x<-"List Example"
y<- c(1,2,3)
z<- matrix(1:6, nrow=3)
k<- c("one","two")
mylist <- list(title=x, array=y,matrix=z,k)
mylist[1]


#USER INPUT (readline)
var1 = readline(prompt = "Enter any value: ") #readline takes string as default, need to convert if required
var2 = readline(prompt = "Enter any Number: ") #prompt used to show text to the console

var2 = as.integer(var2) #converted to integer 
print(var1)
print(var2)

#USER INPUT (scan)
x = scan() #takes input continuously, press enter twice to exit user input
print(x) #only numeric value

#other than numeric
d = scan(what = double())
s = scan(what = "" )

#Entering Data from keyboard/text editor

mydata <- data.frame(age=numeric(0),gender=character(0))
mydata <- edit(mydata)







