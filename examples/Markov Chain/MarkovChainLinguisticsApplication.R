#####################################################
###### "EUGENE ONEGIN" BY ALEXANDER S. PUSHKIN ######
#####################################################
library(tidyverse)
text<- read_file("C:/Users/000110888/OneDrive - CSULB/Desktop/Onegin.txt")

#text cleaning
#gsub() = global substitution function=replaces all instances
lowercase<- tolower(text)
no.blanks<- gsub(" ","",lowercase)
no.line.breaks<- gsub("\r\n", "", no.blanks)
no.punctuation<- gsub("[[:punct:]]","",no.line.breaks)
no.numbers <- gsub("[0-9]","",no.punctuation)
no.soft.signs<- gsub("ь", "", no.numbers)
clean.string<- gsub("ъ","", no.soft.signs) #removes all hard signs

#splitting single string into characters
x2<- strsplit(clean.string, "")[[1]]

#shifting string by one position
no.last<- substr(clean.string, 1, nchar(clean.string)-1)
first.blank<- str_c(" ", no.last)
x1<- strsplit(first.blank,"")[[1]]

#Note: In pre-1918 Russian language "й" was considered a vowel
vowels<-c("а","е","ё","и","і","й","о","у","ы","ѣ","э","ю","я")
consonants<- c("б","в","г","д","ж","з","к","л","м","н","п","р",
"с","т","ф","х","ц","ч","ш","щ","ѳ")

#computing number of vowels, consonants, and four combinations
vv<- 0
vc<- 0
cv<- 0
cc<- 0
for (counter in 1:nchar(clean.string)){
vv<- vv+ifelse(x1[[counter]] %in% vowels & x2[[counter]] %in% vowels,1,0)
vc<- vc+ifelse(x1[[counter]] %in% vowels & x2[[counter]] %in% consonants,1,0)
cv<- cv+ifelse(x1[[counter]] %in% consonants & x2[[counter]] %in% vowels,1,0)
cc<- cc+ifelse(x1[[counter]] %in% consonants & x2[[counter]] %in% consonants,1,0)
}

#specifying the transition probability matrix
tm<- matrix(c(round(vv/(vv+vc),4), round(vc/(vv+vc),4), 
round(cv/(cv+cc),4), round(cc/(cv+cc),4)), nrow=2, ncol=2, byrow=TRUE)
print(tm)

#####################################################
###### "A TALE OF TWO CITIES" BY CHARLES DICKENS ######
#####################################################
library(tidyverse)
text <- read_file("C:/Users/000110888/OneDrive - CSULB/Desktop/ATaleOfTwoCities.txt")

lowercase<- tolower(text)
no.blanks<- gsub(" ","",lowercase)
no.line.breaks<- gsub("\r\n", "", no.blanks)
no.punctuation<- gsub("[[:punct:]]","", no.line.breaks)
clean.string <- gsub("[0-9]","",no.punctuation)
x2<- strsplit(clean.string, "")[[1]]
no.last<- substr(clean.string, 1, nchar(clean.string)-1)
first.blank<- str_c(" ", no.last)
x1<- strsplit(first.blank,"")[[1]]

vowels<-c("a","e","i","o","u")
consonants<- c("b","c","d","f","g","h","j","k","l","m","n","p","q","r",
"s","t","v","w","x","y","z")
vv<- 0
vc<- 0
cv<- 0
cc<- 0
for (counter in 1:nchar(clean.string)){
  vv<- vv+ifelse(x1[[counter]] %in% vowels & x2[[counter]] %in% vowels,1,0)
  vc<- vc+ifelse(x1[[counter]] %in% vowels & x2[[counter]] %in% consonants,1,0)
  cv<- cv+ifelse(x1[[counter]] %in% consonants & x2[[counter]] %in% vowels,1,0)
  cc<- cc+ifelse(x1[[counter]] %in% consonants & x2[[counter]] %in% consonants,1,0)
}

tm<- matrix(c(round(vv/(vv+vc),4), round(vc/(vv+vc),4), 
round(cv/(cv+cc),4), round(cc/(cv+cc),4)), nrow=2, ncol=2, byrow=TRUE)
print(tm)

############################################################
###### CIEN ANOS DE SOLEDAD BY GABRIEL GARCIA MARQUEZ ######
############################################################
library(tidyverse)
text <- read_file("C:/Users/000110888/OneDrive - CSULB/Desktop/CienAnosDeSoledad.txt")

lowercase<- tolower(text)
no.blanks<- gsub(" ","",lowercase)
no.line.breaks<- gsub("\r\n", "", no.blanks)
no.punctuation<- gsub("[[:punct:]]","", no.line.breaks)
clean.string <- gsub("[0-9]","",no.punctuation)
x2<- strsplit(clean.string, "")[[1]]
no.last<- substr(clean.string, 1, nchar(clean.string)-1)
first.blank<- str_c(" ", no.last)
x1<- strsplit(first.blank,"")[[1]]

vowels<-c("a","e","i","o","u")
consonants<- c("b","c","d","f","g","h","j","k","l","m","n","p","q","r",
"s","t","v","w","x","y","z")

vv<- 0
vc<- 0
cv<- 0
cc<- 0
for (counter in 1:nchar(clean.string)){
  vv<- vv+ifelse(x1[[counter]] %in% vowels & x2[[counter]] %in% vowels,1,0)
  vc<- vc+ifelse(x1[[counter]] %in% vowels & x2[[counter]] %in% consonants,1,0)
  cv<- cv+ifelse(x1[[counter]] %in% consonants & x2[[counter]] %in% vowels,1,0)
  cc<- cc+ifelse(x1[[counter]] %in% consonants & x2[[counter]] %in% consonants,1,0)
}

tm<- matrix(c(round(vv/(vv+vc),4), round(vc/(vv+vc),4), 
round(cv/(cv+cc),4), round(cc/(cv+cc),4)), nrow=2, ncol=2, byrow=TRUE)
print(tm)


#########################################################
###### LE PETIT PRINCE BY ANTOINE DE SAINT-EXUPERY ######
#########################################################
library(tidyverse)
text <- read_file("C:/Users/000110888/OneDrive - CSULB/Desktop/LePetitPrince.txt")

lowercase<- tolower(text)
no.blanks<- gsub(" ","",lowercase)
no.line.breaks<- gsub("\r\n", "", no.blanks)
nopunct.string<- gsub("[[:punct:]]","", no.line.breaks)
clean.string <- gsub("[0-9]","", nopunct.string)
x2<- strsplit(clean.string, "")[[1]]
no.last<- substr(clean.string, 1, nchar(clean.string)-1)
first.blank<- str_c(" ", no.last)
x1<- strsplit(first.blank,"")[[1]]

vowels<-c("a","e","i","o","u")
consonants<- c("b","c","d","f","g","h","j","k","l","m","n","p","q","r",
               "s","t","v","w","x","y","z")

vv<- 0
vc<- 0
cv<- 0
cc<- 0
for (counter in 1:nchar(clean.string)){
  vv<- vv+ifelse(x1[[counter]] %in% vowels & x2[[counter]] %in% vowels,1,0)
  vc<- vc+ifelse(x1[[counter]] %in% vowels & x2[[counter]] %in% consonants,1,0)
  cv<- cv+ifelse(x1[[counter]] %in% consonants & x2[[counter]] %in% vowels,1,0)
  cc<- cc+ifelse(x1[[counter]] %in% consonants & x2[[counter]] %in% consonants,1,0)
}

tm<- matrix(c(round(vv/(vv+vc),4), round(vc/(vv+vc),4), 
              round(cv/(cv+cc),4), round(cc/(cv+cc),4)), nrow=2, ncol=2, byrow=TRUE)
print(tm)

#####################################################
###### "MOBY DICK" BY HERMAN MELVILLE ######
#####################################################
library(tidyverse)
text <- read_file("C:/Users/000110888/OneDrive - CSULB/Desktop/MobyDick.txt")

lowercase<- tolower(text)
no.blanks<- gsub(" ","",lowercase)
no.line.breaks<- gsub("\r\n", "", no.blanks)
nopunct.string<- gsub("[[:punct:]]","", no.line.breaks)
clean.string <- gsub("[0-9]","", nopunct.string)
x2<- strsplit(clean.string, "")[[1]]
no.last<- substr(clean.string, 1, nchar(clean.string)-1)
first.blank<- str_c(" ", no.last)
x1<- strsplit(first.blank,"")[[1]]

vowels<-c("a","e","i","o","u")
consonants<- c("b","c","d","f","g","h","j","k","l","m","n","p","q","r",
               "s","t","v","w","x","y","z")
vv<- 0
vc<- 0
cv<- 0
cc<- 0
for (counter in 1:nchar(clean.string)){
  vv<- vv+ifelse(x1[[counter]] %in% vowels & x2[[counter]] %in% vowels,1,0)
  vc<- vc+ifelse(x1[[counter]] %in% vowels & x2[[counter]] %in% consonants,1,0)
  cv<- cv+ifelse(x1[[counter]] %in% consonants & x2[[counter]] %in% vowels,1,0)
  cc<- cc+ifelse(x1[[counter]] %in% consonants & x2[[counter]] %in% consonants,1,0)
}

tm<- matrix(c(round(vv/(vv+vc),4), round(vc/(vv+vc),4), 
              round(cv/(cv+cc),4), round(cc/(cv+cc),4)), nrow=2, ncol=2, byrow=TRUE)
print(tm)

#########################################################
###### "CRIME AND PUNISHMENT" BY FYODOR DOSTOEVSKY ######
#########################################################
library(tidyverse)
text<- read_file("C:/Users/000110888/OneDrive - CSULB/Desktop/CrimeAndPunishment.txt")

#text cleaning
#gsub() = global substitution function=replaces all instances
lowercase<- tolower(text)
no.blanks<- gsub(" ","",lowercase)
no.line.breaks<- gsub("\r\n", "", no.blanks)
no.punctuation<- gsub("[[:punct:]]","", no.line.breaks)
no.numbers <- gsub("[0-9]","",no.punctuation)
no.soft.signs<- gsub("ь", "", no.numbers)
clean.string<- gsub("ъ","", no.soft.signs) #removes all hard signs

#splitting single string into characters
x2<- strsplit(clean.string, "")[[1]]

#shifting string by one position
no.last<- substr(clean.string, 1, nchar(clean.string)-1)
first.blank<- str_c(" ", no.last)
x1<- strsplit(first.blank,"")[[1]]

#Note: In pre-1918 Russian language "й" was considered a vowel
vowels<-c("а","е","ё","и","і","й","о","у","ы","ѣ","э","ю","я")
consonants<- c("б","в","г","д","ж","з","к","л","м","н","п","р",
               "с","т","ф","х","ц","ч","ш","щ","ѳ")

#computing number of vowels, consonants, and four combinations
vv<- 0
vc<- 0
cv<- 0
cc<- 0
for (counter in 1:nchar(clean.string)){
  vv<- vv+ifelse(x1[[counter]] %in% vowels & x2[[counter]] %in% vowels,1,0)
  vc<- vc+ifelse(x1[[counter]] %in% vowels & x2[[counter]] %in% consonants,1,0)
  cv<- cv+ifelse(x1[[counter]] %in% consonants & x2[[counter]] %in% vowels,1,0)
  cc<- cc+ifelse(x1[[counter]] %in% consonants & x2[[counter]] %in% consonants,1,0)
}

#specifying the transition probability matrix
tm<- matrix(c(round(vv/(vv+vc),4), round(vc/(vv+vc),4), 
              round(cv/(cv+cc),4), round(cc/(cv+cc),4)), nrow=2, ncol=2, byrow=TRUE)
print(tm)

################################################
###### DON QUIXOTE BY MIGUEL DE CERVANTES ######
################################################
library(tidyverse)
text <- read_file("C:/Users/000110888/OneDrive - CSULB/Desktop/DonQuijote.txt")

lowercase<- tolower(text)
no.blanks<- gsub(" ","",lowercase)
no.line.breaks<- gsub("\r\n", "", no.blanks)
no.punctuation<- gsub("[[:punct:]]","", no.line.breaks)
clean.string <- gsub("[0-9]","",no.punctuation)
x2<- strsplit(clean.string, "")[[1]]
no.last<- substr(clean.string, 1, nchar(clean.string)-1)
first.blank<- str_c(" ", no.last)
x1<- strsplit(first.blank,"")[[1]]

vowels<-c("a","e","i","o","u")
consonants<- c("b","c","d","f","g","h","j","k","l","m","n","p","q","r",
               "s","t","v","w","x","y","z")

vv<- 0
vc<- 0
cv<- 0
cc<- 0
for (counter in 1:nchar(clean.string)){
  vv<- vv+ifelse(x1[[counter]] %in% vowels & x2[[counter]] %in% vowels,1,0)
  vc<- vc+ifelse(x1[[counter]] %in% vowels & x2[[counter]] %in% consonants,1,0)
  cv<- cv+ifelse(x1[[counter]] %in% consonants & x2[[counter]] %in% vowels,1,0)
  cc<- cc+ifelse(x1[[counter]] %in% consonants & x2[[counter]] %in% consonants,1,0)
}

tm<- matrix(c(round(vv/(vv+vc),4), round(vc/(vv+vc),4), 
              round(cv/(cv+cc),4), round(cc/(cv+cc),4)), nrow=2, ncol=2, byrow=TRUE)
print(tm)

############################################
###### LES MISERABLES BY VICTOR HUGO  ######
############################################
library(tidyverse)
text <- read_file("C:/Users/000110888/OneDrive - CSULB/Desktop/LesMiserables.txt")

lowercase<- tolower(text)
no.blanks<- gsub(" ","",lowercase)
no.line.breaks<- gsub("\r\n", "", no.blanks)
nopunct.string<- gsub("[[:punct:]]","", no.line.breaks)
clean.string <- gsub("[0-9]","", nopunct.string)
x2<- strsplit(clean.string, "")[[1]]
no.last<- substr(clean.string, 1, nchar(clean.string)-1)
first.blank<- str_c(" ", no.last)
x1<- strsplit(first.blank,"")[[1]]

vowels<-c("a","e","i","o","u")
consonants<- c("b","c","d","f","g","h","j","k","l","m","n","p","q","r",
               "s","t","v","w","x","y","z")

vv<- 0
vc<- 0
cv<- 0
cc<- 0
for (counter in 1:nchar(clean.string)){
  vv<- vv+ifelse(x1[[counter]] %in% vowels & x2[[counter]] %in% vowels,1,0)
  vc<- vc+ifelse(x1[[counter]] %in% vowels & x2[[counter]] %in% consonants,1,0)
  cv<- cv+ifelse(x1[[counter]] %in% consonants & x2[[counter]] %in% vowels,1,0)
  cc<- cc+ifelse(x1[[counter]] %in% consonants & x2[[counter]] %in% consonants,1,0)
}

tm<- matrix(c(round(vv/(vv+vc),4), round(vc/(vv+vc),4), 
              round(cv/(cv+cc),4), round(cc/(cv+cc),4)), nrow=2, ncol=2, byrow=TRUE)
print(tm)
