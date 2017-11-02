v=readLines("test.txt")
### 1
length(grep("^[ABC]",v))
### 2
data=do.call(rbind,strsplit(v," "))
name=data[,1]
length((grep("[^aeiou]$",name)))
### 3
score=data[,2]
length(grep("(\\d)\\1+",score))
### 4
word=data[,3]
length(grep("[aeiou]{2}",word))
### 5
score.comma=do.call(rbind,strsplit(score,""))
newdata=cbind(name,score.comma,word)
newdata=as.data.frame(newdata)
for (i in 2:5){
  newdata[,i]=as.numeric(as.character(newdata[,i]))
}
score.new=newdata[,2:5]
str(score.new)
sum(apply(score.new,1,sum))
### 6
new.word=sub("([aeiou])([^aeiou])","\\2\\1",word)
length(grep("[aeiou]{2}",new.word))

