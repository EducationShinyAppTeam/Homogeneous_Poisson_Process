library(ggplot2)

x.value = matrix(0, nrow = 5, ncol = 100)
y.value = matrix(0, nrow = 5, ncol = 100)

for (j in 1:5){
  x = rexp(100,2)
  m = cumsum(x)
  h = 1:100
  for (i in 1:100){
    x.value[j,i] = m[i] 
  }
}
k=array()
for(i in 1:100){
  k[i] = m[i]
}
x.value
y.value

arr = c(0,x.value[1,])
interarr = array()
for (i in 1:100){
  interarr[i] = arr[i+1] - arr[i]
}

hist(interarr,breaks = 20)

if (i == 1){
  plot(x.value[i,], resi.value[i,], xlim = range(x.value[i,]), ylim = range(resi.value[i,]),
       xlab="t", ylab="N(t)-E(N(t))", main = "Residuals Plot", col = colors[i], pch=16)
  m = x.value[i,]
  resi = resi.value[i,]
  lines(m[order(m)], resi[order(m)], xlim=range(m), ylim=range(resi), pch=16, col = colors[i])
}
if (i > 1){
  plot(x.value[i,], resi.value[i,], xlim = range(x.value[i,]), ylim = range(resi.value[i,]),
       xlab="t", ylab="N(t)-E(N(t))", main = "Residuals Plot", col = colors[i], pch=16)
  m = x.value[i,]
  resi = resi.value[i,]
  lines(m[order(m)], resi[order(m)], xlim=range(m), ylim=range(resi), pch=16, col = colors[i])
}

for (i in 1:2){
  if (i == 1){
    plot(x.value[i,], resi.value[i,], xlim = range(x.value[i,]), ylim = range(resi.value[i,]),
         xlab="t", ylab="N(t)-E(N(t))", main = "Residuals Plot", col = colors[i], pch=16)
    m = x.value[i,]
    resi = resi.value[i,]
    lines(m[order(m)], resi[order(m)], xlim=range(m), ylim=range(resi), pch=16, col = colors[i])
  }
  if (i > 1){
    plot(x.value[i,], resi.value[i,], xlim = range(x.value[i,]), ylim = range(resi.value[i,]),
         xlab="t", ylab="N(t)-E(N(t))", main = "Residuals Plot", col = colors[i], pch=16)
    m = x.value[i,]
    resi = resi.value[i,]
    lines(m[order(m)], resi[order(m)], xlim=range(m), ylim=range(resi), pch=16, col = colors[i])
  }
}

x.value = matrix(0, nrow = 100, ncol = 5)
y.value = matrix(0, nrow = 100, ncol = 5)
arr = matrix(0,nrow=100,ncol=5)
for (i in 1:5){
  x = rexp(100,2)
  m = cumsum(x)
  for (j in 1:100){
    x.value[j,i] = m[j] 
  }
  
}

arr = rbind(matrix(0,nrow=1,ncol=5),x.value)
inter.arr = matrix(0,100,5)

for (i in 1:5){
  for (j in 1:100){
    inter.arr[j,i] = arr[j+1,i] - arr[j,i]
  } 
}

colnames(inter.arr) = c("a","b","c","d","e")
for(i in 1:5){
  if (i == 1){
    hist(inter.arr[,1],breaks = 20,method = "stack")
  }
  if(i > 1){
    
  }
}