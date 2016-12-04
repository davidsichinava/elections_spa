### Source: 2012, Target: 2008
### 1. y.sc
### lambda.c: Take variable estimated from AW (vote count in 2008 boundaries)
### area.i - area of intersection btw source and target
### area.i*lambda.c
### sum of lambda.k (area weighted)*area of intersections btw source & k(without target zone)
### multiply by source count

### 2. lambda.c
### sum(y.sc)/area_t

### Calculate Ks:
df8_12$sum.k<-df8_12$aw
sum.k.df<-aggregate(.~PrecID_8, df8_12, sum)
sum.k.df<-subset(sum.k.df, select=c(PrecID_8, sum.k, area_8))
df8_12<-subset(df8_12, select=-c(sum.k))
df8_12<-merge(df8_12, sum.k.df, by=("PrecID_8"))
df8_12$sum.k<-abs(df8_12$sum.k-df8_12$aw)
df8_12$sum.k.area<-abs(df8_12$sum.k-df8_12$aw)
df8_12$sum.k<-(df8_12$sum.k/df8_12$sum.k.area)*df8_12$area_int

### 
y<-((df8_12$aw*df8_12$area_int)/df8_12$sum.k)*df8_12$UNM_12

y


### df8_12$l.t[df8_12$l.t==0]<-0.00001
### df8_12$y.st[df8_12$y.st==0]<-0.00001

#### Set up expectation maximization algorithm.
#### e-step
e.step<-function(data, l){
  ### Calculate yhat
  y<-((l*data$area_int)*data$UNM_12)/data$sum.k
}

#### m-step
m.step<-function(data, l, y){
  ### Plug yhat in the model and predict lambda
  m.st<-lm(l~y, data)
  l<-predict(m.st)
}

### Obtain initial parameters
l<-m.step(df8_12, df8_12$y.st, df8_12$l.t)


## iterate
step<-0
repeat {
## And assign convergence criteria
y<-e.step(df8_12, l)
olp<-l
l<-m.step(df8_12, y, l)
step<-step+1
if(abs(sum(olp-l))<0)
    break
}

abs(sum(olp-l))

df8_12<-cbind(df8_12, y, l)

res.em<-aggregate(.~PrecID_8, df8_12, sum)
res.em<-subset(res.em, select=c(PrecID_8, y))
names(res.em)<-c("PrecID_8", "tdw.interp")

