weather.data<- read.csv("C:/Users/000110888/OneDrive - CSULB/Desktop/weather_description.csv",
header=TRUE, sep=",")

LA<- weather.data$Los.Angeles

X3<- ifelse(LA=="sky is clear", "clear", ifelse(LA %in% c("broken clouds",
"few clouds", "overcast clouds", "scattered clouds"),"clouds", 
ifelse(LA %in% c("light intensity drizzle", "dust", "fog", 
"haze", "mist", "smoke", "drizzle"),"fog","rain")))


library(Hmisc) #Harrell Miscellaneous packages
X2<- Lag(X3,shift=1)
X1<- Lag(X3,shift=2)

sss<- ifelse(X1=="clear" & X2=="clear" & X3=="clear",1,0) 
css<- ifelse(X1=="clouds" & X2=="clear" & X3=="clear",1,0)
fss<- ifelse(X1=="fog" & X2=="clear" & X3=="clear",1,0) 
rss<- ifelse(X1=="rain" & X2=="clear" & X3=="clear",1,0) 
ssc<- ifelse(X1=="clear" & X2=="clear" & X3=="cloudy",1,0)
csc<- ifelse(X1=="clouds" & X2=="clear" & X3=="clouds",1,0) 
fsc<- ifelse(X1=="fog" & X2=="clear" & X3=="clouds",1,0) 
rsc<- ifelse(X1=="rain" & X2=="clear" & X3=="clouds",1,0)
ssf<- ifelse(X1=="clear" & X2=="clear" & X3=="fog",1,0)
csf<- ifelse(X1=="clouds" & X2=="clear" & X3=="fog",1,0)
fsf<- ifelse(X1=="fog" & X2=="clear" & X3=="fog",1,0)
rsf<- ifelse(X1=="rain" & X2=="clear" & X3=="fog",1,0)
ssr<- ifelse(X1=="clear" & X2=="clear" & X3=="rain",1,0)
csr<- ifelse(X1=="clouds" & X2=="clear" & X3=="rain",1,0)
fsr<- ifelse(X1=="fog" & X2=="clear" & X3=="rain",1,0)
rsr<- ifelse(X1=="rain" & X2=="clear" & X3=="rain",1,0)

#computing P(X3=s|X2=s,X1=s)
sum(sss)/sum(sss+ssc+ssf+ssr)

#computing P(X3=s|X2=s)
sum(sss+css+fss+rss)/sum(sss+css+fss+rss+ssc+csc+fsc+rsc+ssf+csf+fsf+rsf+ssr+csr+fsr+rsr)


# CHI-SQUARED TEST FOR MARKOV CHAIN #
# COMPUTING EXPECTED TRANSITION MATRIX ON FIRST 20% OF DATA #
X1.all<- X1
X2.all<- X2
X1<- X1.all[1:round(0.2*length(X1.all),0)]
X2<- X2.all[1:round(0.2*length(X2.all),0)]

ss.exp<- ifelse(X1=="clear" & X2=="clear",1,0) 
sc.exp<- ifelse(X1=="clear" & X2=="clouds",1,0)
sf.exp<- ifelse(X1=="clear" & X2=="fog",1,0)
sr.exp<- ifelse(X1=="clear" & X2=="rain",1,0)

cs.exp<- ifelse(X1=="clouds" & X2=="clear",1,0)
cc.exp<- ifelse(X1=="clouds" & X2=="clouds",1,0)
cf.exp<- ifelse(X1=="clouds" & X2=="fog",1,0)
cr.exp<- ifelse(X1=="clouds" & X2=="rain",1,0)

fs.exp<- ifelse(X1=="fog" & X2=="clear",1,0)
fc.exp<- ifelse(X1=="fog" & X2=="clouds",1,0)
ff.exp<- ifelse(X1=="fog" & X2=="fog",1,0)
fr.exp<- ifelse(X1=="fog" & X2=="rain",1,0)

rs.exp<- ifelse(X1=="rain" & X2=="clear",1,0)
rc.exp<- ifelse(X1=="rain" & X2=="clouds",1,0)
rf.exp<- ifelse(X1=="rain" & X2=="fog",1,0)
rr.exp<- ifelse(X1=="rain" & X2=="rain",1,0)

s.exp<- sum(ss.exp+sc.exp+sf.exp+sr.exp)
c.exp<- sum(cs.exp+cc.exp+cf.exp+cr.exp)
f.exp<- sum(fs.exp+fc.exp+ff.exp+fr.exp)
r.exp<- sum(rs.exp+rc.exp+rf.exp+rr.exp)

# COMPUTING OBSERVED TRANSITION MATRIX ON REMAINING 80% OF DATA #
X1<- X1.all[(round(0.2*length(X1.all),0)+1):length(X1.all)]
X2<- X2.all[(round(0.2*length(X2.all),0)+1):length(X2.all)]

ss.obs<- ifelse(X1=="clear" & X2=="clear",1,0) 
sc.obs<- ifelse(X1=="clear" & X2=="clouds",1,0)
sf.obs<- ifelse(X1=="clear" & X2=="fog",1,0)
sr.obs<- ifelse(X1=="clear" & X2=="rain",1,0)

cs.obs<- ifelse(X1=="clouds" & X2=="clear",1,0)
cc.obs<- ifelse(X1=="clouds" & X2=="clouds",1,0)
cf.obs<- ifelse(X1=="clouds" & X2=="fog",1,0)
cr.obs<- ifelse(X1=="clouds" & X2=="rain",1,0)

fs.obs<- ifelse(X1=="fog" & X2=="clear",1,0)
fc.obs<- ifelse(X1=="fog" & X2=="clouds",1,0)
ff.obs<- ifelse(X1=="fog" & X2=="fog",1,0)
fr.obs<- ifelse(X1=="fog" & X2=="rain",1,0)

rs.obs<- ifelse(X1=="rain" & X2=="clear",1,0)
rc.obs<- ifelse(X1=="rain" & X2=="clouds",1,0)
rf.obs<- ifelse(X1=="rain" & X2=="fog",1,0)
rr.obs<- ifelse(X1=="rain" & X2=="rain",1,0)

s.obs<- sum(ss.obs+sc.obs+sf.obs+sr.obs)
c.obs<- sum(cs.obs+cc.obs+cf.obs+cr.obs)
f.obs<- sum(fs.obs+fc.obs+ff.obs+fr.obs)
r.obs<- sum(rs.obs+rc.obs+rf.obs+rr.obs)

obs.matrix<- matrix(c(sum(ss.obs),sum(sc.obs),
sum(sf.obs),sum(sr.obs),sum(cs.obs),sum(cc.obs),
sum(cf.obs),sum(cr.obs),sum(fs.obs),sum(fc.obs),
sum(ff.obs),sum(fr.obs),sum(rs.obs),sum(rc.obs),
sum(rf.obs),sum(rr.obs)),nrow=4,ncol=4,byrow=TRUE)

print("Observed Transition Frequencies")
print(obs.matrix)

exp.matrix<- matrix(c(sum(ss.exp)/s.exp*s.obs,
sum(sc.exp)/s.exp*s.obs,sum(sf.exp)/s.exp*s.obs,
sum(sr.exp)/s.exp*s.obs,sum(cs.exp)/c.exp*c.obs,
sum(cc.exp)/c.exp*c.obs,sum(cf.exp)/c.exp*c.obs,
sum(cr.exp)/c.exp*c.obs,sum(fs.exp)/f.exp*f.obs,
sum(fc.exp)/f.exp*f.obs,sum(ff.exp)/f.exp*f.obs,
sum(fr.exp)/f.exp*f.obs,sum(rs.exp)/r.exp*r.obs,
sum(rc.exp)/r.exp*r.obs,sum(rf.exp)/r.exp*r.obs,
sum(rr.exp)/r.exp*r.obs),nrow=4,ncol=4,byrow=TRUE)

print("Expected Transition Frequencies")
print(exp.matrix)

# COMPUTING CHI-SQUARED STATISTIC AND P-VALUE #
chi.sq<- 0
 for (i in 1:4) {
    for (j in 1:4)
  chi.sq<- chi.sq+(obs.matrix[i,j]-exp.matrix[i,j])^2/exp.matrix[i,j]
 }

print(chi.sq)
print(p.value<- pchisq(chi.sq, df=12, lower.tail=FALSE))
