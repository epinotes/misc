### From Rothman, Kenneth J. 2012. Epidemiology: An Introduction. New York: Oxford University Press.


########### standard error for risk (prevalence)
prev.ci<-function(a,N,level=.95,f=1){
se<-sqrt((a*(N-a))/N^3)
p<-a/N
z <- -qnorm((1-level)/2)
ll<-round((p-z*se)*f,2)
ul<-round((p+z*se)*f,2)
ci<-data.frame(count=a,pop=N,prev=round(p*f,2),SE=se*f,LL=ll,UL=ul)
print(ci)
}

#prev.ci(20,100,level=.90,f=1)

########### wilson cohfidence limits for binomial
wilson.ci <- function(a,N,level=.95,f=1){
#for smaller data
#simpler than the direct method 
p<-a/N
z <- -qnorm((1-level)/2)
moe <-((z/sqrt(N))*sqrt(p*(1-p) + z^2/(4*N)))/(1+z^2/N)
center <-(p+z^2/(2*N))/(1+z^2/N)
ll<-round((center-moe)*f,2)
ul<-round((center+moe)*f,2)
ci<-data.frame(count=a,pop=N,prev=round(p*f,2),LL=ll,UL=ul)
print(ci)
#print(data.frame(new.center=round(center*f,2),new.ME=moe*f))
}

#wilson.ci(1,20,level=.90)


########### standard error for incidence rate
incid.ci <- function(a,pt,level=.95,f=1){
#pt=person time
se<-sqrt((a/pt^2))
p<-a/pt
z <- -qnorm((1-level)/2)
ll<-round((p-z*se)*f,2)
ul<-round((p+z*se)*f,2)
ci<-data.frame(count=a,PT=pt,rate=round(p*f,2),SE=se*f,LL=ll,UL=ul)
print(ci)
}

#incid.ci(8,85000,level=.90,p=100000)

########### Byar confidence interval for incidence rate for small numbers
byar.ci <- function(a,pt,level=.95,f=1){
#pt=person time
p<-a/pt
a<-a+.5
z <- -qnorm((1-level)/2)
ll<-round((f*a*(1-(1/(9*a))-(z/3)*sqrt(1/a))^3)/pt,2)
ul<-round((f*a*(1-(1/(9*a))+(z/3)*sqrt(1/a))^3)/pt,2)
ci<-data.frame(count=a-.5,PT=pt,rate=round(p*f,2),LL=ll,UL=ul)
print(ci)
}
#byar.ci(3,2500,level=.90,f=10000)

########### confidence intervals for measures of effect
# cohort studies with risk data (prevalence): RR and RD
#              cases | people at risk
# exposed       a    |   N1
#               -------------
# unexposed     b    |   N0     

## Risk difference
RD.ci <- function(a,b,N0,N1,level=.95,f=1){
rd<-(a/N1)-(b/N0)
z <- -qnorm((1-level)/2)
se<-sqrt((a*(N1-a)/(N1^3))+(b*(N0-b)/(N0^3)))
ll<-round((rd-z*se)*f,2)
ul<-round((rd+z*se)*f,2)
ci<-data.frame(cases.exp=a,exposed=N1,RD=round(rd*f,2),SE=se*f,LL=ll,UL=ul)
print(ci)
}

#RD.ci(a=321,b=411,N0=689,N1=686,level=.9)

## Risk Ratio
RR.ci <- function(a,c,N1,N0,level=.95){
###      case |notCase|
#########-----------------
#exposed   a  | b     | N1
#not expo. c  | d     | N0
rr<-(a/N1)/(c/N0)
z <- -qnorm((1-level)/2)
se.ln<-sqrt((1/a)-(1/N1)+(1/c)-(1/N0))
ll<-round(exp(log(rr)-z*se.ln),2)
ul<-round(exp(log(rr)+z*se.ln),2)
ci<-data.frame(cases.exp=a,exposed=N1,RR=round(rr,2),log.SE=se.ln,LL=ll,UL=ul)
print(ci)
}

#RR.ci(a=321,b=411,N0=689,N1=686,level=.9)

# cohort studies with incidence data (incidence
# cohort studies with risk data (prevalence): RR and RD
#              cases | person time at risk
# exposed       a    |   PT1
#               -------------
# unexposed     b    |   PT0     


## Incidence density
ID.ci<-function(a,b,PT0,PT1,level=.95,f=1){
id<-(a/PT1)-(b/PT0)
z <- -qnorm((1-level)/2)
se<-sqrt((a/(PT1^2))+(b/(PT0^2)))
ll<-round((id-z*se)*f,2)
ul<-round((id+z*se)*f,2)
ci<-data.frame(cases.exp=a,exposed=PT1,ID=round(id*f,2),SE=se*f,LL=ll,UL=ul)
print(ci)
}

#ID.ci(a=136,b=1709,PT0=127650,PT1=22050,level=.9,f=1000)

##### Case control studies
# cohort studies with incidence data (incidence
# cohort studies with risk data (prevalence): RR and RD
#              cases | controls
# exposed       a    |   c
#               -------------
# unexposed     b    |   d     

OR.ci<-function(a,b,c,d,level=.95){
or<-(a/c)/(b/d)
z <- -qnorm((1-level)/2)
se.ln<-sqrt((1/a)+(1/b)+(1/c)+(1/d))
ll<-round(exp(log(or)-z*se.ln),2)
ul<-round(exp(log(or)+z*se.ln),2)
ci<-data.frame(cases=a,controls=c,OR=round(or,2),log.SE=se.ln,LL=ll,UL=ul)
print(ci)
}

#OR.ci(a=10,b=337,c=5,d=1016,level=.9)
