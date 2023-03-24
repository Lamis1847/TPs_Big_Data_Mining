
p1=0.08
p0=0.02666
p12=p1-((0.01/3)*((p0+p1*1-2)*1 + (p0+p1*2-1)*2+(p0+p1*4-5)*4))
p02=p0-(0.01/3)*((p0+p1*1-2) + (p0+p1*2-1)+(p0+p1*4-5))
c1=(p02+p12*1-2)*(p02+p12*1-2)
c2=(p02+p12*1-2)*(p02+p12*1-2)
c3=(p02+p12*4-5)*(p02+p12*4-5)
e2= (1/(2*3))*(c1 +c2+c3)



slope <- function(p1,m,alpha, Hp,X,Y){
	result=p1
	somme=0
	i=1
	while(i<=m){
		somme=somme+(Hp[i]-Y[i])*X[i]*alpha/m
		i=i+1
	}
	return (p1-somme)
}
biais  <- function(p0,m,alpha, Hp,X,Y){
	result=p1
	somme=0
	i=1
	while(i<=m){
		somme=somme+(Hp[i]-Y[i])*alpha/m
		i=i+1
	}
	return (p0-somme)
}
hp<-function(X,p0,p1,m,Hp){
	i=1
	while(i<=m){
		Hp[i]=p0+p1*X[i]
		i=i+1
	}
}
h<-function(X,p0,p1){
	return (p0+p1*X)
		
}
erreur <- function(m,Hp,Y){
	somme=0
	i=1
	while(i<=m){
		somme=somme+(Hp[i]-Y[i])*(Hp[i]-Y[i])*1/(2*m)
		i=i+1
	}
	return (somme)
}
gradiant<- function(X,Y,alpha,m){
	j=0
	
	while(0==0){
		j=j+1
		p1=slope(p1,m,alpha,Hp,X,Y)
		p0=biais(p0,m,alpha,Hp,X,Y)
		i=1
		while(i<=m){
			Hp[i]=h(X[i],p0,p1)
			i=i+1
		}
		if(j==1){
			er=erreur(m,Hp,Y)
		}else{
			erp=er
			er=erreur(m,Hp,Y)
			if(erp-er==0){
				print("p0=")
				print(p0)
				print("p1=")
				print(p1)
				print("nb iteration=")
				print(j)

				break
			}
		}
		
		print(er)	
	}
	
}
################################
#programme principal
X=c(1,2,4)
Y=c(2,1,5)
Hp=c(0,0,0)
#data<-read.table(file.choose(),header=true)
#X=data[,1]
#Y=data[,2]

alpha=0.01
m=length(X)
p1=0
p0=0
gradiant(X,Y,alpha,m)
####################################################################
Regularisation de l'asso
erreurRegularisation <- function(m,Hp,Y,Pj,p,lampda){
	somme=0
	i=1
	sommepj=0
	for(k in 2:p){
		sommepj=sommepj+(Pj[k]*Pj[k])
	}
	sommepj
	sommepj=sommepj*lampda*1/(2*m)
	while(i<=m){
		somme=somme+(Hp[i]-Y[i])*(Hp[i]-Y[i])*1/(2*m)
		i=i+1
	}
	return (somme+sommepj)
}
gradiantRegularisation <- function(X,Y,alpha,m,Pj,p,lampda){
	j=0
	while(0==0){
		j=j+1
		p1=slope(p1,m,alpha,Hp,X,Y)
		p0=biais(p0,m,alpha,Hp,X,Y)
		i=1
		while(i<=m){
			Hp[i]=h(X[i],p0,p1)
			i=i+1
		}
		if(j==1){
			er=erreur(m,Hp,Y)
		}else{
			erp=er
			er=erreur(m,Hp,Y)
			if(erp-er==0){
				print("p0=")
				print(p0)
				print("p1=")
				print(p1)
				print("nb iteration=")
				print(j)

				break
			}
		}
		
		print(er)	
	}
	
}

####################################################################
X=c(1,2,4)
Y=c(2,1,5)
Hp=c(0,0,0)
alpha=0.01
m=3
p1=0
p0=0
gradiant(X,Y,alpha,m)



