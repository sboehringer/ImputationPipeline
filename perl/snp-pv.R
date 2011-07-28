args = commandArgs(trailingOnly = TRUE)

#OPTION
outputFile=args[2]
inputDir=args[1]


prefix = args[4]
infoFile = paste(inputDir ,"/", prefix, ".info", sep="")
snps = readLines(infoFile)
NoT=1:args[5]


form = y ~ x;
ptypeFile = args[3]
table = read.delim(ptypeFile, header=TRUE, sep = " ", col.names=c("fid","iid","ptype"))
table$ptype = as.factor(table$ptype -1 )
          
for(i in snps){
	filename=paste(inputDir ,"/", prefix, ".", i, sep="")
	col1 = paste(i, ":G1", sep="")
	col2 = paste(i, ":G2", sep="")
	col3 = paste(i, ":G3", sep="")

	table= data.frame(table,read.delim(filename, header=FALSE, sep = " ", col.names=c(col1,col2,col3)))
}

int=4:6
TstatReal=0
for(k in seq(snps)){
	m = apply(table,1,function(row){	
		rmultinom(1,1,row[int])
	})

	gtypes = t(m) %*% c(-1,0,1)
	data = data.frame(y = table[,"ptype"], x = c(gtypes))
	
	res = glm(formula=form, data=data, family=binomial(link = "logit"))
	#summary(res)
	#coefficients(summary(res))
	TstatReal = TstatReal - log(coefficients(summary(res))["x", 4])
	int = int + 3
}

#TstatReal

Tstats=c()

for(j in NoT){

	int=4:6
	Tstats = append(Tstats, 0)
	permuted = sample(table[,"ptype"])
	for(k in seq(snps)){
		m = apply(table,1,function(row){	
			rmultinom(1,1,row[int])
		})
	
		gtypes = t(m) %*% c(-1,0,1)
		data = data.frame(y = permuted, x = c(gtypes))
		
		res = glm(formula=form, data=data, family=binomial(link = "logit"))
		Tstats[j] = Tstats[j] - log(coefficients(summary(res))["x", 4])
		int = int + 3
	}           
}


NoB=1
for(val in Tstats){
     if(val >= TstatReal) {NoB = NoB + 1}
}

write(NoB/(length(NoT) + 1), file=outputFile)

TstatReal
Tstats