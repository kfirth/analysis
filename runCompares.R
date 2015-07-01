runCompares <- function(var, roundDigits=1, condVar=condL, compares=compareList){
	i <- 1;
	
	while (i < length(compares)){
		comp1 <- compares[i]
		comp2 <- compares[i+1]

		writeLines("*******************************")
		dispMeans=c(
			round(mean(var[condVar==comp1]), roundDigits),
			round(mean(var[condVar==comp2]), roundDigits)   )
 
		dispMedians=c(
			round(median(var[condVar==comp1]), roundDigits),
			round(median(var[condVar==comp2]) , roundDigits)  )

		output <- data.frame(mean=dispMeans, median=dispMedians, row.names=c(comp1, comp2))
		print(output)
		writeLines("")

		tout <- t.test(var[condVar==comp1], var[condVar==comp2])
		writeLines(paste("t-test p-value:", round(tout$p.value, 3)))
           
		wout <- wilcox.test(var[condVar==comp1], var[condVar==comp2])
		writeLines(paste("wilcox-test p-value:", round(wout$p.value, 3)))
 
		i <- i + 2
		writeLines("")
	}
}
