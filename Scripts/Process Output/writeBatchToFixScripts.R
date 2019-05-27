oldNewCol <- read.table("oldNewCol.txt", stringsAsFactors = FALSE)

files <- list.files(dirsGit$Scripts)
files <- files[!files == "ConsistentVariableNames.R"]
files <- files[!files == "writeBatchToFixScripts.R"]
oldNewCol$OldWithSpace <- paste("$",oldNewCol$V1," ",sep="")
oldNewCol$NewWithSpace <- paste("$",oldNewCol$V2," ",sep="")
oldNewCol$OldWithQuote <- paste("\"",oldNewCol$V1,"\"",sep="")
oldNewCol$NewWithQuote <- paste("\"",oldNewCol$V2,"\"",sep="")
oldNewCol$OldWithPar <- paste("$",oldNewCol$V1,")",sep="")
oldNewCol$NewWithPar <- paste("$",oldNewCol$V2,")",sep="")
oldNewCol$OldWithBracket <- paste("$",oldNewCol$V1,"]",sep="")
oldNewCol$NewWithBracket <- paste("$",oldNewCol$V2,"]",sep="")
oldNewCol$OldWithComma <- paste("$",oldNewCol$V1,",",sep="")
oldNewCol$NewWithComma <- paste("$",oldNewCol$V2,",",sep="")
oldNewCol$OldWithEquals <- paste("$",oldNewCol$V1,"=",sep="")
oldNewCol$NewWithEquals <- paste("$",oldNewCol$V2,"=",sep="")


out   <- paste("sed -i.bak 's/",oldNewCol$OldWithSpace,"/",oldNewCol$NewWithSpace,"/g' ",sep="")
out2   <- sapply(files, function(file) paste(out,file,sep=""), simplify = FALSE)
out2  <- unlist(out2)
out2  <- sub("//","/",out2)
write.table(out2, file="batchReplaceNames.txt", row.names = FALSE, quote = FALSE, col.names = FALSE)

out   <- paste("sed -i.bak 's/",oldNewCol$OldWithQuote,"/",oldNewCol$NewWithQuote,"/g' ",sep="")
out3   <- sapply(files, function(file) paste(out,file,sep=""), simplify = FALSE)
out3  <- unlist(out3)
write.table(out3, file="batchReplaceNames2.txt", row.names = FALSE, quote = FALSE, col.names = FALSE)

out   <- paste("sed -i.bak 's/",oldNewCol$OldWithPar,"/",oldNewCol$NewWithPar,"/g' ",sep="")
out4   <- sapply(files, function(file) paste(out,file,sep=""), simplify = FALSE)
out4  <- unlist(out4)
write.table(out4, file="batchReplaceNames3.txt", row.names = FALSE, quote = FALSE, col.names = FALSE)

out   <- paste("sed -i.bak 's/",oldNewCol$OldWithBracket,"/",oldNewCol$NewWithBracket,"/g' ",sep="")
out5   <- sapply(files, function(file) paste(out,file,sep=""), simplify = FALSE)
out5  <- unlist(out5)
write.table(out5, file="batchReplaceNames4.txt", row.names = FALSE, quote = FALSE, col.names = FALSE)

out   <- paste("sed -i.bak 's/",oldNewCol$OldWithComma,"/",oldNewCol$NewWithComma,"/g' ",sep="")
out6   <- sapply(files, function(file) paste(out,file,sep=""), simplify = FALSE)
out6  <- unlist(out6)
write.table(out6, file="batchReplaceNames5.txt", row.names = FALSE, quote = FALSE, col.names = FALSE)

out   <- paste("sed -i.bak 's/",oldNewCol$OldWithEquals,"/",oldNewCol$NewWithEquals,"/g' ",sep="")
out7   <- sapply(files, function(file) paste(out,file,sep=""), simplify = FALSE)
out7  <- unlist(out7)
write.table(out7, file="batchReplaceNames6.txt", row.names = FALSE, quote = FALSE, col.names = FALSE)

# In Plotting
files <- list.files(dirsGit$ScriptsPlot)

out   <- paste("sed -i.bak 's/",oldNewCol$OldWithSpace,"/",oldNewCol$NewWithSpace,"/g' ",sep="")
out2   <- sapply(files, function(file) paste(out,file,sep=""), simplify = FALSE)
out2  <- unlist(out2)
out2  <- sub("//","/",out2)
write.table(out2, file="batchReplaceNamesPlot.txt", row.names = FALSE, quote = FALSE, col.names = FALSE)

out   <- paste("sed -i.bak 's/",oldNewCol$OldWithQuote,"/",oldNewCol$NewWithQuote,"/g' ",sep="")
out3   <- sapply(files, function(file) paste(out,file,sep=""), simplify = FALSE)
out3  <- unlist(out3)
write.table(out3, file="batchReplaceNamesPlot2.txt", row.names = FALSE, quote = FALSE, col.names = FALSE)

out   <- paste("sed -i.bak 's/",oldNewCol$OldWithPar,"/",oldNewCol$NewWithPar,"/g' ",sep="")
out4   <- sapply(files, function(file) paste(out,file,sep=""), simplify = FALSE)
out4  <- unlist(out4)
write.table(out4, file="batchReplaceNamesPlot3.txt", row.names = FALSE, quote = FALSE, col.names = FALSE)

out   <- paste("sed -i.bak 's/",oldNewCol$OldWithBracket,"/",oldNewCol$NewWithBracket,"/g' ",sep="")
out5   <- sapply(files, function(file) paste(out,file,sep=""), simplify = FALSE)
out5  <- unlist(out5)
write.table(out5, file="batchReplaceNamesPlot4.txt", row.names = FALSE, quote = FALSE, col.names = FALSE)

out   <- paste("sed -i.bak 's/",oldNewCol$OldWithComma,"/",oldNewCol$NewWithComma,"/g' ",sep="")
out6   <- sapply(files, function(file) paste(out,file,sep=""), simplify = FALSE)
out6  <- unlist(out6)
write.table(out6, file="batchReplaceNamesPlot5.txt", row.names = FALSE, quote = FALSE, col.names = FALSE)

out   <- paste("sed -i.bak 's/",oldNewCol$OldWithEquals,"/",oldNewCol$NewWithEquals,"/g' ",sep="")
out7   <- sapply(files, function(file) paste(out,file,sep=""), simplify = FALSE)
out7  <- unlist(out7)
write.table(out7, file="batchReplaceNamesPlot6.txt", row.names = FALSE, quote = FALSE, col.names = FALSE)
