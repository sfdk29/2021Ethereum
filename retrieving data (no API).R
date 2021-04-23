library(stringr)
library(httr)
library(jsonlite)
library(ethr)
library(ether)
library(DescTools)
library(curl)

#hereinafter, 'smart contracts' will be refered as 'addresses'  
#retriving a list of ponzi addresses (for now, just ponzi addresses, later both ponzi and non-ponzi addresses)
ponzi.addresses <- read.csv("https://raw.githubusercontent.com//sfdk29//2021Ethereum//main//ponzi-addresses.csv",header = FALSE,stringsAsFactors=FALSE,colClasses=c("character","character"))
ponzi.addresses <- ponzi.addresses[,1] #these are the smart contract addresses

#we need an Etherscan API in order to retrieve information relating to each such address  
#API <- ??? 

#Now, using such API and the function 'GET' from the package 'httr', we will retrive information relating to:
#(i) 'normal' transactions of each address (these are exclusively incoming payments [i.e. amounts of Ethereum deposited into the smart contract]) 
#(ii) 'internal' transactions of each address (these are either incoming or outgoing payments [i.e. amounts of Ethereum deposited into or distributed by the smart contract])
#(iii) opcodes of each smart contract 


#(i) 'normal' transactions

#we first need to define the url specific to each address in order to retrieve information on the 'normal' transactions relating to each address
urls <- sapply(ponzi.addresses,function(u) paste0("https://api.etherscan.io/api?module=account&action=txlist&address=",u,"&startblock=0&endblock=99999999&sort=asc&apikey=",API))

#creating a list which will store the information retrieved from Etherscan.io  
strings.list <- as.list(NA)
for (j in 1:length(urls)){
  obj <- content(GET(urls[j]),"text")
  obj <- strsplit(obj,"\\{")
  obj <- unlist(obj) #check if this should actually remain a list
  strings.list[[j]] <- obj
}

#saving the list for convenience (since the API endpoint is throttled to 2 calls/second)
save(strings.list,file="C:\\Users\\zenob\\OneDrive\\Documenti\\ETH thesis\\strings.list.Rdata")
load("C:\\Users\\zenob\\OneDrive\\Documenti\\ETH thesis\\strings.list.Rdata")

#each element of the strings.list object is a vector of strings that relate to the 'normal' transactions of a smart contract 
#for instance, for smart contract no. 4, we have 
str(strings.list[[4]])
#note that the first three elements of this vector do not relate to actual incoming transactions: 
strings.list[[4]][1:3]
#whereas all other elements of the vector do relate to actual incoming transactions 
#e.g. 
strings.list[[4]][4]
strings.list[[4]][length(strings.list[[4]])]


strings.list2 <- as.list(NA)
for(i in 1:length(strings.list)){
  strings.list2[i] <- ifelse(length(strings.list[[i]])>3,strings.list[i],NA) 
}

list.obj <- as.list(rep(NA,length(strings.list2)))

for (j in 1:length(strings.list2)){
  obj <- strings.list2[[j]]
  if (length(obj)>3) {
    obj <- obj[-(1:3)]
    m <- matrix(NA,nrow=length(obj),ncol = 10)
    for (i in 1:length(obj)){
      from <- as.character(str_match(obj[i],"\"from\":\"\\s*(.*?)\\s*\""))
      from <- from[2] #no idea why but I need to not use a comma here 
      
      to <- as.character(str_match(obj[i],"\"to\":\"\\s*(.*?)\\s*\""))
      to <- to[2] #no idea why but I need to not use a comma here 
      
      a <- str_match(obj[i],"blockNumber\":\"\\s*(.*?)\\s*\"")
      a <- as.numeric(a[,2]) #once inserted into the matrix, will be forced to character vector 
      
      b <- str_match(obj[i],"timeStamp\":\"\\s*(.*?)\\s*\"")
      b <- as.numeric(b[,2]) #once inserted into the matrix, will be forced to character vector
      b.asdate <- as.POSIXct(b, origin="1970-01-01")
      
      c <- str_match(obj[i],"value\":\"\\s*(.*?)\\s*\"")
      c <- as.numeric(c[,2]) #once inserted into the matrix, will be forced to character vector
      
      d <- str_match(obj[i],"gas\":\"\\s*(.*?)\\s*\"")
      d <- as.numeric(d[,2]) #once inserted into the matrix, will be forced to character vector
      
      e <- str_match(obj[i],"gasPrice\":\"\\s*(.*?)\\s*\"")
      e <- as.numeric(e[,2]) #once inserted into the matrix, will be forced to character vector
      
      f <- str_match(obj[i],"cumulativeGasUsed\":\"\\s*(.*?)\\s*\"")
      f <- as.numeric(f[,2]) #once inserted into the matrix, will be forced to character vector
      
      g <- str_match(obj[i],"gasUsed\":\"\\s*(.*?)\\s*\"")
      g <- as.numeric(g[,2]) #once inserted into the matrix, will be forced to character vector
      m[i,] <- c(from,to,a,b,b.asdate,c,d,e,f,g)
    } 
    d <- data.frame(m[,1],m[,2],as.numeric(m[,3]),as.numeric(m[,4]),as.numeric(m[,5]),as.numeric(m[,6]),as.numeric(m[,7]),as.numeric(m[,8]),as.numeric(m[,9]),as.numeric(m[,10]))
    colnames(d) <- c("from","to","blockNumber","timeStamp","timeStamp.asdate","value","gas","gasPrice","cumulativeGasUsed","gasUsed")
    list.obj[[j]] <- d 
  }
  else {
    d <- data.frame(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
    colnames(d) <- c("from","to","blockNumber","timeStamp","timeStamp.asdate","value","gas","gasPrice","cumulativeGasUsed","gasUsed")
    list.obj[[j]] <- d
  }
}

#list.obj is a list whose elements are datasframes of 10 columns and as many rows as the number of 'normal' transactions to the smart contract  
str(list.obj)
#e.g. 
list.obj[183] #no transactions at all 
list.obj[184] #only five transactions 
list.obj[2] #many transactions ('successful' Ponzi smart contract)
#note the class of variables within each such dataframe 
class(list.obj[[184]][1,"from"]) #good
class(list.obj[[184]][1,"value"]) #good 


#(ii) 'internal' transactions
#retrievial process as in (i), yet keep in mind that 'internal' transactions might refer to both incoming and outcoming payments 

urls.internaltx <- sapply(ponzi.addresses,function(u) paste0("https://api.etherscan.io/api?module=account&action=txlistinternal&address=",u,"&startblock=0&endblock=2702578&sort=asc&apikey=",API))

strings.list.internaltx <- as.list(NA)
for (j in 1:length(urls.internaltx)){
  obj <- content(GET(urls.internaltx[j]),"text")
  obj <- strsplit(obj,"\\{")
  obj <- unlist(obj) #check if this should actually remain a list
  strings.list.internaltx[[j]] <- obj
}

save(strings.list.internaltx,file="C:\\Users\\zenob\\OneDrive\\Documenti\\ETH thesis\\strings.list.internaltx.Rdata")

#in this case, only the first two transactions are not relevant (whereas in the case of 'normal' transactions, the first three are not relevant)
strings.list.internaltx[[4]][1:2] #not relevant 
strings.list.internaltx[[4]][3] #first relevant 

strings.list.internaltx2 <- as.list(NA)
for(i in 1:length(strings.list.internaltx)){
  strings.list.internaltx2[i] <- ifelse(length(strings.list.internaltx[[i]])>2,strings.list.internaltx[i],NA) 
}

list.obj.internaltx <- as.list(rep(NA,length(strings.list.internaltx2)))


for (j in 1:length(strings.list.internaltx2)){
  obj <- strings.list.internaltx2[[j]]
  if (length(obj)>2) {
    obj <- obj[-(1:2)]
    m <- as.data.frame(matrix(NA,nrow=length(obj),ncol = 8)) #I need to use a data.frame as opposed to a matrix so to store both character and numeric values   
    for (i in 1:length(obj)){
      from <- str_match(obj[i],"\"from\":\"\\s*(.*?)\\s*\"")
      from <- from[2] #no idea why but I need to not use a comma here 
      
      to <- str_match(obj[i],"\"to\":\"\\s*(.*?)\\s*\"")
      to <- to[2] #no idea why but I need to not use a comma here 
      
      a <- str_match(obj[i],"blockNumber\":\"\\s*(.*?)\\s*\"")
      a <- as.numeric(a[,2])
      
      b <- str_match(obj[i],"timeStamp\":\"\\s*(.*?)\\s*\"")
      b <- as.numeric(b[,2])
      b.asdate <- as.POSIXct(b, origin="1970-01-01")
      
      c <- str_match(obj[i],"value\":\"\\s*(.*?)\\s*\"")
      c <- as.numeric(c[,2])
      
      d <- str_match(obj[i],"gas\":\"\\s*(.*?)\\s*\"")
      d <- as.numeric(d[,2])
      
      
      e <- str_match(obj[i],"gasUsed\":\"\\s*(.*?)\\s*\"")
      e <- as.numeric(e[,2])
      
      m[i,] <- c(from,to,a,b,b.asdate,c,d,e)
    } 
    d <- data.frame(m[,1],m[,2],as.numeric(m[,3]),as.numeric(m[,4]),as.numeric(m[,5]),as.numeric(m[,6]),as.numeric(m[,7]),as.numeric(m[,8]))  
    colnames(d) <- c("from","to","blockNumber","timeStamp","timeStamp.asdate","value","gas","gasUsed")
    list.obj.internaltx[[j]] <- d
  }
  else {
    d <- data.frame(NA,NA,NA,NA,NA,NA,NA,NA)
    colnames(d) <- c("from","to","blockNumber","timeStamp","timeStamp.asdate","value","gas","gasUsed")
    list.obj.internaltx[[j]] <- d
  }
}

class(list.obj.internaltx[[184]][1,"from"]) #good
class(list.obj.internaltx[[184]][1,"value"]) #good


###############
#Now we combine the information of both (i) and (ii) through certain indexes (e.g. Gini coefficient).
#the variables obtained will be the 'behavioural' variables that we'll feed into the classifier models 

dat <- matrix(NA,nrow = length(list.obj),ncol=4)

for (i in 1:length(list.obj)){
  if (!is.na(list.obj[[i]][1,1]) | !is.na(list.obj.internaltx[[i]][1,1])) {
    #gini index 
    incoming.amounts.normal <- list.obj[[i]][,"value"]
    incoming.amounts.internal <- list.obj.internaltx[[i]][,"value"] [ list.obj.internaltx[[i]][,"to"]==ponzi.addresses[i]]
    incoming.amounts <- c(incoming.amounts.normal,incoming.amounts.internal)
    dat[i,1] <- if(!is.na(Gini(incoming.amounts,unbiased = FALSE,na.rm = TRUE))) {Gini(incoming.amounts,unbiased = FALSE,na.rm = TRUE)} else {NA}
    
    #lifetime
    incoming.time.normal <- list.obj[[i]][,"timeStamp"]
    incoming.time.internal <- list.obj.internaltx[[i]][,"timeStamp"] [ list.obj.internaltx[[i]][,"to"]==ponzi.addresses[i]]
    incoming.time <- c(incoming.time.normal,incoming.time.internal)
    incoming.time <- ifelse(is.na(incoming.time),NA,sort(incoming.time)) #as I sort the vector, the NA is already excluded (and the length reduced by one)
    incomingtx.lifetime <- ifelse(is.na(incoming.time[1]),NA,range(incoming.time,na.rm = TRUE)[2] - range(incoming.time,na.rm = TRUE)[1])
    dat[i,2] <- incomingtx.lifetime #could be zero if we only have one transaction. Do we keep it 0 or force it to NA?
    
    #avereage time btw two transactions 
    dat[i,3] <- if (!is.nan(mean(diff(incoming.time)))) {mean(diff(incoming.time))} else {NA}
    
    #average gas expenditure #note that this is relevant only for 'normal' transactions, I supppose 
    gas <- list.obj[[i]][,"gas"]
    gas.price <- list.obj[[i]][,"gasPrice"]
    dat[i,4] <- mean(gas*gas.price)
  }
  else {
    dat[i,1] <- NA
    dat[i,2] <- NA
    dat[i,3] <- NA
    dat[i,4] <- NA
  }
}

colnames(dat) <- c("gini","lifespan","ave.time.btw.tx","gas.expenditure")
head(dat)

#(iii) retrieving opcodes 
###########

#we need an API from infura.io 
#infura.API <- ???
set_rpc_address(infura.API)

#list of opcodes (available at https://ethervm.io/)
opcodes <- c("STOP", "ADD", "MUL", "SUB", "DIV", "SDIV", "MOD", "SMOD", "ADDMOD", "MULMOD", "EXP", "SIGNEXTEND", "LT", "GT", "SLT", "SGT", "EQ", "ISZERO", "AND", "OR", "XOR", "NOT", "BYTE", "SHL", "SHR", "SAR", "SHA3", "ADDRESS", "BALANCE", "ORIGIN", "CALLER", "CALLVALUE", "CALLDATALOAD", "CALLDATASIZE", "CALLDATACOPY", "CODESIZE", "CODECOPY", "GASPRICE", "EXTCODESIZE", "EXTCODECOPY", "RETURNDATASIZE", "RETURNDATACOPY", "EXTCODEHASH", "BLOCKHASH", "COINBASE", "TIMESTAMP", "NUMBER", "DIFFICULTY", "GASLIMIT", "POP", "MLOAD", "MSTORE", "MSTORE8", "SLOAD", "SSTORE", "JUMP", "JUMPI", "PC", "MSIZE", "GAS", "JUMPDEST", "PUSH1", "PUSH2", "PUSH3", "PUSH4", "PUSH5", "PUSH6", "PUSH7", "PUSH8", "PUSH9", "PUSH10", "PUSH11", "PUSH12", "PUSH13", "PUSH14", "PUSH15", "PUSH16", "PUSH17", "PUSH18", "PUSH19", "PUSH20", "PUSH21", "PUSH22", "PUSH23", "PUSH24", "PUSH25", "PUSH26", "PUSH27", "PUSH28", "PUSH29", "PUSH30", "PUSH31", "PUSH32", "DUP1", "DUP2", "DUP3", "DUP4", "DUP5", "DUP6", "DUP7", "DUP8", "DUP9", "DUP10", "DUP11", "DUP12", "DUP13", "DUP14", "DUP15", "DUP16", "SWAP1", "SWAP2", "SWAP3", "SWAP4", "SWAP5", "SWAP6", "SWAP7", "SWAP8", "SWAP9", "SWAP10", "SWAP11", "SWAP12", "SWAP13", "SWAP14", "SWAP15", "SWAP16", "LOG0", "LOG1", "LOG2", "LOG3", "LOG4", "PUSH", "DUP", "SWAP", "CREATE", "CALL", "CALLCODE", "RETURN", "DELEGATECALL", "CREATE2", "STATICCALL", "REVERT", "SELFDESTRUCT")
save(opcodes,file="C:\\Users\\zenob\\OneDrive\\Documenti\\ETH thesis\\opcodes.Rdata")

m <- matrix(NA,nrow = length(ponzi.addresses),ncol = length(opcodes))
for (i in 1:length(ponzi.addresses)){
  address <- ponzi.addresses[i]
  url <- paste0("https://ethervm.io/decompile/",address,"#disassembly")
  obj <- content(GET(url),"text")
  vector.of.opcodes.counts <- str_count(obj,opcodes)
  m[i,] <- vector.of.opcodes.counts
}

colnames(m) <- opcodes
head(m)
opcodes.count.matrix <- m

save(opcodes.count.matrix,file="C:\\Users\\zenob\\OneDrive\\Documenti\\ETH thesis\\opcodes.count.matrix.Rdata")
load("C:\\Users\\zenob\\OneDrive\\Documenti\\ETH thesis\\opcodes.count.matrix.Rdata")

dat <- cbind(dat,opcodes.count.matrix)
head(dat)
