library(XML)
library(tseries)
library(zoo)

options(stringsAsFactors=FALSE)

getStockList <- function(){
  NASDAQ <- read.csv("http://www.nasdaq.com/screening/companies-by-industry.aspx?exchange=NASDAQ&render=download", as.is=c(1:10),na.strings=c("n/a","NA"))
  NYSE <- read.csv("http://www.nasdaq.com/screening/companies-by-industry.aspx?exchange=NYSE&render=download", as.is=c(1:10),na.strings=c("n/a","NA"))
  AMEX <- read.csv("http://www.nasdaq.com/screening/companies-by-industry.aspx?exchange=AMEX&render=download", as.is=c(1:10),na.strings=c("n/a","NA"))
  #Junta todas as bolsas na mesma tabela
  Stocks <- rbind(AMEX,NASDAQ,NYSE)
  
  return(Stocks)
}

getIndustries <- function(){
  
  #Tabulas as ações das Bolsas
  NASDAQ <- read.csv("http://www.nasdaq.com/screening/companies-by-industry.aspx?exchange=NASDAQ&render=download", as.is=c(1:10),na.strings=c("n/a","NA"))
  NYSE <- read.csv("http://www.nasdaq.com/screening/companies-by-industry.aspx?exchange=NYSE&render=download", as.is=c(1:10),na.strings=c("n/a","NA"))
  AMEX <- read.csv("http://www.nasdaq.com/screening/companies-by-industry.aspx?exchange=AMEX&render=download", as.is=c(1:10),na.strings=c("n/a","NA"))
  #Junta todas as bolsas na mesma tabela
  Stocks <- rbind(AMEX,NASDAQ,NYSE)
  
  #Cria dicionário de Indústrias
  industries <-unique(Stocks[,8])
  industries <- industries[order(industries)]
  return(industries)
}

getKS <- function(stock){
  #Abre o site
  theurl <- paste("http://finance.yahoo.com/q/ks?s=", stock,"+Key+Statistics", sep="")
  tables <- readHTMLTable(theurl)
  
  # EV
  table <- as.data.frame(tables[[9]])
  EV <- convertBtoM(as.character(table$V2)[2])
  
  #DE, Debt, Cash
  table <- as.data.frame(tables[[20]])
  DE <- as.numeric(as.character(table$V2[5]))
  Debt <- convertBtoM(as.character(table$V2)[4])
  Cash <- convertBtoM(as.character(table$V2)[2])
  
  #Beta
  table <- as.data.frame(tables[[25]])
  temp <- as.character(table$V2)[3]
  Beta <- as.numeric(substr(temp,1,nchar(temp)-1))
  if (substr(temp,nchar(temp),nchar(temp))=='B') Beta <- (Beta*1000)
  
  #DLPL e Equity
  DLPL <- NA
  try(DLPL <- (Debt-Cash)/(Debt/(DE/100)),silent=TRUE)
  if (!is.numeric(DLPL)) DLPL <- NA
  Equity <- NA
  try(Equity <- Debt/(DE/100),silent=TRUE)
  if (!is.numeric(Equity)) Equity <- NA
  dif <- NA
  try(dif <- EV-Debt-Equity, silent=TRUE)
  
  #MARGEM EBITDA
  table <- as.data.frame(tables[[18]])
  EBITDA <- convertBtoM(as.character(table$V2)[6])  
  Revenue <- convertBtoM(as.character(table$V2)[2])
  MRGEBITDA <- EBITDA/Revenue
  
  #Resultados
  result <- list(EV=EV, DE=DE, Debt=Debt, Cash=Cash, Beta=Beta, DLPL=DLPL, Equity=Equity, dif=dif, MRGEBITDA=MRGEBITDA, EBITDA=EBITDA, Revenue=Revenue)
  
  return(result)
}

convertBtoM <- function(value) {
  result <- value
  if (substr(value,nchar(value),nchar(value))=='B'){
    result <- (as.numeric(substr(value,1,nchar(value)-1))*1000)
  }
  else if (substr(value,nchar(value),nchar(value))=='M'){
    result <- (as.numeric(substr(value,1,nchar(value)-1)))
  }
  else if (substr(value,nchar(value),nchar(value))=='k'){
    result <- (as.numeric(substr(value,1,nchar(value)-1))/1000)
  }
  return(as.numeric(result))
}

getSymbols <- function(industry, minIPOyear, exclude){
  if(missing(minIPOyear)) {
    y <- 2007 
  } else {
    y <- minIPOyear }  
  
  if(missing(exclude)) {
    exclude <- "" 
  }
  
  #Tabulas as ações das Bolsas
  NASDAQ <- read.csv("http://www.nasdaq.com/screening/companies-by-industry.aspx?exchange=NASDAQ&render=download", as.is=c(1:10),na.strings=c("n/a","NA"))
  NYSE <- read.csv("http://www.nasdaq.com/screening/companies-by-industry.aspx?exchange=NYSE&render=download", as.is=c(1:10),na.strings=c("n/a","NA"))
  AMEX <- read.csv("http://www.nasdaq.com/screening/companies-by-industry.aspx?exchange=AMEX&render=download", as.is=c(1:10),na.strings=c("n/a","NA"))
  #Junta todas as bolsas na mesma tabela
  Stocks <- rbind(AMEX,NASDAQ,NYSE)
  
  #Cria dicionário de Indústrias
  industries <-unique(Stocks[,8])
  industries <- industries[order(industries)]
  
  #Separa as ações que são daquela industria e com IPO antes de 2007
  symbols <- as.data.frame(subset(Stocks, Industry==industry & (is.na(IPOyear)|IPOyear < y)))
  symbols <- as.data.frame(symbols[,1])
  symbols <- subset(symbols, !(symbols[,1] %in% exclude))
  
  symbols <- symbols[,1]
  
  return(symbols)
}

getQuote <- function(stock, ndays){
  quote <-get.hist.quote(stock, start = Sys.Date() - ndays, quote="Close")
  return(quote)
}

getBeta <- function(stock, years){
  SNP <-get.hist.quote("^GSPC", start = Sys.Date() - (years*365), quote="Close", quiet=TRUE)
  SNP <- ((SNP/lag(SNP,-1))-1)
  temp <- try(get.hist.quote(stock, start = Sys.Date() - (years*365), quote="Close", quiet=TRUE))
  if(!inherits(temp, "try-error")) temp <- (temp/lag(temp,-1)-1)
  try(beta <- (cov(SNP, temp)/var(SNP)),silent=TRUE)
  if (!is.numeric(beta)) beta <-NA
  return(beta)
}

getBS <- function(stock, qtr=FALSE, acc=NA){
  p <- ifelse(qtr, "Qtr", "Ann")
  theurl <- paste("http://investing.money.msn.com/investments/stock-balance-sheet/?symbol=",stock,"&stmtView=",p, sep="",col="")
  tables <- readHTMLTable(theurl)
  # table <- as.data.frame(lapply(tables[[2]], as.character),stringsAsFactors=F)
  table <- tables[[2]]
  colnames(table) <- table[1,]
  colnames(table)[1] <- "Type"
  if (!is.na(acc)){
    table <- subset(table, Type==acc)
  }
  return(table)
}

getIS <- function(stock, qtr=FALSE, acc=NA){
  p <- ifelse(qtr, "Qtr", "Ann")
  theurl <- paste("http://investing.money.msn.com/investments/stock-income-statement/?symbol=",stock,"&stmtView=",p, sep="",col="")
  tables <- readHTMLTable(theurl)
  # table <- as.data.frame(lapply(tables[[2]], as.character),stringsAsFactors=F)
  table <- tables[[2]]
  colnames(table) <- table[1,]
  colnames(table)[1] <- "Type"
  if (!is.na(acc)){
    table <- subset(table, Type==acc)
  }
  return(table)
}

getCF <- function(stock, qtr=FALSE, acc=NA){
  p <- ifelse(qtr, "Qtr", "Ann")
  theurl <- paste("http://investing.money.msn.com/investments/stock-cash-flow/?symbol=",stock,"&stmtView=",p, sep="",col="")
  tables <- readHTMLTable(theurl)
  # table <- as.data.frame(lapply(tables[[2]], as.character),stringsAsFactors=F)
  table <- tables[[2]]
  colnames(table) <- table[1,]
  colnames(table)[1] <- "Type"
  if (!is.na(acc)){
    table <- subset(table, Type==acc)
  }
  return(table)
}

getCCC <- function(stock, qtr=FALSE){
  
  bs <- getBS(stock, qtr)
  is <- getIS(stock, qtr)
  
  #get vectors
  inventory <- subset(bs, Type=="Total Inventory")
  cogs <- subset(is, Type=="Cost of Revenue, Total")
  
  accreceivable <- subset(bs, Type=="Total Receivables, Net")
  sales <- subset(is, Type=="Total Revenue")
  
  accpayable <- subset(bs, Type=="Accounts Payable")
  
  #calcula medias
  
  inventory <- (as.numeric(sub(",", "", inventory[,2]))+as.numeric(sub(",", "", inventory[,3])))/2
  accreceivable <- (as.numeric(sub(",", "", accreceivable[,2]))+as.numeric(sub(",", "", accreceivable[,3])))/2
  accpayable <- (as.numeric(sub(",", "", accpayable[,2]))+as.numeric(sub(",", "", accpayable[,3])))/2
  
  # desvetorizar
  cogs <- as.numeric(sub(",", "", cogs[,2]))
  sales <- as.numeric(sub(",", "", sales[,2]))
  
  
  ICP <- inventory/(cogs/365)
  RCP <- accreceivable/(sales/365)
  PCP <- accpayable/(cogs/365)                   
  
  CCC <- ICP + RCP - PCP
  
  output <- list(ICP=ICP, RCP=RCP, PCP=PCP, CCC=CCC)
  
  return(output)
  
}

getDLPL <- function(stock, qtr=FALSE){
  bs <- getBS(stock, qtr)
  
  #get vectors
  LTD <- as.numeric(sub(",", "",subset(bs, Type == "Total Long Term Debt")[2]))
  STD <- as.numeric(sub(",", "",subset(bs, Type == "Current Port. of LT Debt/Capital Leases")[2]))
  Cash <- as.numeric(sub(",", "",subset(bs, Type == "Cash and Short Term Investments")[2]))
  Equity <- as.numeric(sub(",", "",subset(bs, Type == "Total Equity")[2]))
  
  DLPL <-((LTD+STD)-Cash)/Equity
  return(DLPL)
}

getCAPEX <- function(stock, qtr=FALSE){
  if (is.null(nrow(stock))){
    cf <- getCF(stock)
    CAPEX <- subset(cf, Type=="Capital Expenditures")
    colnames(CAPEX) <- colnames(cf)
  }
  else CAPEX <-tableCAPEX(stock)
  return(CAPEX)
}

getEquity <- function(stock, qtr=FALSE){
  bs <- getBS(stock, qtr)
  
  Equity <- subset(bs, Type=="Total Equity")
  colnames(Equity) <- colnames(bs)
  return(Equity)
}

getDebt <- function(stock, qtr=FALSE){
  
  bs <- getBS(stock, qtr)
  output <- data.frame()
  
  #get vectors
  LTD <- as.data.frame(subset(bs, Type=="Total Long Term Debt"))
  STD1 <- as.data.frame(subset(bs, Type=="Current Port. of LT Debt/Capital Leases"))
  STD2 <- as.data.frame(subset(bs, Type=="Notes Payable/Short Term Debt"))
  output[1,1] <- "Total Debt"
  for(i in seq(2,ncol(LTD))){
    output[1,i] <- as.numeric(gsub(",", "", (LTD[1,i]))) + as.numeric(gsub(",", "", (STD1[1,i])))+ as.numeric(gsub(",", "", (STD2[1,i])))
  }                  
  colnames(output) <- colnames(LTD)                  
  return(output)
}

getCash <- function(stock, qtr=FALSE){
  bs <- getBS(stock, qtr)
  
  #get vectors
  Cash <- subset(bs, Type=="Cash and Short Term Investments")
  colnames(Cash) <- colnames(bs)
  
  return(Cash)
}

getPPE <- function(stock, qtr=FALSE){
  bs <- getBS(stock, qtr)
  
  #get vectors
  PPE <- subset(bs, Type=="Property/Plant/Equipment, Total - Net")
  colnames(PPE) <- colnames(bs)
  
  return(PPE)
}

getDepreciation <- function(stock, qtr=FALSE){
  bs <- getCF(stock, qtr)
  
  #get vectors
  Depreciation <- subset(bs, Type=="Depreciation/Depletion")
  colnames(Depreciation) <- colnames(bs)
  return(Depreciation)
}

getRevenue <- function(stock, qtr=FALSE){
  is <- getIS(stock, qtr)
  
  #get vectors
  Revenue <- subset(is, Type=="Total Revenue")
  
  colnames(Revenue) <- colnames(is)
  
  return(Revenue)
}

tableCAPEX <- function(symbols){
  output <- data.frame()

  for (i in seq(1,nrow(symbols))){
    temp <- getCAPEX(symbols[i,1])
    temp <- as.data.frame(temp)
    temp[,7] <- colnames(temp)[2]
    colnames(temp) <- 1:7
    output <- rbind(output, temp)
    symbols[i,2] <- "OK"
  }
  output[,1] <- symbols[,1]
  colnames(output) <- c("Stock", "Year 0", "Year -1", "Year -2", "Year -3", "Year -4", "Ref. Year")
  return(output)
}

tableRevenue <- function(symbols){
  output <- data.frame()
  
  for (i in seq(1,nrow(symbols))){
    temp <- getRevenue(symbols[i,1])
    temp <- as.data.frame(temp)
    temp[,7] <- colnames(temp)[2]
    colnames(temp) <- 1:7
    output <- rbind(output, temp)
    symbols[i,2] <- "OK"
  }
  colnames(output) <- c("Stock", "Year 0", "Year -1", "Year -2", "Year -3", "Year -4","Ref. Year")
  output[,1] <- symbols[,1]
  
  return(output)
}

tableDepreciation <- function(symbols){
  output <- data.frame()
  
  for (i in seq(1,nrow(symbols))){
    temp <- getDepreciation(symbols[i,1])
    temp <- as.data.frame(temp)
    temp[,7] <- colnames(temp)[2]
    colnames(temp) <- 1:7
    output <- rbind(output, temp)
    symbols[i,2] <- "OK"
  }
  colnames(output) <- c("Stock", "Year 0", "Year -1", "Year -2", "Year -3", "Year -4","Ref. Year")
  output[,1] <- symbols[,1]
  
  return(output)
}

tablePPE <- function(symbols){
  output <- data.frame()
  
  for (i in seq(1,nrow(symbols))){
    temp <- getPPE(symbols[i,1])
    temp <- as.data.frame(temp)
    temp[,7] <- colnames(temp)[2]
    colnames(temp) <- 1:7
    output <- rbind(output, temp)
    symbols[i,2] <- "OK"
  }
  colnames(output) <- c("Stock", "Year 0", "Year -1", "Year -2", "Year -3", "Year -4","Ref. Year")
  output[,1] <- symbols[,1]
  
  return(output)
}

tableDebt <- function(symbols){
  output <- data.frame()
  
  for (i in seq(1,nrow(symbols))){
    temp <- getDebt(symbols[i,1])
    temp <- as.data.frame(temp)
    temp[,7] <- colnames(temp)[2]
    colnames(temp) <- 1:7
    output <- rbind(output, temp)
    symbols[i,2] <- "OK"
  }
  colnames(output) <- c("Stock", "Year 0", "Year -1", "Year -2", "Year -3", "Year -4","Ref. Year")
  output[,1] <- symbols[,1]
  
  return(output)
}

tableCash <- function(symbols){
  output <- data.frame()
  
  for (i in seq(1,nrow(symbols))){
    temp <- getCash(symbols[i,1])
    temp <- as.data.frame(temp)
    temp[,7] <- colnames(temp)[2]
    colnames(temp) <- 1:7
    output <- rbind(output, temp)
    symbols[i,2] <- "OK"
  }
  colnames(output) <- c("Stock", "Year 0", "Year -1", "Year -2", "Year -3", "Year -4","Ref. Year")
  output[,1] <- symbols[,1]
  
  return(output)
}

tableEquity <- function(symbols){
  
  output <- data.frame()
  for (i in seq(1,nrow(symbols))){
    temp <- getEquity(symbols[i,1])
    temp <- as.data.frame(temp)
    temp[,7] <- colnames(temp)[2]
    colnames(temp) <- 1:7
    output <- rbind(output, temp)
    symbols[i,2] <- "OK"
  }
  colnames(output) <- c("Stock", "Year 0", "Year -1", "Year -2", "Year -3", "Year -4","Ref. Year")
  output[,1] <- symbols[,1]
  
  return(output)
}

getBSBR <- function(stock){
  #Abre o site
  theurl <- paste("http://www.bmfbovespa.com.br/pt-br/mercados/acoes/empresas/ExecutaAcaoConsultaInfoEmp.asp?CodCVM=2453&ViewDoc=0")
  tables <- readHTMLTable(theurl)
  #2453 é o código da CEMIG
  
  # BS
  BS <- as.data.frame(tables[[10]])
  
  #Resultados
  
  result <- list(BS=BS)
  
  return(result)
}

getISBR <- function(stock){
  #Abre o site com DRE AMBEV
  theurl <- paste("http://www.rad.cvm.gov.br/enetconsulta/frmDemonstracaoFinanceiraITR.aspx?Informacao=2&Demonstracao=4&Periodo=0&Grupo=DFs+Consolidadas&Quadro=Demonstração+do+Resultado")
  tables <- readHTMLTable(theurl)
  
  # IS
  IS <- as.data.frame(tables[[2]])
  
  #Resultados
  
  result <- list(IS=IS)
  
  return(result)
}