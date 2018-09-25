require(rjson)
require(httr)
require(caTools)
source('saveup-pain.R')
source('saveup-connect.R')

loginUser <- function(username, password) {
  token = auth0GetToken(saveupConnection$product, username, password)
  
  url= paste(saveupConnection$product$url, "users", "info", token$access_token, saveupConnection$product$clientId,  sep="/")
  response= GET(url = url)
  if (response$status_code != 200) { stop("Product server refuses connection") }
  
  content(response, "parsed")
}

loginAgent <- function(username, password) {
  token= auth0GetToken(saveupConnection$product, username, password)
  url= paste(saveupConnection$product$url, "users", "info", token$access_token, saveupConnection$product$clientId,  sep="/")
  response= GET(url)
  if (response$status_code != 200) { stop("Product server refuses connection") }
  user= content(response, "parsed")
  header= add_headers(authorization=paste("Bearer", user$accessToken)) 
  
  agent=list(
    userId= user$userId,
    accessToken= user$accessToken,
    signUpUser= function(email, firstName, familyName, phone="") {
      url= paste(saveupConnection$product$url, "api", "v1", "client", sep="/")
      body= list(
        email= email,
        familyName= familyName,
        firstName= firstName,
        phone="phone"
      )
      response= POST(url, body=body, config=header, encode='json')
      if (response$status_code!=201)
        stop("Client could not be registered", content(response,'text'))
      user = content(response, 'parsed')$user
      user$userId = user$`_id`
      user
    },
    findUser= function(searchText="") {
      url =paste(policyUrl, 'admin', 'person',searchText, sep='/')        
      response= GET(url=url, config=header, encode="json")
      if (response$status_code != 200) stop("Nothing found.")
      person= myjson(response)$person
      policies= myjson(response)$linkedInfo$policies
      policyId= policies[[1]]$displayId
      url= paste(policyUrl, "admin", "policy", policyId, sep="/")
      response= GET(url, config=header, encode='json')
      policy= myjson(response)$policy
      url= paste(policyUrl, "admin", "client", person$displayId, sep='/')
      response= GET(url, config=header, encode='json')
      accessToken= myjson(response)$accessToken
      refreshUser(policy$`_policyHolder`$userId, accessToken)
    },
    findUserElastic= function(searchText="", firstMatch=F) {
      url=paste(saveupConnection$product$url, "api", "v1", "search", paste0("client?q=", searchText),sep="/")
      result= GET(url, config=header, encode='json')
      data= content(result, 'parsed')$result
      if (data$total == 0) stop("Nothing found")
      user = data$hits[[1]]
      if (data$total > 1 && !firstMatch) {
        index= menu(sapply(data$hits, function(x) paste(x$`_source`$firstName, x$`_source`$lastName, x$`_source`$email)))
        user = data$hits[[index]]
      }
      url= paste(saveupConnection$product$url, "api", "v1", "client", user$`_id`, sep="/")
      response= GET(url, config=header, encode='json')
      refreshUser(user$`_id`, content(response, 'parsed')$accessToken)
    }
  )
}

loginPASAdmin <- function(username, password) {
  token= auth0GetToken(saveupConnection$policy, username, password)
  
  myjson = function(response) {
    if (response$status_code==401)
      stop('Not authenticated')
    if (response$status_code==500)
      stop('Internal server error')
    if (response$status_code!=200 && response$status_code!=201)
      stop('Request failed: ', content(response,'text'))
    content(response,'parsed')
  }
  
  policyUrl= saveupConnection$policy$url
  url= paste(policyUrl, "users", "info", token$access_token, saveupConnection$policy$clientId,  sep="/")
  pasUser= myjson(GET(url))
  header= add_headers(authorization=paste("Bearer", pasUser$accessToken)) 
  
  trigger = function(name, arg=NULL) {
    function() {
      url = paste(policyUrl, 'admin', 'trigger', name, arg, sep='/')
      url = paste0(url, '?async')
      res= POST(url=url, config=header, encode="json")
      url2= paste0(policyUrl, res$headers$location)
      if (res$status_code == 202) {
        msgId= content(res,'parsed')$messageId
        count=0
        while (TRUE) {
          res2 = tryCatch(POST(url= url2, config=header), error= function(e) { 
            message("Error calling the server: ", e)
            res 
          })
          if (res2$status_code == 201) {
            res= res2
            break;
          }
          if (res2$status_code %in% c(400,401,500)) {
            msg= tryCatch(content(res2,'parsed')$message, error=function(e) { "Failed "})
            stop("Trigger failed: ", msg, ' (', res2$status_code, ')')
          }
          count= count+1
          if (count>50) stop ("Failed running trigger after 50 retries.")
          message("Waiting for message: ", msgId, " (", res2$status_code, ")")
        }
      }
      myjson(res);
    }    
  }
  
  getAttachments <-function() {
    url = paste(policyUrl, 'admin', 'attachments', sep='/')
    data= myjson(GET(url=url, config=header, encode="json"))$attachments
    Reduce(rbind, lapply(data, function(d) data.frame(d)[,c('X_id','key','createdAt')]))
  }
  
  downloadAttachment <- function(attachmentId=NA, type) {
    if(is.na(attachmentId)) {
      list= getAttachments()
      typeMatch= list[grepl(type, list$key),]
      index= which.max(as.Date(typeMatch$createdAt))
      attachmentId= as.character(typeMatch$X_id[index])
      pathArr = unlist(strsplit(as.character(typeMatch$key[index]),"/"))
      filename=pathArr[length(pathArr)]
      print(filename)
    } else {
      list= getAttachments()
      match= sapply(list, function(x) {attachmentId==x$X_id})
      idMatch = list[match]
      pathArr = unlist(strsplit(as.character(idMatch$key[0]),"/"))
      filename=pathArr[length(pathArr)]
    }
    file= tempfile()
    url = paste(policyUrl,'admin', 'attachment', attachmentId, sep='/')
    response= GET(url, append(header, write_disk(file)))
    list(
      orgfile=filename,
      file=file
    )    
  }
  
  downloadPainFile <- function(attachmentId=NA, type=".*pain.*cd1|DD.*col|BOV.*SCT$") {
    result= downloadAttachment(attachmentId, type)
    data=extractPainFile(result$file)
    file.remove(result$file)
    data$filename=result$orgfile
    data
  }
  downloadCSVAttachment <- function(attachmentId=NA, type=NA) {
    result= downloadAttachment(attachmentId, type)
    data=read.csv(result$file, sep=";")
    file.remove(result$file)
    data$filename=result$orgfile
    data
  }
  
  uploadCollectionFile <- function(fileName) {
    url= paste(policyUrl,'admin','timemodels','collection',sep='/')
    response=POST(url, config=header, body=list(f=upload_file(fileName)), encode="multipart")
    data.frame(myjson(response)$updates)
  }
  
  getBatches <- function(type) {
    url = paste0(paste(policyUrl, 'admin', 'batches', sep='/'), '?type=', type)
    data= myjson(GET(url=url, config=header, encode="json"))$batches
    data.frame(
      X_id = sapply(data, function(x) x$`_id`),
      name = sapply(data, function(x) x$name),
      entryCount = sapply(data, function(x) x$entryCount),
      effectiveDate= sapply(data, function(x) x$effectiveDate)
    )
  }
  
  downloadBatch <- function(batchId = NA, type = NA) {
    #message("The function downloadBatch() is deprecated. Please use downloadPainFile() instead. ")
    if (is.na(batchId)) {
      data = getBatches(type)
      if (is.null(data)) stop("Payment processing opening has not yet been called")
      batchId = data[order(as.character(data$effectiveDate), decreasing=T)[1],'X_id']
    }
    url= paste(policyUrl, 'admin', 'timemodels', batchId, 'collection.json', sep='/')
    response= GET(url=url, config=header, encode="json")
    list= myjson(response)
    Reduce(function(a,b) {a[,b] = sapply(list, function(x) x[[b]]); a}, 
           c("ID","Subaccount","EntryType","EffectiveDate","AccountName","AccountNumber","Amount","ActualAmount", "ActualDate"), 
           data.frame(row.names=1:length(list)))
  }
  
  uploadPaymentBatch <- function(data) {
    #message("The function uploadPaymentBatch() is deprecated. Please use confirmPayments() instead. ")
    url= paste(policyUrl,'admin','timemodels','collection',sep='/')
    body = toJSON(lapply(seq(1, nrow(data)), function(x) data[x,]))
    file = tempfile()
    fileConn<-file(file)
    writeLines(body, fileConn)
    close(fileConn)
    response=POST(url, config=header, body=list(f=upload_file(file)), encode="multipart")
    data.frame(myjson(response)$updates)
  }
  
  uploadSplitRequest <- function(splitRequest) {
    url= paste(policyUrl,'admin','fdSplitRequestUpload',sep='/')
    file = tempfile()
    fileConn<-file(file)
    write.csv2(splitRequest, fileConn, row.names=FALSE)
    response=POST(url, config=header, body=list(f=upload_file(file)), encode="multipart")
    data.frame(myjson(response))
  }
  
  splitResponseForBatchId <- function(exportBatchId) {
    url= paste(policyUrl,'admin','fdSplitRequestResult', exportBatchId, sep='/')
    response=GET(url, config=header)
    splitResponse = content(response, as="text")
    splitResponse
  }
  
  premiumsDue <- function(fromDate, toDate) {
    url= paste0(paste(policyUrl,'admin','payments', 'due', sep='/'), '?startDate=', fromDate, '&endDate=', toDate)
    response=GET(url, config=header)
    stopifnot(response$status_code == 200)
    content(response)
  }
  
  list(
    header=header,
    email = pasUser$email,
    accessToken = pasUser$accessToken,
    envstatus = function() {
      url = paste(policyUrl, 'admin', 'environmentcheck', sep='/')
      cat(url)
      myjson(GET(url=url, config=append(header,timeout(500)), encode="json"))
    },  
    investmentSummary = function() {
      url= paste(policyUrl, 'admin','summary', sep='/')
      data= myjson(GET(url=url, config=header, encode="json"))
      Reduce(rbind, lapply(data$investedAmounts, data.frame))
    },
    investments = function() {
      url= paste(policyUrl, 'admin','investments', sep='/')
      data= myjson(GET(url=url, config=header, encode="json"))$investments
      data.frame(
        X_id = sapply(data, function(x) substring(x$displayId,regexpr('-', x$displayId)+1)),
        type = sapply(data, function(x) x$investmentType),
        unitValue = sapply(data, function(x) c(x$unitValue,NA)[[1]]),
        units = sapply(data, function(x) x$units),
        guaranteeAmount = sapply(data, function(x) c(x$saveInvest$guaranteeAmount, NA)[[1]]),
        guaranteeDate = as.Date(sapply(data, function(x) sub('T.*','',c(x$saveInvest$guaranteeDate, NA)[[1]]))),
        fundId = sapply(data, function(x) c(x$fundInvest$isin, NA)[[1]]),
        valuationDate= as.Date(sapply(data, function(x) sub('T.*','',c(x$valuationDate, NA)[[1]])))
      )
    },
    investmentDetails = function(investmentDisplayId) {
      url= paste(policyUrl, 'admin','investment', investmentDisplayId, sep='/')
      data= myjson(GET(url=url, config=header, encode="json"))
      data
    },
    transactions = function(sa) {
      policyId=sa$`_policy`$`_id`
      url= paste(policyUrl, 'admin','transactions', sep='/')
      url=paste(url,"?after=0&before=253373439600000&policyId=",policyId,sep='')
      data= myjson(GET(url=url, config=header, encode="json"))$transactions
      data
    },
    allPolicies = function() {
      url= paste(policyUrl, 'admin','policies', sep='/')
      data= myjson(GET(url=url, config=header, encode="json"))$policies
      
    },
    allSubaccounts = function() {
      url= paste(policyUrl, 'admin','subaccounts', sep='/')
      data= myjson(GET(url=url, config=header, encode="json"))$subaccounts
      data.frame(
        X_id = sapply(data, function(x) x$displayId),
        goalName = sapply(data, function(x) x$goalName),
        policyName = sapply(data, function(x)  x$Policy),
        marketValue = sapply(data, function(x) ifelse(is.null(x$marketValue), 0,x$marketValue)),
        status = sapply(data, function(x) x$status))
      
    }, 
    allSubaccountDetails = function() {
      url= paste(policyUrl, 'admin','subaccounts', sep='/')
      data= myjson(GET(url=url, config=header, encode="json"))$subaccounts
      data
    }, 
    subaccountDetails = function(displayId) {
      url= paste(policyUrl, 'admin','subaccount', displayId, sep='/')
      data= myjson(GET(url=url, config=header, encode="json"))
      data
    },     
    allPersons = function() {
      url= paste(policyUrl, 'admin','policies', sep='/')
      dataPol= myjson(GET(url=url, config=header, encode="json"))$policies
      url= paste(policyUrl, 'admin','persons', sep='/')
      data= myjson(GET(url=url, config=header, encode="json"))$persons
      
      policyName = c();
      email = c();
      for(i in 1:length(data)) {
        match= sapply(dataPol, function(x) {x$PolicyHolder==data[[i]]$displayId})
        if (!any(match)) next
        policy= dataPol[match][[1]]
        person=data[[i]]
        policyName = c(policyName, policy$displayId)
        email = c(email, person$email)
      }
      
      result = data.frame(
        policyName,email
      )
      
    },
    getPolicy = function(displayId) {
      url= paste(policyUrl, 'admin','policy',displayId, sep='/')
      data= myjson(GET(url=url, config=header, encode="json"))$policy
    },
    getPolicy = function(displayId) {
      url= paste(policyUrl, 'admin','policy',displayId, sep='/')
      data= myjson(GET(url=url, config=header, encode="json"))$policy
    },    
    getPolicyByPersonName = function(familyName) {
      url= paste(policyUrl, 'admin','persons', sep='/')
      data= myjson(GET(url=url, config=header, encode="json"))$persons
      match= sapply(data, function(x) {
        is.na(familyName) || familyName==x$familyName  })
      if (!any(match)) stop("Person not found")
      if (length(match[match==TRUE])>1) stop("More than one Person found")
      personId= data[match][[1]]$displayId
      
      url= paste(policyUrl, 'admin','person',personId, sep='/')
      data= myjson(GET(url=url, config=header, encode="json"))$linkedInfo$policies
      if(length(data)==0)  stop("Policy for Person not found")
      if(length(data)>1)  stop("More than one Policy for Person found")
      
      data= data[[1]]
    },
    policyCancellationOpening = function(name) {
      url= paste(policyUrl, 'admin','policies', sep='/')
      data= myjson(GET(url=url, config=header, encode="json"))$policies
      match= sapply(data, function(x) {name==x$displayId})
      if (!any(match)) stop("Policy  not found")
      if (length(match[match==TRUE])>1) stop("More than one Policy found")
      data= data[match][[1]]
      trigger('policyCancellationOpening', data$displayId)()
    },
    policyCancel = function(name,taxationByInsurer=NA) {
      if(!is.na(taxationByInsurer)) {
        body=list(
          taxationByInsurer= taxationByInsurer
        )
        url = paste(policyUrl, 'admin', 'policy','cancel', name, sep='/')
        myjson(PUT(url=url, body=body,config=append(header,timeout(5000)), encode="json"))
      } else {
        url = paste(policyUrl, 'admin', 'policy','cancel', name, sep='/')
        myjson(PUT(url=url, config=append(header,timeout(5000)), encode="json"))
      }
    },
    getSubaccount= function(goal) {
      url= paste(policyUrl,"admin","subaccounts",sep="/")
      data= myjson(GET(url, config= header, encode='json'))
      match= sapply(data$subaccounts, function(x) x$subaccountId==goal$`_subaccount`$`_id`)
      if (!any(match)) stop("Goal is not purchased.")
      subaccountId= data$subaccounts[match][[1]]$`_id`
      url = paste(policyUrl, 'admin', 'subaccount', subaccountId, sep='/')
      data = myjson(GET(url=url, config=header, encode="json"))
      data.frame(
        X_id = data$subaccount$displayId,
        displayId = data$subaccount$displayId,
        status = data$subaccount$status,
        id= data$subaccount$`_id`
      )
    },  
    getPayments= function(displayId) {
      url= paste(policyUrl,"admin","payments",sep="/")
      url2=paste0(url, "?subaccountId=" ,displayId)
      data= myjson(GET(url2, config= header, encode='json'))
      payments=data$payments
    },    
    getSubaccountDetails= function(sa) {
      url = paste(policyUrl, 'admin', 'subaccount', sa$displayId, sep='/')
      data = myjson(GET(url=url, config=header, encode="json"))
    },
    getSubaccountTransactionHistory= function(displayId,after=NA,before=NA) {
      url = paste(policyUrl, 'api','v1', 'payments','transactionhistory',  sep='/')
      url2=paste0(url, "?subaccountId=" ,displayId)
      if(!is.na(after)) {
        url2=paste0(url2,"&after=",formatTime(after))
      }
      if(!is.na(before)) {
        url2=paste0(url2,"&before=", formatTime(before))
      }
      
      header2= add_headers(authorization= 'Basic dGVzdDp0ZXN0')
      response = myjson(GET(url=url2, config=header2, encode="json"))
      if (response$status != "SUCCESS") {
        stop("Could not get transaction history")
      }
      transactions = response$data$transactions
    },
    getGoalTransactionHistory = function(user,goal) {
      url= paste(policyUrl,"admin","subaccounts",sep="/")
      data= myjson(GET(url, config= header, encode='json'))
      match= sapply(data$subaccounts, function(x) x$subaccountId==goal$`_subaccount`$`_id`)
      if (!any(match)) stop("Goal is not purchased.")
      displayId= data$subaccounts[match][[1]]$displayId
      url=paste(saveupConnection$product$url,'api','v2','payments','transactionhistory',displayId, sep='/')
      response = content(GET(url,config=add_headers(authorization=paste('Bearer', user$accessToken))),'parsed')
      if (response$status != "SUCCESS") {
        stop("Could not get transaction history")
      }
      transactions = response$data$transactions;
    },
    
    trigger = list(
      paymentProcessingOpening = trigger('paymentProcessingOpening'),
      investmentOpening = trigger('investmentOpening'),
      testprocessDailyRevaluation = trigger('testprocessDailyRevaluation'),
      partSurrenderOpening= trigger('partSurrenderOpening'),
      policyCancellationClosing= trigger('policyCancellationClosing')
    ),
    getAttachments= getAttachments,
    getPremiumBatches = function() getBatches('Premium'),
    getPaymentBatches = function() getBatches('Payment'),
    getInvestmentBatches = function() getBatches('Investment'),
    downloadPremiumBatch = function(id=NA) downloadBatch(id, 'Premium'),
    downloadPaymentBatch = function(id=NA) downloadBatch(id, 'Payment'),
    downloadTransactionFile = function(id=NA) downloadCSVAttachment(id, '.*Transaction.*csv'),
    downloadInforceFile = function(id=NA) downloadCSVAttachment(id, '.*Inforce.*csv'),
    downloadPainFile = downloadPainFile,
    uploadCollectionFile = uploadCollectionFile,
    uploadPaymentBatch = uploadPaymentBatch,
    uploadSplitRequest=uploadSplitRequest,
    splitResponseForBatchId=splitResponseForBatchId,
    premiumsDue = premiumsDue,
    collectAllPremiums = function() {
      trigger('paymentProcessingOpening')()
      data = downloadBatch(NA, 'Premium')
      if (!is.null(data)) {
        data$ActualAmount = data$Amount
        data$ActualDate = format(Sys.time(),"%Y-%m-%d")
        uploadPaymentBatch(data)
      }
    },
    fullSurrender= function(goal, cancellation=NA,taxationByInsurer=NA) {
      url= paste(policyUrl,"admin","subaccounts",sep="/")
      data= myjson(GET(url, config= header, encode='json'))
      match= sapply(data$subaccounts, function(x) x$subaccountId==goal$`_subaccount`$`_id`)
      if (!any(match)) stop("Goal is not purchased.")
      subaccountId= data$subaccounts[match][[1]]$`_id`
      if(!is.na(taxationByInsurer)) {
        url = paste(policyUrl, 'admin', 'trigger', 'fullSurrenderOpening',subaccountId,  sep='/')
        body= list(
          subaccountId= subaccountId,
          taxationByInsurer= taxationByInsurer
        )
        response= POST(url, body=body, config=header, encode='json')
      } else {
        trigger('fullSurrenderOpening', subaccountId)()
      }
    },
    partSurrenderOpening= function(goal) {
      url= paste(policyUrl,"admin","subaccounts",sep="/")
      data= myjson(GET(url, config= header, encode='json'))
      match= sapply(data$subaccounts, function(x) x$subaccountId==goal$`_subaccount`$`_id`)
      if (!any(match)) stop("Goal is not purchased.")
      subaccountId= data$subaccounts[match][[1]]$`_id`      
      url = paste(policyUrl, 'admin', 'trigger', 'partSurrenderOpening',subaccountId, sep='/')
      myjson(POST(url=url, config=header, encode="json"))
    },  
    deathClaimOpening= function(goal) {
      url= paste(policyUrl,"admin","subaccounts",sep="/")
      data= myjson(GET(url, config= header, encode='json'))
      match= sapply(data$subaccounts, function(x) x$subaccountId==goal$`_subaccount`$`_id`)
      if (!any(match)) stop("Goal is not purchased.")
      policyId= data$subaccounts[match][[1]]$Policy
      url = paste(policyUrl, 'admin', 'trigger', 'deathClaimOpening',policyId, sep='/')
      myjson(POST(url=url, config=header, encode="json"))
    },     
    findUser = function(searchText, firstMatch=F) {
      url = paste0(paste(policyUrl, 'admin', 'search', 'policy', sep='/'),'?q=',searchText)        
      response= GET(url=url, config=header, encode="json")
      data= myjson(response)$result
      if (data$total == 0) stop("Nothing found")
      user = data$hits[[1]]
      if (data$total > 1 && !firstMatch) {
        index= menu(sapply(data$hits, function(x) paste(x$`_source`$firstName, x$`_source`$lastName, x$`_source`$email)))
        user = data$hits[[index]]
      }
      policyId= user$`_source`$policies[[1]]$displayId
      url= paste(policyUrl, "admin", "policy", policyId, sep="/")
      response= GET(url, config=header, encode='json')
      policy= myjson(response)$policy
      if (is.null(policy$PolicyHolder)) {
        #legacy access
        url= paste(policyUrl, "admin", "client", policy$`_policyHolder`$`_id`, sep='/')
      } else {
        url= paste(policyUrl, "admin", "client", policy$PolicyHolder, sep='/')
      }
      response= GET(url, config=header, encode='json')
      accessToken= myjson(response)$accessToken
      refreshUser(policy$`_policyHolder`$userId, accessToken)
    },
    getBenefits= function(goal) {
      url= paste(policyUrl,"admin","subaccounts",sep="/")
      data= myjson(GET(url, config= header, encode='json'))
      match= sapply(data$subaccounts, function(x) x$subaccountId==goal$`_subaccount`$`_id`)
      if (!any(match)) stop("Goal is not purchased.")
      displayId= data$subaccounts[match][[1]]$displayId
      url=paste(saveupConnection$policy$url,'admin','benefits', sep='/')
      url2=paste0(url, "?subaccountId=",displayId)
      response= GET(url2, config=header, encode='json')
      data= myjson(response)$benefits
    },
    getBenefitsDisplayId= function(displayId) {
      url=paste(saveupConnection$policy$url,'admin','benefits', sep='/')
      url2=paste0(url, "?subaccountId=",displayId)
      response= GET(url2, config=header, encode='json')
      data= myjson(response)$benefits
    },
    getTimeModelEntries= function(after, before=Sys.time(), asList=FALSE) {
      url=paste(saveupConnection$policy$url,'admin','timemodels', sep='/')
      url2=paste0(url, "?after=",formatTime(after),"&before=", formatTime(before),"&limit=10000")
      data= content(GET(url2, config =header), 'parsed')$timemodels
      if (asList) data else
        data.frame(
          id= sapply(data, function(x) x$displayId),
          subaccount= sapply(data, function(x) x$Subaccount),
          effectiveDate= as.Date(sapply(data, function(x) x$effectiveDate)),
          type= as.character(sapply(data, function(x) x$entryType)),
          amount= sapply(data, function(x) x$amount)
        )
    },
    getTransactions= function(after, before=Sys.time(), asList=FALSE) {
      url=paste(saveupConnection$policy$url,'admin','transactions', sep='/')
      url2=paste0(url, "?after=",formatTime(after),"&before=", formatTime(before),"&limit=10000")
      data= content(GET(url2, config =header), 'parsed')$transactions
      getReport = function(n) lapply(data, function(x) if (n<=length(x$changeReport)) { x$changeReport[[n]] } else { list(attributeOldValue=NA, attributeNewValue=NA, changeMarketValue=NA) })
      if (asList) data else
        
        data.frame(
          subaccount= sapply(data, function(x) x$Subaccount),
          effectiveDate= as.Date(sapply(data, function(x) x$effectiveDate)),
          type= as.character(sapply(data, function(x) x$transactionType)),
          change1= as.character(sapply(getReport(1), function(x) x$investmentType)),
          attribute1= as.character(sapply(getReport(1), function(x) x$attributeName)),
          oldVal1= sapply(getReport(1), function(x) x$attributeOldValue),
          newVal1= sapply(getReport(1), function(x) x$attributeNewValue),
          marketChange1= sapply(getReport(1), function(x) x$changeMarketValue),
          change2= as.character(sapply(getReport(2), function(x) x$investmentType)),
          attribute2= as.character(sapply(getReport(2), function(x) x$attributeName)),
          oldVal2= sapply(getReport(2), function(x) x$attributeOldValue),
          newVal2= sapply(getReport(2), function(x) x$attributeNewValue),
          marketChange2= sapply(getReport(2), function(x) x$changeMarketValue),
          change3= as.character(sapply(getReport(3), function(x) x$investmentType)),
          attribute3= as.character(sapply(getReport(3), function(x) x$attributeName)),
          oldVal3= sapply(getReport(3), function(x) x$attributeOldValue),
          newVal3= sapply(getReport(3), function(x) x$attributeNewValue),
          marketChange3= sapply(getReport(3), function(x) x$changeMarketValue),
          change4= as.character(sapply(getReport(4), function(x) x$investmentType)),
          attribute4= as.character(sapply(getReport(4), function(x) x$attributeName)),
          oldVal4= sapply(getReport(4), function(x) x$attributeOldValue),
          newVal4= sapply(getReport(4), function(x) x$attributeNewValue),
          marketChange4= sapply(getReport(4), function(x) x$changeMarketValue)
        )
      
    },
    reindex= function() {
      url= paste(saveupConnection$product$url, "synchronize",  sep="/")
      response= GET(url)
      response$status_code == 200
    }
  )
}

registerUser <- function(email, password) {
  url= paste(saveupConnection$product$auth0Url,"dbconnections","signup",sep="/")
  
  body = list(
    client_id= saveupConnection$product$clientId,
    client_secret= saveupConnection$product$clientSecret,
    email= email,
    password= password,
    connection= "Username-Password-Authentication",
    email_verified = TRUE,
    user_metadata= list(role="CLIENT")
  )
  response= POST(url=url, body=body, encode="json")
  if (response$status_code != 200) {
    message(content(response,"text"))
    stop("Registration failed")
  }
  sprintf("Please verify the user's email: %s", email)
}

refreshUser <- function(userId, accessToken) {
  header= add_headers(authorization=paste("Bearer", accessToken)) 
  url = paste(saveupConnection$product$url,"api","v1","data",sep="/")
  response= GET(url, config=header, encode='json')
  user= content(response, 'parsed')
  user$accessToken= accessToken
  user
} 

setUserData <- function(user, firstName, familyName, birthday, gender, phone, birthplace="Born in nowhere", language="de") {
  header= add_headers(authorization=paste("Bearer", user$accessToken)) 
  url = paste(saveupConnection$product$url,"api","v1","user",user$userId,sep="/")
  body= list(
    firstName = firstName,
    familyName = familyName,
    birthday = birthday,
    birthdayPlace = birthplace,
    gender = gender,
    phone = phone,
    contractLanguage= language
  )
  response = PUT(url=url, config=header, body=body, encode="json")
  if (response$status_code != 200) { 
    message(content(response,"text"))
    stop("Uploading data failed")
  }
}

setUserDataFid <- function(user, firstName, familyName, birthday, gender, phone, fiscalNumber, language="pt", birthplace="Braga") {
  header= add_headers(authorization=paste("Bearer", user$accessToken)) 
  url = paste(saveupConnection$product$url,"api","v1","user",user$userId,sep="/")
  body= list(
    firstName = firstName,
    familyName = familyName,
    birthday = birthday,
    fiscalNumber = fiscalNumber,
    gender = gender,
    phone = phone,
    contractLanguage= language,
    birthdayPlace = birthplace
  )
  response = PUT(url=url, config=header, body=body, encode="json")
  if (response$status_code != 200) { 
    message(content(response,"text"))
    stop("Uploading data failed")
  }
}

setUserDataMSV <- function(user, 
                           firstName, 
                           familyName, 
                           birthday, 
                           gender,
                           phone, 
                           phoneCountryCode="0034",
                           birthplace="Born in nowhere",              
                           countryOfBirth="Country Of Birth",
                           idCardNumber="IdCardNumber",
                           nationality="The Nationality",
                           dualNationality="Second Nationality",                           
                           language="en",  
                           allowMarketingCommunication=TRUE,
                           allowProductCommunication=TRUE,
                           politicallyExposedPerson=FALSE,
                           politicallyExposedPersonDetails="",
                           taxResidences=list()
) {
  header= add_headers(authorization=paste("Bearer", user$accessToken)) 
  url = paste(saveupConnection$product$url,"api","v1","user",user$userId,sep="/")
  body= list(
    firstName = firstName,
    familyName = familyName,
    birthday = birthday,
    birthdayPlace = birthplace,
    allowMarketingCommunication = allowMarketingCommunication,
    allowProductCommunication=allowProductCommunication,
    politicallyExposedPerson = politicallyExposedPerson,
    politicallyExposedPersonDetails = politicallyExposedPersonDetails,
    gender = gender,
    phone = phone,
    phoneCountryCode = phoneCountryCode,
    countryOfBirth = countryOfBirth,
    idCard = list(
      idNumber=idCardNumber,
      documentId="DOCUMENTID",
      dateOfIssue="2017-02-31",
      placeOfIssue="Issue Place of Nowhere",
      validUntil="2222-02-22",
      idType="PASSPORT",
      nationality="Austria"
    ),
    nationality = nationality,
    dualNationality=dualNationality,
    contractLanguage= language,
    taxResidents=TRUE,
    taxResidences=taxResidences
  )
  response = PUT(url=url, config=header, body=body, encode="json")
  if (response$status_code != 200) { 
    message(content(response,"text"))
    stop("Uploading data failed")
  }
}

setAddress <- function(user, addressLine1, addressLine2, city, zipCode, country) {
  header= add_headers(authorization=paste("Bearer", user$accessToken)) 
  url = paste(saveupConnection$product$url,"api","v1","user",user$userId,sep="/")
  body= list(address=list(
    addressLine1 = addressLine1,
    addressLine2 = addressLine2,
    city = city,
    zipCode = zipCode,
    country = country
  ))
  response = PUT(url=url, config=header, body=body, encode="json")
  if (!response$status_code %in% c(200,201)) { 
    message(content(response,"text"))
    stop("Uploading data failed") 
  }
}

setAddressMSV <- function(user, addressLine1, addressLine2, city, zipCode, country,
                          state="A State",
                          houseNumber="156") {
  header= add_headers(authorization=paste("Bearer", user$accessToken)) 
  url = paste(saveupConnection$product$url,"api","v1","user",user$userId,sep="/")
  body= list(address=list(
    addressLine1 = addressLine1,
    addressLine2 = addressLine2,
    city = city,
    zipCode = zipCode,
    country = country,
    state=state,
    houseNumber=houseNumber
  ))
  response = PUT(url=url, config=header, body=body, encode="json")
  if (!response$status_code %in% c(200,201)) { 
    message(content(response,"text"))
    stop("Uploading data failed") 
  }
}

addGoal <- function(user, goalName, targetValue, guaranteeLevel, maturity, isin="DTFS") {
  header= add_headers(authorization=paste("Bearer", user$accessToken)) 
  url = paste(saveupConnection$product$url,"api","v1","goal",sep="/")
  
  riskAssessmentAnswers=c();
  riskAssessmentAnswers[[1]]= list(
    questionText="qt1",
    answerText="at1",
    answerValue=1,
    selectedOnDate="2222-02-22"
  )
  riskAssessmentAnswers[[2]]= list(
    questionText="qt2",
    answerText="at2",
    answerValue=1,
    selectedOnDate="2222-02-22"
  )
  body= list(
    goalName = goalName,
    targetValue = targetValue,
    `_subaccount` = list(
      fund = list(
        isin= isin,
        guaranteeLevel = guaranteeLevel,
        guaranteeMaturity = formatTime(maturity)
      )
    ),
    initialRegularPayment= 0,
    initialPremium= 0,
    riskAssessmentResult=list(
      riskAssessmentAnswers =riskAssessmentAnswers,
      selectedGuaranteeLevel=guaranteeLevel
    ),
    image="30-01.png"
  )
  response = POST(url=url, config=header, body=body, encode="json")
  if (response$status_code != 201) { 
    message(content(response,"text"))
    stop("Uploading data failed") 
  }
  data= content(response,"parsed")
  list(`_id`= data$goalId, `_subaccount`=list(`_id`= data$subAccountId))
}

getGoal= function(user,saDisplayId) {
  header= add_headers(authorization=paste("Bearer", user$accessToken)) 
  url = paste(saveupConnection$product$url,"api","v1","data",sep="/")
  response= GET(url, config=header, encode='json')
  if (response$status_code != 200) { 
    message(content(response,"text"))
    stop("Not able to get data for current user") 
  }
  
  data=content(response, 'parsed')
  goals=data$goals;
  match= sapply(goals, function(x) {x$`_subaccount`$displayId==saDisplayId})
  if (!any(match)) stop("Goal not found")	
  foundGoal= data$goals[match][[1]]
}

updateGoal <- function(goal, goalName=NA, targetValue=NA, guaranteeLevel=NA, guaranteeMaturity=NA) {
  header= add_headers(authorization=paste("Bearer", user$accessToken)) 
  url = paste(saveupConnection$product$url,"api","v1","data",sep="/")
  response= GET(url, config=header, encode='json')
  if (response$status_code != 200) { 
    message(content(response,"text"))
    stop("Not able to get data for current user") 
  }
  
  data=content(response, 'parsed')
  match= sapply(data$goals, function(x) x$`_id`==goal$`_id`)
  if (!any(match)) stop("Goal not found in database")
  foundGoal= data$goals[match][[1]]
  
  body= list(
    goalName = foundGoal$goalName,
    targetValue = foundGoal$targetValue,
    `_subaccount` = list(
      fund = list(
        guaranteeLevel = foundGoal$`_subaccount`$fund$guaranteeLevel,
        guaranteeMaturity = foundGoal$`_subaccount`$fund$guaranteeMaturity
      )
    )
  )
  
  if (!is.na(goalName)) body$goalName=goalName;
  if (!is.na(targetValue)) body$targetValue=targetValue;
  if (!is.na(guaranteeLevel)) body$`_subaccount`$fund$guaranteeLevel=guaranteeLevel;
  if (!is.na(guaranteeMaturity)) body$`_subaccount`$fund$guaranteeMaturity=formatTime(guaranteeMaturity)
  
  url = paste(saveupConnection$product$url,"api","v1","goal",foundGoal$`_id`,sep="/")
  response = PUT(url=url, config=header, body=body, encode="json")
  if (response$status_code != 200) { 
    message(content(response,"text"))
    stop("Updating goal failed") 
  }
}

goalSurrender <- function(user,goal,taxationByInsurer=NA) {
  header= add_headers(authorization=paste("Bearer", user$accessToken)) 
  url = paste(saveupConnection$product$url,"api","v1","goal","surrender",goal$`_id`,sep="/")
  
  if(!is.na(taxationByInsurer)) {
    body= list(
      taxationByInsurer= taxationByInsurer
    )
    response = POST(url=url,body=body, config=header, encode="json")
  } else {
    response = POST(url=url, config=header, encode="json")
  }
  
  if (!response$status_code %in% c(200,201)) { 
    message(content(response,"text"))
    stop("Goal Surrender failed") 
  }
}

addPayment <- function(user, goal, accountName, accountNumber, amount, frequency, startDate=NA, endDate=NA) {
  cat("This function is deprecated. Please use addPayments")
  if (frequency==0) {
    addPayments(user, goal, accountName, accountNumber, amount, NA, startDate=startDate, endDate=endDate)
  } else {
    addPayments(user, goal, accountName, accountNumber, 0, amount, startDate=startDate, endDate=endDate)
  }
}

addPayments <- function(user, goal, accountName, accountNumber, singlePremium, 
                        regularPremium=NA, startDate=NA, endDate=NA, arbitraryStart=FALSE,taxationByInsurer=NA) {
  header= add_headers(authorization=paste("Bearer", user$accessToken)) 
  isoNow= formatTime()
  
  #update userInfo
  url = paste(saveupConnection$product$url,"api","v1","user",user$userId,sep="/")
  body= list(
    accountName= accountName,
    accountNumber= accountNumber
  )
  response = PUT(url=url, config=header, body=body, encode="json")
  if (response$status_code != 200) { 
    message(content(response,"text"))
    stop("Could not set account number for user")
  }
  
  # get policy
  url = paste(saveupConnection$product$url,"api","v1","policy",sep="/")
  body= list(
    effectiveDate= isoNow,
    requestDate= isoNow,
    userId= user$userId
  )
  response = POST(url=url, config=header, body=body, encode="json")
  if (!response$status_code %in% c(200,201)) { 
    message(content(response,"text"))
    stop("Could not get a policy for this user") 
  }
  policyId= fromJSON(content(response,"text"))$policyId
  
  # create payments
  if (is.na(startDate))
    startDate=Sys.time()
  paymentAgreement = function(amount, frequency) { list(
    accountName= accountName,
    accountNumber= accountNumber,
    amount= amount,
    currency= "EUR",
    frequency= frequency,
    paymentType= "SEPA",
    startDate= formatTime(startDate),
    effectiveDate= isoNow
  )}
  body= list(
    goalId= goal$`_id`,
    policyId= policyId,
    userId= user$userId,
    paymentAgreements= list(paymentAgreement(singlePremium,0), paymentAgreement(regularPremium,12))[c(singlePremium!=0.0, !is.na(regularPremium))]
  )
  if (!is.na(endDate))
    body$endDate= formatTime(endDate)
  
  if (isTRUE(arbitraryStart)) {
    body$arbitraryStartDate="1"
  }
  if (!is.na(taxationByInsurer)) {
    body$taxationByInsurer=taxationByInsurer
  }  
  url = paste(saveupConnection$product$url,"api","v1","purchases",sep="/")
  response = POST(url=url, config=header, body=body, encode="json",timeout(300))
  if (!response$status_code %in% c(200,201)) { 
    message(content(response,"text"))
    stop("Payment failed") 
  }
  Sys.sleep(10)
  TRUE
}
addPaymentsV2 <- function(user, goal, accountName, accountNumber, singlePremium, 
                          regularPremium=NA, startDate=NA, endDate=NA, arbitraryStart=FALSE,taxationByInsurer=NA) {
  header= add_headers(authorization=paste("Bearer", user$accessToken)) 
  isoNow= formatTime()
  
  #update userInfo
  url = paste(saveupConnection$product$url,"api","v1","user",user$userId,sep="/")
  body= list(
    accountName= accountName,
    accountNumber= accountNumber
  )
  response = PUT(url=url, config=header, body=body, encode="json")
  if (response$status_code != 200) { 
    message(content(response,"text"))
    stop("Could not set account number for user")
  }
  
  # get policy
  url = paste(saveupConnection$product$url,"api","v1","policy",sep="/")
  body= list(
    effectiveDate= isoNow,
    requestDate= isoNow,
    userId= user$userId
  )
  
  response = POST(url=url, config=header, body=body, encode="json")
  if (!response$status_code %in% c(200,201)) { 
    message(content(response,"text"))
    stop("Could not get a policy for this user") 
  }
  policyId= fromJSON(content(response,"text"))$policyId
  
  # create payments
  if (is.na(startDate))
    startDate=Sys.time()
  paymentAgreement = function(amount, frequency) { list(
    accountName= accountName,
    accountNumber= accountNumber,
    amount= amount,
    currency= "EUR",
    frequency= frequency,
    paymentType= "SEPA",
    startDate= formatTime(startDate),
    effectiveDate= isoNow
  )}
  body= list(
    purchaseType="deposit",
    goalId= goal$`_id`,
    policyId= policyId,
    userId= user$userId,
    paymentAgreements= list(paymentAgreement(singlePremium,0), paymentAgreement(regularPremium,12))[c(singlePremium!=0.0, !is.na(regularPremium))]
  )
  if (!is.na(endDate))
    body$endDate= formatTime(endDate)
  
  if (isTRUE(arbitraryStart)) {
    body$arbitraryStartDate="1"
  }
  if (!is.na(taxationByInsurer)) {
    body$taxationByInsurer=taxationByInsurer
  }  
  baseurl = saveupConnection$product$url;
  url = paste(baseurl,"api","v2","payments?async",sep="/")
  
  response = POST(url=url, config=header, body=body, encode="json")
  response=handleLishResponse(response,baseurl,body,header);
  
  if (response$status!="SUCCESS") { 
    message(Sys.time()," ", response$status," ", response$messageId)
    stop("Payment failed") 
  }
  TRUE
}
myjson = function(response) {
  if (response$status_code==401)
    stop('Not authenticated')
  if (response$status_code==500)
    stop('Internal server error')
  if (response$status_code!=200 && response$status_code!=201)
    stop('Request failed: ', content(response,'text'))
  content(response,'parsed')
}
handleLishResponse <- function(res, baseUrl,body,header) {
  print("Handle Lish Response")
  url2= paste0(baseUrl, res$headers$location)
  if (res$status_code == 202) {
    msgId= content(res,'parsed')$messageId
    count=0
    while (TRUE) {
      res2 = tryCatch(POST(url= url2,body=body, config=header, encode="json"), error= function(e) { 
        message("Error calling the server: ", e)
        res 
      })
      if (res2$status_code == 201) {
        res= res2
        break;
      }
      if (res2$status_code %in% c(400,401,500)) {
        msg= tryCatch(content(res2,'parsed')$message, error=function(e) { "Failed "})
        stop("Trigger failed: ", msg, ' (', res2$status_code, ')')
      }
      count= count+1
      if (count>50) stop ("Failed running trigger after 50 retries.")
      message("Waiting for message: ", msgId, " (", res2$status_code, ")")
    }
  }
  myjson(res);  
}
getStatus <- function(user, goal) {
  url=paste(saveupConnection$product$url,'api','v1','goal','status',goal$`_id`, sep='/')
  content(GET(url,config=add_headers(authorization=paste('Bearer', user$accessToken))),'parsed')  
}
getGoalOverview <- function(user, goal) {
  url=paste(saveupConnection$product$url,'api','v2','goal','status',goal$`_id`, sep='/')
  data = content(GET(url,config=add_headers(authorization=paste('Bearer', user$accessToken))),'parsed')
  if (data$status != "SUCCESS") { 
    stop("Could not get goal status")
  }
  details = data$details;
}
setFundValue <- function(value, fundId='Equity.VIG AV', date=Sys.time()) {
  response= POST(
    url='https://compute-dev.save-up.net/api/v1/saveup-compute/fund/value',
    config= add_headers(authorization= 'Basic bXI6VzhlUEhhY3VnNGJydTZyNnBhCg'),
    body=list(
      fundId= fundId,
      valueDate= formatTime(date),
      value= value
    ), encode="json")
  response$status_code %in% c(200,201)
}
setFundValueDate <- function(value, date, fundId='Equity.VIG AV') {
  message("The setFundValueDate() function is deprecated. Please us setFundValue() with the optional date parameter\n")
  setFundValue(value, fundId, date)
}
getFundValue <- function(fundId=NULL, startDate=Sys.time()-3600*24*7, endDate=Sys.time()) {
  urlPart= if (is.null(fundId))
    paste0(saveupConnection$product$url,'/compute/v1/fund/value?fundId=X')
  else
    paste0('https://compute-dev.save-up.net/api/v1/saveup-compute/fund/value?fundId=',fundId)
  response= GET(
    url=URLencode(paste0(urlPart, '&valueDateFrom=', formatTime(startDate), '&valueDateUntil=', formatTime(endDate))),
    config= add_headers(authorization= 'Basic bXI6VzhlUEhhY3VnNGJydTZyNnBhCg'))
  data= content(response,'parsed')
  if (is.null(fundId)) data=data$fund
  data.frame(
    date= as.POSIXct(sapply(data, function(d) d$valueDate/1000), origin="1970-01-01"),
    value= sapply(data, function(d) d$value))
}

formatTime <<- function(time= Sys.time()) {
  if (typeof(time)=="character") {
    if (grepl("^....-..?-..?$", time)) {
      time= as.Date(time)
    }
    else if (grepl("^....-..?-..?T..?:..?:..?", time)) {
      time = strptime(time, "%FT%T")
    }
    else
      stop("Date format not recognized: ", time)
  }
  format(time, "%Y-%m-%dT%H:%M:%SZ", tz = 'UTC')
}

fidelidade <- function() {
  nif <- function() {
    response= GET('fid-mock.fid-develop.save-up.net/nif')
    body = content(response)
    body$NIF
  }
  iban <- function() {
    response= GET('fid-mock.fid-develop.save-up.net/iban')
    body = content(response)
    body$iban
  }
  computeUnits <- function(newPremium, 
                           guaranteeLevel,
                           maturityDate, 
                           forDate=Sys.time(), 
                           numberOfHeldFundUnits=0, 
                           guaranteeAmount=0,
                           cost=0) {
    response= POST(
      url='https://compute-dev.save-up.net/api/v1/saveup-compute/computeunits',
      config= add_headers(authorization= 'Basic bXI6VzhlUEhhY3VnNGJydTZyNnBhCg'),
      body=list(
        sumOfPaidPremiums = 0,
        guaranteeLevel = guaranteeLevel,
        maturityDate = maturityDate,
        numberOfHeldFundUnits = numberOfHeldFundUnits,
        fundId = "Fidelidade Index",
        valueOfGuarantee = 0,
        guaranteeAmount = guaranteeAmount,
        newPremium = newPremium,
        cost = cost,
        priceEvaluationDate = formatTime(forDate)
      ), encode="json")
    content(response)
  }
  
  splitResponse <- function(subaccountId, transactionId, split) {
    list(
      SubAccountID = subaccountId,
      UnitPriceRiskyFund = round(split$currentFundPrice, digits = 7) ,
      BuyUnitPriceGteeFund = round(split$guaranteeBuyPrice, digits = 7),
      RiskyFundAmount = round(split$unitsToBuy * split$currentFundPrice, digits = 2),
      GteeFundAmount = round(split$unitsToBuy * split$guaranteeBuyPrice, digits = 2),
      EffectiveDate = format(Sys.time(), "%Y%m%d"),
      TransactionID = transactionId
    )
  }
  
  list(
    fiscalNumber = nif,
    iban = iban,
    computeUnits=computeUnits,
    splitResponse=splitResponse
  )
}

fidProxy <- function() {
  auth = base64encode(paste(
    saveupConnection$proxy$fidUser, saveupConnection$proxy$fidPass, sep=':'))
  header= add_headers(
    authorization=paste(
      "Basic", auth, sep=" "))
  list(
    collectPremium = function(policyId, 
                              subaccountId, 
                              amount, 
                              paymentAgreementId=NA,
                              state="SUCCESS", 
                              collectionDate=Sys.time()) {
      transactionId = paste(sample(letters, 24), collapse='');
      response = POST(
        url=paste(saveupConnection$policy$url,"pi","v1","premium", sep="/"),
        config=header,
        body=list(
          transactionId=transactionId,
          policyId=policyId,
          subaccountId=subaccountId,
          paymentAgreementId=paymentAgreementId,
          state=state,
          amount=amount,
          collectionDate=formatTime(collectionDate)
        ),
        encode='json')
      response$status_code
    },
    
    duePayments = function(fromDate, untilDate) {
      url=paste0(
        paste(saveupConnection$policy$url,"api","v1","payments", "due?", sep="/"),
        "startDate=", fromDate, "&endDate=", untilDate)
      response = GET(
        url=url,
        config=header,
        encode='json')
      
      content(response)
    },
    
    openPremium = function(fromDate, untilDate) {
      response = POST(
        url=paste(saveupConnection$policy$url,"api","v1","payments", "due", sep="/"),
        config=header,
        body=list(
          startDate=fromDate,
          endDate=untilDate
        ),
        encode='json')
      content(response)
    },
    
    policyDetails = function(policyDisplayId) {
      url=paste(saveupConnection$policy$url,"api","v1","policy", policyDisplayId, sep="/")
      response = GET(
        url=url,
        config=header,
        encode='json')
      response      
    }
  )
}

cancelPolicy <- function(user, taxationByInsurer = FALSE) {
  header= add_headers(authorization=paste("Bearer", user$accessToken)) 
  url = paste(saveupConnection$product$url,"api","v1","user", user$userId,sep="/")
  response = DELETE(url=url, config=header, body=list(taxationByInsurer = taxationByInsurer), encode="json")
  response
}

policyDocuments = function(userId) {
  header= add_headers(authorization=paste("Basic", base64encode("test:test"))) 
  url=paste0(
    paste(saveupConnection$policy$url,"api","v1","document", "search", sep="/"), 
    "?characteristic=legal&userId=", userId)
  response = GET(
    url=url,
    config=header,
    encode='json')
  response
}