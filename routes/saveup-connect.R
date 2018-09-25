require(httr)

saveupConnect <- function(name=NA, onlyIfUndefined=FALSE) {
  auth= list(
    thetaris = list(
      auth0Url= "https://thetaris.auth0.com",
      clientSecret= "z2XIDIBgCFmQfb0sbfophzn4fZlmY72xaXD22x8MONIHMoIYh44kWFMypheM6Gay",
      clientId= "nNKKFRz7kRnIVlMot6nKtIFEzUVn4tOI"
    ),
    msvdev = list(
      auth0Url= "https://thetaris.auth0.com",
      clientSecret= "z2XIDIBgCFmQfb0sbfophzn4fZlmY72xaXD22x8MONIHMoIYh44kWFMypheM6Gay",
      clientId= "nNKKFRz7kRnIVlMot6nKtIFEzUVn4tOI"
    ),
    msvdemo = list(
      auth0Url= "https://demo-msv.eu.auth0.com",
      clientSecret= "fU6B7nY7FoXEzCumguFJM1GdYu9D3uY_iHdAxVXjMHlAMpF_kGCDllUeWxejrP7r",
      clientId= "NWLoxMRaQU4odh49Q9nPPLuDnHiQHraQ"
    ),
    fiddev = list(
      auth0Url= "https://demo-fid.eu.auth0.com",
      clientSecret="qokkLwMqi0QiJ8_9gStd0qMXU_BmQrLClu8R8XweAnnHFNXNWT7a3JALCjDUxxCy",
      clientId= "gd_rJbgUjf30oN4hVWOjChRfvjYmzhcX"
    )
  )
  dockerEnv <- function(name, authProd, authPolicy, protocol="http") {
    list(
      name= name,
      product= append(list(
        url=paste0(protocol,'://api.',name,'.save-up.net')
      ), authProd),
      policy= append(list(
        url=paste0(protocol,'://policy-api.',name,'.save-up.net')
      ), authPolicy)
    )
  }
  environments= list(
    list(
      name="local",
      product= list(
        url= "http://localhost:3000",
        auth0Url= "https://thetaris.auth0.com",
        clientSecret= "z2XIDIBgCFmQfb0sbfophzn4fZlmY72xaXD22x8MONIHMoIYh44kWFMypheM6Gay",
        clientId= "nNKKFRz7kRnIVlMot6nKtIFEzUVn4tOI"
      ),
      policy= list(
        url= "http://localhost:3033",
        auth0Url= "https://thetaris.auth0.com",
        clientSecret= "z2XIDIBgCFmQfb0sbfophzn4fZlmY72xaXD22x8MONIHMoIYh44kWFMypheM6Gay",
        clientId= "nNKKFRz7kRnIVlMot6nKtIFEzUVn4tOI"
      )
    ),
    list(
      name="ff",
      product= list(
        url= "https://api.vestup.de",
        auth0Url= "https://prod-ff.eu.auth0.com",
        clientSecret="-vE6J_y5e2rNnDxmoCnRhnXFdnZ-rILeQkJdNcKq4BAacQh9zqIllUz07QuBa5nM",
        clientId= "Ne8SHbsoNnfL6Tk_cK3vlRHLjVtyx5Xb"
      ),
      policy= list(
        url= "https://policy-api.vestup.de",
        auth0Url= "https://prod-ff-admin.eu.auth0.com",
        clientSecret= "AVBXAabIFcHTQZaHTztkDq1vLxNcTv_G7m5_3fmO5xYayB9mKT9Dag_jYW-Fqv5w",
        clientId= "9XPpd3gb0H0HusBL-Zw1tZbuiYUuDBVx"
      )
    ),
    list(
      name="ff-migration",
      product= list(
        url= "https://api.ff-migration.save-up.net",
        auth0Url= "https://prod-ff.eu.auth0.com",
        clientSecret="-vE6J_y5e2rNnDxmoCnRhnXFdnZ-rILeQkJdNcKq4BAacQh9zqIllUz07QuBa5nM",
        clientId= "Ne8SHbsoNnfL6Tk_cK3vlRHLjVtyx5Xb"
      ),
      policy= list(
        url= "https://policy-api.ff-migration.save-up.net",
        auth0Url= "https://prod-ff-admin.eu.auth0.com",
        clientSecret= "AVBXAabIFcHTQZaHTztkDq1vLxNcTv_G7m5_3fmO5xYayB9mKT9Dag_jYW-Fqv5w",
        clientId= "9XPpd3gb0H0HusBL-Zw1tZbuiYUuDBVx"
      )
    ),
    dockerEnv('accounting', auth$thetaris, auth$thetaris),
    dockerEnv('ff-accounting', auth$thetaris, auth$thetaris),
    dockerEnv('msv-accounting', auth$thetaris, auth$thetaris),
    dockerEnv('msv', auth$thetaris, auth$thetaris),
    dockerEnv('fidelidade', auth$fiddev, auth$fiddev),
    dockerEnv('ff-develop', auth$thetaris, auth$thetaris,'https'),
    dockerEnv('fid-develop', auth$thetaris, auth$thetaris),
    dockerEnv('msv-develop', auth$thetaris, auth$thetaris,'https'),
    dockerEnv('ff-alpha', auth$thetaris, auth$thetaris,'https'),
    dockerEnv('ff-loadtest', auth$thetaris, auth$thetaris,'https'),
    dockerEnv('fid-alpha', auth$thetaris, auth$thetaris,'https'),
    dockerEnv('msv-alpha', auth$thetaris, auth$thetaris,'https'),
    dockerEnv('msv-training', auth$thetaris, auth$thetaris),
    dockerEnv('ff-beta', auth$thetaris, auth$thetaris,'https'),
    dockerEnv('vestup-mirror', auth$thetaris, auth$thetaris,'https'),
    dockerEnv('fid-beta', auth$fiddev, auth$fiddev),
    dockerEnv('msv-beta', auth$msvdev, auth$msvdev,'https'),
    dockerEnv('msv-realmoney', auth$thetaris, auth$thetaris),
    dockerEnv('msv-stable', auth$msvdev, auth$msvdev),
    dockerEnv('msv-mirror', auth$msvdemo, auth$msvdemo,'https'),
    # dockerEnv('stephan-test', auth$thetaris, auth$thetaris, 'https'),
 
    
    
    
       list(
      name="savvisave",
      product= list(
        url= "https://api.savvisave.com",
        auth0Url= "https://prod-msv.eu.auth0.com",
        clientSecret="_sFcwhfERAnsePFAlbPP_b05hOK6WORaYZzUVlBmxpkrY3r1FCatb-_8u_OMZ7-w",
        clientId= "8Hn-Ce2mRvTXmxJaj3ofTaK1avmf9wn"
      ),
      policy= list(
        url= "https://policy-api.savvisave.com",
        auth0Url= "https://prod-ff-admin.eu.auth0.com",
        clientSecret= "AVBXAabIFcHTQZaHTztkDq1vLxNcTv_G7m5_3fmO5xYayB9mKT9Dag_jYW-Fqv5w",
        clientId= "9XPpd3gb0H0HusBL-Zw1tZbuiYUuDBVx"
      )
    )
  )
  names = sapply(environments, function(x) x$name)
  
  if (exists('saveupConnection') && onlyIfUndefined && is.na(name)) {
    # do nothing
  } else {
    if (is.na(name)) {
      sel= menu(names, title="Which environment do you want to connect to?")
      name= names[sel]
    } else {
      sel= which(names==name)
    }
    
    if (length(sel)==0) stop('unknown environment name')
    saveupConnection <<- environments[[sel]]
  }
  cat(paste("Connected to servers ",saveupConnection$product$url,"and",saveupConnection$policy$url,'\n'))
}

auth0GetToken= function(connection, username, password) {
  body = list(
    client_id= connection$clientId,
    client_secret= connection$clientSecret,
    username= username,
    password= password,
    connection= "Username-Password-Authentication",
    grant_type= "password",
    scope= "openid",
    isSocial = FALSE
  )
  response= POST(url=paste(connection$auth0Url,"oauth","token",sep="/"), body=body, encode="json")
  if (response$status_code != 200) {
    stop("Login failed: ", content(response,"text"))
  }
  content(response, "parsed")
}