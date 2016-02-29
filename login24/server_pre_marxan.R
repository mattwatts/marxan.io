# marxan.io

CreateLogFile <- function(sPath,sID,sCallingApp)
{
  Gfold <- sprintf("%s",round(runif(1)*1000000))
  for (ii in 1:100000){
    sFile <- sprintf("%s/%s_%s_%s.log",sPath,sCallingApp,sID,Gfold)
    if(!file.exists(sFile)) {
      write(paste0(date()," start log ",sID),file=sFile)
      break()
    }
  }
  return(sFile)
}

AppendLogFile <- function(sMessage)
{
  write(paste0(date()," ",sMessage),file=sLogFile,append=TRUE)
}

InitialiseUserSession <- function()
{
    # initialise the user session key and file
    sSessionsDir <- paste0(sShinyPath,"sessions/")
    dir.create(sSessionsDir)
    SessionLoginDate <- Sys.time()
    sSessionUserName <- sUserName
    sSessionUserIP <- sUserIP
    
    repeat
    ({
        sUserSessionKey <<- gen_pwd()
        sUserSessionKeyFile <- paste0(sSessionsDir,sUserSessionKey,".Rdata")

        if (!file.exists(sUserSessionKeyFile))
        {
            # create the session key file
            save(sSessionUserName,SessionLoginDate,sUserSessionKey,sSessionUserIP,file=sUserSessionKeyFile)
            sText <- paste0("sessionkey: ", sUserSessionKey,"\n",
                            "sessionkeyfile: ", sUserSessionKeyFile,"\n",
                            "username: ",sSessionUserName,"\n",
                            "logindate: ", SessionLoginDate, "\n",
                            "userip: ", sSessionUserIP)
            AppendLogFile(sText)
            cat(paste0(sText,"\n"))
            
            break
        }
    })
}

InitialiseUser <- function()
{
    cat(paste0("InitialiseUser start ",sUserName,"\n"))

    # if the users home directory doesn't exist, create it and populate it with a sample dataset
    sUserHome <<- paste0(sShinyUserPath,sUserName)
    if (!file.exists(sUserHome))
    {
        dir.create(sUserHome)
        system(paste0("unzip ",sShinyDataPath,"/",sSampleDataset,".zip -d ",sUserHome))
    }

    # if the users apps don't exist, create them
    sUserApps <- paste0(sShinyPath,"apps/",sUserName,"/")
    dir.create(sUserApps)
    sUserUploadApp <- paste0(sUserApps,sUploadApp,"/")
    if (!file.exists(sUserUploadApp))
    {
        dir.create(sUserUploadApp)
        sCpCmd <- paste0("cp -r ",sShinyPath,"apps/",sUploadApp,"/* ",sUserUploadApp)
        cat(paste0(sCpCmd,"\n"))
        system(sCpCmd)
    }
    sUserMarxanApp <- paste0(sUserApps,sMarxanApp,"/")
    if (!file.exists(sUserMarxanApp))
    {
        dir.create(sUserMarxanApp)
        sCpCmd <- paste0("cp -r ",sShinyPath,"apps/",sMarxanApp,"/* ",sUserMarxanApp)
        cat(paste0(sCpCmd,"\n"))
        system(sCpCmd)
    }

    # initialise the user session log file
    sLogFile <<- CreateLogFile(paste0(sShinyUserPath,sUserName),sUserName,"login")

    # initialise the user session temp dir
    #sUserSession <<- CreateTempDir(sUserTemp)

    cat("InitialiseUser end\n")
}

freegeoip <- function(ip, format = ifelse(length(ip)==1,'list','dataframe'))
{
    return("unknown")
}   

substrRight <- function(x, n)
{
    substr(x, nchar(x)-n+1, nchar(x))
}

gen_pwd <- function(iLength=16)
# password generator. minimum length is 4
# at least 1 upper case character, 1 lower case character, 1 number
# omit IiLlOo01 so no character confusion when reading/typing
{
    library(stringi)
    rand_all <- stri_rand_strings(n=1, length=iLength-3, pattern="[ABCDEFGHJKMNPQRSTUVWXYZabcdefghjkmnpqrstuvwxyz23456789]")
    rand_upper <- stri_rand_strings(n=1, length=1, pattern="[ABCDEFGHJKMNPQRSTUVWXYZ]")
    rand_lower <- stri_rand_strings(n=1, length=1, pattern="[abcdefghjkmnpqrstuvwxyz]")
    rand_numeric <- stri_rand_strings(n=1, length=1, pattern="[23456789]")
    x <- paste0(rand_all,rand_upper,rand_lower,rand_numeric)
    y <- as.data.frame(strsplit(x,""))
    return(paste(as.character(y[sample(nchar(x)),]),collapse=""))
}

