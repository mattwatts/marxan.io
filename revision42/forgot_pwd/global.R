# marxan.io Run Marxan

cat(paste0("hello\n"))
cat(paste0(getwd(),"\n"))

sAppDir <<- getwd()

sShinyPath <<- "/mnt/shiny/"
sShinyRegisterPath <<- paste0(sShinyPath,"register/")
sShinyTempPath <<- paste0(sShinyPath,"temp/")
sAppServer <<- "marxan.io"

iRevision <- 42
sUserGuide <<- "Marxan_io_rev39_user_guide.pdf"

sPwdResetApp <<- paste0("revision",iRevision,"/reset")

if (.Platform$pkgType == "source")
{
  sShinySourcePath <<- paste0(sShinyPath,"apps/revision",iRevision,"/source/")
} else {
  sShinySourcePath <<- paste0("/Users/matt/Documents/R/revision",iRevision,"/source/")
}

irefreshtable <<- 0
fInformAuthors <<- FALSE
fAcknowledgeIP <<- FALSE
sName <<- ""
sOrganisation <<- ""
sEmail <<- ""

isendemail <<- 0

