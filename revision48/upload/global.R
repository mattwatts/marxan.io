# marxan.io Upload a Dataset

cat(paste0("hello\n"))
cat(paste0(getwd(),"\n"))

sAppDir <<- getwd()

sShinyPath <<- "/mnt/shiny/"
sShinyUserPath <<- paste0(sShinyPath,"users/")
sShinyDataPath <- paste0(sShinyPath,"data/")
sShinyTempPath <<- paste0(sShinyPath,"temp/")

iRevision <- 48
sUserGuide <<- "Marxan_io_rev39_user_guide.pdf"

if (.Platform$pkgType == "source")
{
  sShinySourcePath <<- paste0(sShinyPath,"apps/revision",iRevision,"/source/")
} else {
  sShinySourcePath <<- paste0("/Users/matt/Documents/R/revision",iRevision,"/source/")
}

iupdateusermessages <<- 0
puid_choices <<- c("a","b")
iupdatepuidchoices <<- 0
fUserMessagesFile <<- FALSE
input_messages_df <<- c()#as.data.frame(c("a"))