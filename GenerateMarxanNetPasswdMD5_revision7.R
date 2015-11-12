


passwd <- read.csv("/Users/matt/Documents/R/marxan_net_passwd.csv",stringsAsFactors=FALSE)

library(digest)

for (i in 1:nrow(passwd))
{
  passwd$password[i] <- digest(passwd$password[i], algo="md5", serialize=FALSE)
}

save(passwd,file="/Users/matt/Documents/R/passwd.Rdata")

