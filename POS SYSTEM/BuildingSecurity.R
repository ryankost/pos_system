library(sodium)


inputPassword <- 'test123'

hashFunction <- function(x){
  y <- as.character(x)
  passphrase <- charToRaw(y)
  sodium::scrypt(passphrase,size=64)
}

df <- data.frame(login = 'adminUser',
                 pswd = I(list(hashFunction(inputPassword))),
                 perm = 'admin')

testUserdf <- data.frame(login = 'testuser',
                               pswd = I(list(hashFunction('testuser'))),
                               perm = 'standard')
df <- rbind(df,testUserdf)

saveRDS(df,'authentication.rds')

print(identical(df[which(df$login=='ryankost'),2][[1]],hashFunction(inputPassword)))