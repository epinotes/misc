anonymize_sha1 <- function(x, digits = 10){
	substr(openssl::sha1(x = x), 1, digits)
}
