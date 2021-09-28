anonymize_sha1 <- function(x, digits = 10){
	x <- as.character(x)
	substr(openssl::sha1(x = x), 1, digits)
}
