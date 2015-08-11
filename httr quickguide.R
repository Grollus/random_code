library(httr)
r <- GET("http://httpbin.org/get")
r

status_code(r)
headers(r)

str(content(r))

#Parts of http RESPONSE
# status code - code summarising whether or not request was successful
http_status(r)
warn_for_status(r)
stop_for_status(r)

#body
content(r, "text", encoding = "ISO-8859-1")

content(r, "raw")
bin <- content(r, "raw")
writeBin(bin, "myfile.txt")
str(content(r, "parsed"))

#headers
headers(r)
headers(r)$date
headers(r)$DaTE

r <- GET("http://httpbin.org/cookies/set", query = list(a = 1))
cookies(r)
r <- GET("http://httpbin.org/cookies/set", query = list(b = 1))
cookies(r

#REQUEST
# status line defines the http method and the url

# url query string
r <- GET("http://httpbin.org/get",
         query = list(key1 = "value1", key2 = "value2"))
content(r)$args
content(r)

#HEADERS
r <- GET("http://httpbin.org/get", add_headers(Name = "Adam"))
str(content(r)$headers)
# content(r)$header retrieves the headers that httpbin received. REQUEST
# headers(r) gives the headers that it sent back in its RESPONSE

#sending/setting your own cookies
r <- GET("http://httpbin.org/cookies", set_cookies("MeWant" = "cookies"))
content(r)$cookies

# BODY

r <- POST("http://httpbin.org/post", body = list(a = 1, b = 2, c = 3))
url <- "http://httpbin.org/post"
body <- list(a = 1, b = 2, c = 3)

r <- POST(url, body = body, encode = "form", verbose())
r <- POST(url, body = body, encode = "multipart", verbose())
r <- POST(url, body = body, encode = "json", verbose())

POST(url, body = upload_file("mypath.txt"))
POST(url, body = list(x = upload_file("mypath.txt")))
