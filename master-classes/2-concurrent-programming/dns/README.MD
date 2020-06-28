# Master Class 3 - Concurrent Programming

## Client-server: name-server and resolver

### DNS Evaluator

#### Activity

+ Make and expression evaluator
+ Mirror the structure of the Internet
+ DNS-resolver-client-server model
+ Start simple
+ "Textbook" Erlang - no OTP generics
+ Migrate code into OTP structure while keeping clarity of the original design

#### About a simple DNS

+ Name server
+ Converts names to addresses
+ Replicated
+ Hides a lot of complexity
+ Resolver
+ Client software that accesses the name server

#### Notes

+ Mirrors the (abstract) DNS design.
+ Can be used to test the logic of the server
+ Most of work is in writing the semantics of the server. The rest is boilerplate
+ Starting/stopping/logging is more complicated than suggested here (covered in next lecture)
