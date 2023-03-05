
exception Panic(message : String) = "PANIC! (the impossible happened): " ~ message 


raise Panic("AAAAAAAA")
print("EXCEPTION DID NOT ABORT THE PROGRAM!!!")
