encrypt <- 
  function(msg, 
           pool = LETTERS, 
           do_replace = FALSE, 
           cheat = FALSE
          ) {
    
    msg_chars <- strsplit(msg,"")[[1]]

    u <- unique(msg_chars)

    encrypt_chars <- 
      sample(pool,
             size = length(u),
             replace = do_replace 
                                         
      )
    
    encrypt_chars <- 
      setNames(encrypt_chars, # vector to name
               u              # vector of names
      )

    if (cheat)
      # return the full map ..
      result <- encrypt_chars[msg_chars]
    else 
      # .. or just the encrypted message  
      result <- paste(encrypt_chars[msg_chars], 
                      collapse="")
      
    
    return(result)
  }

