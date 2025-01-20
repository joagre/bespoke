<!-- -*- mode: markdown; fill-column: 100 -*- -->

# Password hashing

`->` is a REST request and `<-` is a REST response

## Create password

A user has no password but creates one

```
Client                                                                                        Server
------                                                                                        ------
salt = mk_rand_bytes(16)
hash = mk_argon2_hash(salt, password)
/api/create_password(username, salt, hash) ->

                                                               store_in_passwd(username, salt, hash)
                                                                                               <- ok
```

## Login

A user provides a username and password in order to login

```
Client                                                                                        Server
------                                                                                        ------
/api/generate_challenge(username) ->

                                                                       challenge = mk_rand_bytes(32)
                                                                 salt = passwd_lookup_salt(username)
                                                                  [Remember challenge for 5 minutes]
                                                                                  <- challenge, salt

hash = mk_argon2_hash(salt, password)
client_response = mk_sha256_hmac(challenge, hash)
/api/login(client_response) ->

                                                          response = mk_sha256_hmac(challenge, hash)
                                                                      <- response == client_repsonse
```
