<!-- -*- mode: markdown; fill-column: 100 -*- -->

# Periodic ephemeral key rotation

`->` is a REST request and `<-` is a REST response

## Server setup

Extend user DETS record with user's public identity key:

```
-record(user,
        {
         id               :: db_serv:user_id(),
         pub_identity_key :: db_serv:pub_identity_key(),
         ...
        }
```

Create new ephemerel_key DETS table:

```
-record(ephemeral_key,
        {
         id         :: db_serv:user_id(),
         pub_key    :: db_serv:ephemeral_pub_key(),
         signature  :: db_serv:ephemeral_pub_key_signature(),
         valid_from :: db_serv:seconds_since_epoch(),
         valid_to   :: db_serv:seconds_since_epoch(),
         active     :: yes | no | expiring
        }).
```

Bob uses this REST resource to add a new ephemeral key:

```
Client                                                                                        Server
------                                                                                        ------
/api/update_ephemeral_pub_key(id, pub_key, signature, valid_from, valid_to) ->

     valid = crypto_verify_detached(signature, pub_key, crypto_box_PUBLICKEYBYTES, pub_identity_key)
                                                                                            <- valid
                                       [Store in ephemeral_key table if valid and set active to yes]
                                                     [After valid_to the ephemeral key is set to no]
```

Alice uses this REST resource to ask for Bob's ephemeral public key:

```
/api/get_ephemeral_pub_key(id) ->

                                                       <- {pub_key, valid_from, valid_to, signature}

[Alice verifies the signature (see above) using Bob's public identity key]
```

## Client setup

Clients generate and store a unique identity keypair for each user account. For now the local
storage in the Web browser is used for this. This can indeed be made better.

``
const unsigned char pub_identity_key[crypto_sign_PUBLICKEYBYTES];
unsigned char sec_identity_key[crypto_sign_SECRETKEYBYTES];
const userId = bespoke.getCookieValue("userId");
bespoke.setLocalItem(`pubIdentityKey-${userId}`, pub_identity_key);
```
