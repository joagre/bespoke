# Bespoke - A location-based BBS

## Pre-requisites

- [Erlang](http://www.erlang.org/)
- [Libsodium](https://doc.libsodium.org/)

## Build

```
$ make all setcap
```

## Test

```
$ make runtest
```
## Start server and populate database

```
$ ./main/bin/bespoke
```

Optionally populate the database with data taken from r/sweden

```
Erlang/OTP 27 [erts-15.1.2] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [jit:ns]

(bespoke@localhost)1> db_tools:create_subreddit_db().
...
```

## Test with curl (in another shell)

```
$ curl -k http://localhost/list_root_messages
```
