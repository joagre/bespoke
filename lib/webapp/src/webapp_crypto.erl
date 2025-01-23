-module(webapp_crypto).
-export([generate_challenge/0,
         verify_client_response/3,
         generate_rsa_key/0,
         sign_file/2]).
-export_type([challenge/0, client_response/0]).

-include_lib("public_key/include/public_key.hrl").

-define(CHALLENGE_SIZE, 32).

-type challenge() :: binary().
-type client_response() :: binary().
-type public_identity_key() :: binary().
-type private_identity_key() :: binary().

%%
%% Exported: generate_challenge
%%

-spec generate_challenge() -> challenge().

generate_challenge() ->
    crypto:strong_rand_bytes(?CHALLENGE_SIZE).

%%
%% Exported: verify_client_response
%%

-spec verify_client_response(
        client_response(), challenge(), db_user_serv:password_hash()) -> boolean().

verify_client_response(ClientResponse, Challenge, PasswordHash) ->
    ExpectedResponse = crypto:mac(hmac, sha256, PasswordHash, Challenge),
    crypto:hash_equals(ExpectedResponse, ClientResponse).

%%
%% generate_rsa_key
%%

-spec generate_rsa_key() -> ok.

%% https://blog.differentpla.net/blog/2023/02/07/generate-rsa-key-erlang/
generate_rsa_key() ->
    Size = 2048,
    Exp = 65537,
    RSAPrivateKey = public_key:generate_key({rsa, Size, Exp}),
    RSAPrivateKeyPEM =
        public_key:pem_encode([public_key:pem_entry_encode('RSAPrivateKey',
                                                           RSAPrivateKey)]),
    #'RSAPrivateKey'{modulus = Modulus, publicExponent = PublicExponent} =
        RSAPrivateKey,
    RSAPublicKey =
        #'RSAPublicKey'{modulus = Modulus, publicExponent = PublicExponent},
    RSAPublicKeyPEM =
        public_key:pem_encode(
          [public_key:pem_entry_encode('RSAPublicKey', RSAPublicKey)]),
    {RSAPublicKeyPEM, RSAPrivateKeyPEM}.

%%
%% sign_file
%%

sign_file(FilePath, PrivateKey) ->
    sign_file(FilePath, PrivateKey, sha256).

sign_file(FilePath, PrivateKey, Algorithm) ->
    Hash = hash_file(FilePath, Algorithm),
    Signature = public_key:sign(Hash, Algorithm, PrivateKey),
    base64:encode_to_string(Signature).

hash_file(FilePath, Algorithm) ->
    {ok, File} = file:open(FilePath, [read, binary]),
    HashCtx = crypto:hash_init(Algorithm),
    case read_chunks(File, HashCtx) of
        {ok, HashCtxFinal} ->
            file:close(File),
            crypto:hash_final(HashCtxFinal);
        {error, Reason} ->
            file:close(File),
            {error, Reason}
    end.

read_chunks(File, HashCtx) ->
    case file:read(File, 4096) of
        {ok, Chunk} ->
            NewCtx = crypto:hash_update(HashCtx, Chunk),
            read_chunks(File, NewCtx);
        eof ->
            {ok, HashCtx};
        {error, Reason} ->
            {error, {file_read_error, Reason}}
    end.
