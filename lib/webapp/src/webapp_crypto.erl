% -*- fill-column: 100; -*-

-module(webapp_crypto).
-export([generate_challenge/0, verify_client_response/3, generate_rsa_key/2, load_native_rsa_key/1,
         sign_file/1, sign_file/2]).
-export_type([challenge/0, client_response/0]).

-include_lib("public_key/include/public_key.hrl").

-define(CHALLENGE_SIZE, 32).

-type challenge() :: binary().
-type client_response() :: binary().

-define(PRIVATE_KEY_FILE_PATH, "/home/jocke/.bespoke/keys/bespoke.key").

%%
%% Exported: generate_challenge
%%

-spec generate_challenge() -> challenge().

generate_challenge() ->
    crypto:strong_rand_bytes(?CHALLENGE_SIZE).

%%
%% Exported: verify_client_response
%%

-spec verify_client_response(client_response(), challenge(), db_user_serv:password_hash()) ->
          boolean().

verify_client_response(ClientResponse, Challenge, PasswordHash) ->
    ExpectedResponse = crypto:mac(hmac, sha256, PasswordHash, Challenge),
    crypto:hash_equals(ExpectedResponse, ClientResponse).

%%
%% generate_rsa_key
%%

-spec generate_rsa_key(file:filename(), file:filename()) ->
          {file:filename(), file:filename(), file:filename(), file:filename()}.


%% https://blog.differentpla.net/blog/2023/02/07/generate-rsa-key-erlang/
%% webapp_crypto:generate_rsa_key("/home/jocke/.bespoke/keys", "bespoke").
generate_rsa_key(DirPath, Name) ->
    Size = 2048,
    Exp = 65537,
    RSAPrivateKey = public_key:generate_key({rsa, Size, Exp}),
    #'RSAPrivateKey'{modulus = Modulus, publicExponent = PublicExponent} = RSAPrivateKey,
    RSAPublicKey = #'RSAPublicKey'{modulus = Modulus, publicExponent = PublicExponent},
    SubjectPublicKeyInfo =
        #'SubjectPublicKeyInfo'{
           algorithm = #'AlgorithmIdentifier'{
                          algorithm = {1,2,840,113549,1,1,1},
                          parameters = <<5,0>>},
           subjectPublicKey = public_key:der_encode('RSAPublicKey', RSAPublicKey)},
    %% Save private key in term format
    RSAPrivateKeyFilePath = filename:join([DirPath, Name]) ++ ".key",
    ok = file:write_file(RSAPrivateKeyFilePath, base64:encode(term_to_binary(RSAPrivateKey))),
    %% Save public key in term format
    RSAPublicKeyFilePath = filename:join([DirPath, Name]) ++ ".pub",
    ok = file:write_file(RSAPublicKeyFilePath, base64:encode(term_to_binary(RSAPublicKey))),
    %% Save private key in PEM format (PKCS#1)
    RSAPrivateKeyPEM =
        public_key:pem_encode([public_key:pem_entry_encode('RSAPrivateKey', RSAPrivateKey)]),
    RSAPrivateKeyPEMFilePath = filename:join([DirPath, Name]) ++ "-pem.key",
    ok = file:write_file(RSAPrivateKeyPEMFilePath, RSAPrivateKeyPEM),
    %% Save public key in PEM format (PKCS#8)
    RSAPublicKeyPEM =
        public_key:pem_encode(
          [public_key:pem_entry_encode('SubjectPublicKeyInfo', SubjectPublicKeyInfo)]),
    RSAPublicKeyPEMFilePath = filename:join([DirPath, Name]) ++ "-pem.pub",
    ok = file:write_file(RSAPublicKeyPEMFilePath, RSAPublicKeyPEM),
    {RSAPrivateKeyFilePath, RSAPrivateKeyPEMFilePath,
     RSAPublicKeyFilePath, RSAPublicKeyPEMFilePath}.

%%
%% load_native_rsa_key
%%

-spec load_native_rsa_key(file:filename()) -> public_key:rsa_private_key().

load_native_rsa_key(FilePath) ->
    {ok, KeyBin} = file:read_file(FilePath),
    binary_to_term(base64:decode(KeyBin)).

%%
%% sign_file
%%

-spec sign_file(file:filename()) -> no_return().

sign_file(FilePath) ->
    PrivateKey = load_native_rsa_key(?PRIVATE_KEY_FILE_PATH),
    sign_file(FilePath, PrivateKey).

-spec sign_file(file:filename(), public_key:rsa_private_key()) -> no_return().

sign_file(FilePath, PrivateKey) ->
    {ok, FileBin} = file:read_file(FilePath),
    Signature = public_key:sign(FileBin, sha256, PrivateKey),
    io:format("~s", [base64:encode_to_string(Signature)]),
    halt().
