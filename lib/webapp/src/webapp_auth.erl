-module(webapp_auth).
-export([generate_challenge/0, verify_client_response/3]).
-export_type([salt/0, challenge/0, client_response/0]).

-define(CHALLENGE_SIZE, 32).

-type salt() :: binary().
-type challenge() :: binary().
-type client_response() :: binary().

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
        client_response(), challenge(), db_user_serv:pwhash()) -> boolean().

verify_client_response(ClientResponse, Challenge, Pwhash) ->
    ExpectedResponse = crypto:mac(hmac, sha256, Pwhash, Challenge),
    crypto:hash_equals(ExpectedResponse, ClientResponse).
