-module(apptools_image).
-export([dimensions/1]).

-type dimensions() :: {non_neg_integer(), non_neg_integer()}.

%%%-------------------------------------------------------------------
%%%  Extract width × height from PNG/JPEG files without decoding
%%%
%%%  References
%%%    • PNG:  ISO/IEC 15948:2004 - W3C PNG 1.2 Recommendation,
%%%             11.2.4 "IHDR Image header”
%%%    • JPEG: ITU-T Rec.T.81 (1992) | ISO/IEC 10918-1:1994,
%%%             Annex F 2.2.2 "Start-Of-Frame markers”
%%%    • JFIF: JPEG File Interchange Format v1.02 (ISO/IEC 10918 Annex B),
%%%            for APP0 segment layout
%%%
%%%  The code reads:
%%%      • PNG: Bytes 16-23 of the IHDR chunk (big-endian 32-bit Wwidth x Height)
%%%      • JPEG: first SOF0/2/... marker, bytes 3-6 after the marker
%%%
%%%  No external libraries; only header bytes are touched.
%%%-------------------------------------------------------------------

%%
%% Exported: dimensions
%%

-spec dimensions(file:filename()) ->
          {ok, dimensions()} |
          {error, file:posix() | badarg | system_limit |
           unknown_format | corrupt_png | corrupt_jpeg | no_sof}.

dimensions(File) ->
    case file:open(File, [read, binary, raw]) of
        {ok, Fd} ->
            Result = detect(Fd),
            _ = file:close(Fd),
            Result;
        {error, Reason} ->
            {error, Reason}
    end.

detect(Fd) ->
    case file:read(Fd, 8) of
        {ok, <<16#89504E470D0A1A0A:64>>} ->
            file:position(Fd, {bof, 0}),
            read_png(Fd);
        {ok, <<16#FF,16#D8,_/binary>>} ->
            file:position(Fd, {bof, 0}),
            read_jpeg(Fd);
        _ ->
            {error, unknown_format}
    end.

%%
%% PNG
%%

read_png(Fd) ->
    case file:read(Fd, 24) of
        {ok, <<16#89504E470D0A1A0A:64,
               0, 0, 0, 13, %% IHDR length
               "IHDR",
               Width:32/big, Height:32/big>>} ->
            {ok, {Width, Height}};
        _ ->
            {error, corrupt_png}
    end.

%%
%% JPEG
%%

-define(IS_SOF(M),
        (M band 16#F0) =:= 16#C0 andalso
        M =/= 16#C4 andalso
        M =/= 16#C8 andalso
        M =/= 16#CC).

-define(ZERO_LEN(M),
        (M =:= 16#D8) orelse
        (M =:= 16#D9) orelse
        (M >= 16#D0 andalso M =< 16#D7) orelse
        (M =:= 16#01)).

read_jpeg(Fd) ->
    skip(Fd, 2), %% Skip SOI
    scan(Fd).

scan(Fd) ->
    case next_marker(Fd) of
        eof  ->
            {error, no_sof};
        16#D9 ->
            {error,no_sof}; %% Hit EOI first
        M when ?IS_SOF(M) ->
            %% seg-len(2) | precision(1) | height(2) | width(2)
            case file:read(Fd, 7) of
                {ok, <<_Len:16/big, _Prec:8, Height:16/big, Width:16/big>>} ->
                    {ok, {Width, Height}};
                _ ->
                    {error, corrupt_jpeg}
            end;
        M when ?ZERO_LEN(M) ->
            scan(Fd);
        _M ->
            %% Variable-length segment: Read its 2-byte length and skip
            case file:read(Fd, 2) of
                {ok, <<SegLen:16/big>>} ->
                    ok = skip(Fd, SegLen - 2),
                    scan(Fd);
                _ ->
                    {error,corrupt_jpeg}
            end
    end.

next_marker(Fd) ->
    case file:read(Fd, 1) of
        eof ->
            eof;
        {ok, <<16#FF>>} ->
            consume_ff(Fd);
        _ ->
            next_marker(Fd)
    end.

consume_ff(Fd) ->
    case file:read(Fd,1) of
        eof ->
            eof;
        {ok, <<16#FF>>} ->
            consume_ff(Fd); %% Padding: 0xFF FF FF ...
        {ok,<<M>>} ->
            M %% Real marker (including 0x00)
    end.

skip(_, 0) ->
    ok;
skip(Fd, N) ->
    file:position(Fd, {cur, N}),
    ok.
