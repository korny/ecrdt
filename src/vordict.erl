-module(vordict).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([new/0, store/3, remove/2, merge/2, value/1]).  %from_list/1, gc/1 ]).

-record(vordict, {adds :: vgset:vgset(),
                 removes :: vgset:vgset()}).

-opaque vordict() :: #vordict{}.

-export_type([vordict/0]).

%%%===================================================================
%%% Implementation
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a new empty OR Set.
%% @end
%%--------------------------------------------------------------------
-spec new() -> vordict().
new() ->
    #vordict{adds = vgset:new(),
            removes = vgset:new()}.

%%--------------------------------------------------------------------
%% @doc
%% Creates a new OR Set form a list by adding each element in the
%% list.
%% @end
%%--------------------------------------------------------------------
% -spec from_list(list()) -> vorset().
% from_list(L) ->
%     ID = ecrdt:id(),
%     #vorset{adds = vgset:from_list([ {E, ID} || E <- L]),
%            removes = vgset:new()}.

%%--------------------------------------------------------------------
%% @doc
%% Adds an element to the OR set using the default ID function
%% provided by ecrdt:id().
%% @end
%%--------------------------------------------------------------------
-spec store(Key::term(), Value::term(), ORDict::vordict()) -> ORDict1::vordict().
store(Key, _Value, ORDict = #vordict{adds = Adds}) ->
    ORDict#vordict{adds = vgset:add({Key, ecrdt:id()}, Adds)}.

%%--------------------------------------------------------------------
%% @doc
%% Removes a element from the OR set by finding all observed adds and
%% putting them in the list of removed items.
%% @end
%%--------------------------------------------------------------------
-spec remove(Key::term(), ORDict::vordict()) -> ORDict1::vordict().
remove(Key, ORDict = #vordict{removes = Removes}) ->
    CurrentExisting = [Elem || Elem = {E1, _} <- raw_value(ORDict), E1 =:= Key],
    Removes1 = lists:foldl(fun(R, Rs) ->
                                  vgset:add(R, Rs)
                          end, Removes, CurrentExisting),
    ORDict#vordict{removes = Removes1}.

%%--------------------------------------------------------------------
%% @doc
%% Merges two OR Sets by taking the union of adds and removes.
%% @end
%%--------------------------------------------------------------------
-spec merge(ORDict0::vordict(), ORDict1::vordict()) -> ORDictM::vordict().
merge(#vordict{adds = Adds0,
             removes = Removes0},
      #vordict{adds = Adds1,
             removes = Removes1}) ->
    #vordict{adds = vgset:merge(Adds0, Adds1),
           removes = vgset:merge(Removes0, Removes1)}.

%%--------------------------------------------------------------------
%% @doc
%% Retrives the list of values from an OR Set by taking the
%% substract of adds and removes then eleminating the ID field.
%% @end
%%--------------------------------------------------------------------
-spec value(ORDict::vordict()) -> [Element::term()].
value(ORDict) ->
    ordsets:from_list([E || {E, _} <- raw_value(ORDict)]).

%%--------------------------------------------------------------------
%% @doc
%% Garbage collects a OR set by storing the currently existing values
%% only as adds and clearing the removes.
%%
%% Be aware that this needs to be carried out syncronously or will
%% lead to unexpected results!
%% @end
%%--------------------------------------------------------------------
% -spec gc(ORSet::vorset()) -> ORSetGCed::vorset().
% gc(ORSet) ->
%     #vorset{adds = vgset:from_list(raw_value(ORSet)),
%            removes = vgset:new()}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec raw_value(ORDict::vordict()) -> orddict:orddict().
raw_value(#vordict{adds = Adds,
                 removes = Removes}) ->
    ordsets:subtract(vgset:value(Adds), vgset:value(Removes)).

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

op(a, store, K, V, C1, C2, Check) ->
    _ID = ecrdt:id(),
    {store(K, V, C1), C2, store(K, V, Check)};
op(b, store, K, V, C1, C2, Check) ->
    _ID = ecrdt:id(),
    {C1, store(K, V, C2), store(K, V, Check)};
op(ab, store, K, V, C1, C2, Check) ->
    _ID = ecrdt:id(),
    {store(K, V, C1), store(K, V, C2), store(K, V, Check)};
op(a, remove, K, _V, C1, C2, Check) ->
    {remove(K, C1), C2, remove(K, Check)};
op(b, remove, K, _V, C1, C2, Check) ->
    {C1, remove(K, C2), remove(K, Check)};
op(ab, remove, K, _V, C1, C2, Check) ->
    {remove(K, C1), remove(K, C2), remove(K, Check)}. %;

%% Applies the list of opperaitons to three empty dicts.
apply_ops(Ops) ->
    lists:foldl(fun({T, O, K, V}, {A, B, C}) ->
                        op(T, O, K, V, A, B, C)
                end, {new(), new(), new()}, Ops).

%% A list of opperations and targets.
targets() ->
    list({oneof([a, b, ab]), oneof([store, remove]), pos_integer(), pos_integer()}).

prop_vordict() ->
    ?FORALL(Ts,  targets(),
            begin
                {A, B, C} = apply_ops(Ts),
                value(C) =:= value(merge(merge(A, B), C))
            end).

propper_test() ->
    ?assertEqual([],
                 proper:module(
                   ?MODULE,
                   [{numtests, 1000},
                    {to_file, user},
                    long_result])).

-endif.
