-module(vordict).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([new/0, store/3, remove/2, merge/2, value/1]).  %from_list/1, gc/1 ]).

-record(vordict, {stores :: orddict:orddict(),
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
    #vordict{stores = orddict:new(),
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
store(Key, Value, ORDict = #vordict{stores = Stores}) ->
    ORDict#vordict{stores = orddict:update(Key, fun(Value1) ->
                                                  ecrdt:merge(Value1, vlwwregister:new(Value))
                                                end, vlwwregister:new(Value), Stores)}.

%%--------------------------------------------------------------------
%% @doc
%% Removes a element from the OR set by finding all observed adds and
%% putting them in the list of removed items.
%% @end
%%--------------------------------------------------------------------
-spec remove(Key::term(), ORDict::vordict()) -> ORDict1::vordict().
remove(Key, ORDict = #vordict{removes = Removes}) ->
    ORDict#vordict{removes = vgset:add(Key, Removes)}.

%%--------------------------------------------------------------------
%% @doc
%% Merges two OR Sets by taking the union of adds and removes.
%% @end
%%--------------------------------------------------------------------
-spec merge(ORDict0::vordict(), ORDict1::vordict()) -> ORDictM::vordict().
merge(#vordict{stores = Stores0,
             removes = Removes0},
      #vordict{stores = Stores1,
             removes = Removes1}) ->
    #vordict{stores = orddict:merge(fun(_Key, Value1, Value2) ->
                                      ecrdt:merge(Value1, Value2)
                                    end, Stores0, Stores1),
           removes = vgset:merge(Removes0, Removes1)}.

%%--------------------------------------------------------------------
%% @doc
%% Retrives the list of values from an OR Set by taking the
%% substract of adds and removes then eleminating the ID field.
%% @end
%%--------------------------------------------------------------------
-spec value(ORDict::vordict()) -> [Element::term()].
value(ORDict) ->
    ordsets:from_list([ecrdt:value(E) || E <- raw_value(ORDict)]).

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

-spec raw_value(ORDict::vordict()) -> [Element::vlwwregister:vlwwregister()].
raw_value(#vordict{stores = Stores,
                 removes = Removes}) ->
    RemovesRaw = vgset:value(Removes),
    orddict:filter(fun(K, _V) ->
                     not lists:member(K, RemovesRaw)
                   end, Stores).

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
    list({oneof([a, b, ab]), oneof([store, remove]), atom(), pos_integer()}).

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
