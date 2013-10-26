%% =============================================================================
%% Streams
%%
%% Streams (also known as lazy lists) are very similar to ordinary lists, except
%% that every cell is systematically suspended.
%%
%% The MIT License (MIT)
%%
%% Copyright (c) 2013 Enrique Fernandez
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell 
%% copies of the Software, and to permit persons to whom the Software is 
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in 
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%%
%% Author contact: efcasado@gmail.com
%% =============================================================================
-module(streams).

-compile({no_auto_import,[length/1]}).

-include("streams.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([take/2, drop/2,
         seq/2, seq/3,
         append/2, 
         reverse/1,
         is_empty/1,
         length/1,
         equal/2,
         foreach/2, foreach/3,
         reduce/3, reduce/4,
         map/2, map/3,
         filter/2,
         filtermap/2,
         from_list/1, to_list/1]).


%%-----------------------------------------------------------------------------
%% @doc
%% Returns a new stream consisting of the first N elements
%% of the given initial stream.
%%
%% This function is computed incrementally.
%% @end
%%-----------------------------------------------------------------------------
-spec take(non_neg_integer(), stream()) -> stream().
take(0, _S) -> ?STREAM();
take(N, S0) -> 
    case S0() of
        {}     -> ?STREAM();
        {X, S} -> ?STREAM(X, take(N - 1, S))
    end.

%%-----------------------------------------------------------------------------
%% @doc
%% Returns a new stream which is the result of appending
%% T to S.
%%
%% This function is computed incrementally.
%% @end
%%-----------------------------------------------------------------------------
-spec append(stream(), stream()) -> stream().
append(S0, T) ->
    case S0() of
        {}     -> T;
        {X, S} -> ?STREAM(X, append(S, T))
    end.

%%-----------------------------------------------------------------------------
%% @doc
%% Returns a new stream consisting of the last M elements in
%% S, where M is equals to length(S) - N.
%% 
%% This function is computed monolithically.
%% @end
%%-----------------------------------------------------------------------------
-spec drop(non_neg_integer(), stream()) -> stream().
drop(0, S) -> S;
drop(N, S0) -> 
    case S0() of
        {}       -> ?STREAM();
        {_X, S}  -> drop(N - 1, S)
    end.

%%-----------------------------------------------------------------------------
%% @doc
%% Creates the specified [F, T] sequence using a unitary interval step.
%% If F > T, the sequence goes downwards.
%% @end
%%-----------------------------------------------------------------------------
seq(F, T) when F < T -> seq(F, T, 1);
seq(F, T) -> seq(F, T, -1).

%%-----------------------------------------------------------------------------
%% @doc
%% Creates the specified [F, T] sequence using the specified interval
%% step.
%% @end
%%-----------------------------------------------------------------------------
seq(F, T, I) when (F =:= T); 
                  (I > 0 andalso (F + I) > T); 
                  (I < 0 andalso (F + I) < T) ->
    ?STREAM(F);
seq(F, T, I) ->
    ?STREAM(F, seq(F + I, T, I)).

%%-----------------------------------------------------------------------------
%% @doc
%% Returns a new stream which consists in a reversed version
%% of the given stream.
%%
%% This functions is computed monolithically.
%% @end
%%-----------------------------------------------------------------------------
-spec reverse(stream()) -> stream().
reverse(S) ->
    reverse(S(), ?STREAM()).

reverse({},     R) -> R;                 
reverse({X, S}, R) -> reverse(S(), ?STREAM(X, R)).

%%-----------------------------------------------------------------------------
%% @doc
%% Returns true if the stream is empty. Else, it returns false.
%% @end
%%-----------------------------------------------------------------------------
-spec is_empty(stream()) -> 'true' | 'false'.
is_empty(S) ->
    S() =:= {}. 

%%-----------------------------------------------------------------------------
%% @doc
%% Returns the length of the given stream.
%%
%% This function is computed monolithically.
%% @end
%%-----------------------------------------------------------------------------
-spec length(stream()) -> non_neg_integer().
length(S) ->
    length(S(), 0).

length({},       N) -> N;
length({_X, S0}, N) -> length(S0(), N + 1).

%%-----------------------------------------------------------------------------
%% @doc
%% Returns true if the two streams hold the same values.Else, it returns 
%% false.
%%
%% This function is computed monolithically.
%% @end
%%-----------------------------------------------------------------------------
-spec equal(stream(), stream()) -> boolean().
equal(S1, S2) -> are_equal(S1(), S2()).

are_equal({},      {})      -> true;
are_equal({X, S1}, {X, S2}) -> are_equal(S1(), S2());
are_equal(_,       _)       -> false.

%%-----------------------------------------------------------------------------
%% @doc
%% Iterates over the given stream executing the provided function on each
%% one of its elements. 
%% @end
%%-----------------------------------------------------------------------------
-spec foreach(fun((term()) -> term()), stream()) -> 'ok'.
foreach(F, S0)  ->
    case S0() of
        {}     -> ok;
        {X, S} -> F(X), foreach(F, S)                   
    end.
%%-----------------------------------------------------------------------------
%% @doc
%% Iterates over the first N elements of the stream executing the provided
%% function on each one of its elements and returns the remaining part of
%% the stream.
%%
%% This function may come in handy when working with infinite streams. 
%% @end
%%-----------------------------------------------------------------------------
-spec foreach(non_neg_integer(), fun((term()) -> term()), stream()) -> 'ok'.
foreach(0, _F, S) -> S;
foreach(N,  F, S0) ->
    case S0() of
        {}     -> ?STREAM();
        {X, S} -> F(X), foreach(N - 1, F, S)
    end.

%%-----------------------------------------------------------------------------
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec reduce(fun((term(), term()) -> term()), term(), stream()) -> term().
reduce(F, Acc0, S0) ->
    case S0() of
        {}     -> Acc0;
        {X, S} -> reduce(F, F(X, Acc0), S)
    end.

%%-----------------------------------------------------------------------------
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec reduce(non_neg_integer(), fun((term(), term()) -> term()), term(), stream()) -> term().
reduce(0, _F, Acc,  S)  -> {Acc, S};
reduce(N,  F, Acc0, S0) ->
    case S0() of
        {}     -> {Acc0, S0};
        {X, S} -> reduce(N - 1, F, S, F(X, Acc0))
    end.

%%-----------------------------------------------------------------------------
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec map(fun((term()) -> term()), stream()) -> stream().
map(F, S0) ->
    case S0() of
        {} -> 
            S0;
        {X, S} -> 
            reduce(fun(Y, Acc0) -> 
                       append(Acc0, ?STREAM(F(Y))) 
                   end, 
                   ?STREAM(F(X)), 
                   S)
    end.

%%-----------------------------------------------------------------------------
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec map(non_neg_integer(), fun((term()) -> term()), stream()) -> stream().
map(0, _F, S)  -> {?STREAM(), S};
map(N,  F, S0) ->
    case S0() of
        {} -> 
            {S0(), S0()};
        {X, S} -> 
            reduce(N - 1, 
                   fun(Y, Acc0) -> 
                       append(Acc0, ?STREAM(F(Y))) 
                   end,
                   ?STREAM(F(X)), 
                   S)
    end.

-spec filter(fun((term()) -> boolean()), stream()) -> stream().
filter(F, S) ->
    reduce(
      fun(X, Acc) -> 
          case F(X) of
              true  -> append(Acc, ?STREAM(X));
              false -> Acc
          end
      end, ?STREAM(), S).

%%-----------------------------------------------------------------------------
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec filtermap(fun((term()) -> {'true', term()} | 'false'), stream()) -> stream().
filtermap(F, S) ->
    reduce(
      fun(X0, Acc) ->
          case F(X0) of
              {true, X} -> append(Acc, ?STREAM(X));                      
              false     -> Acc
          end
      end, ?STREAM(), S).

%%-----------------------------------------------------------------------------
%% @doc
%% Converts the given list into a stream.
%%
%% This function is computed incrementally.
%% @end
%%-----------------------------------------------------------------------------
-spec from_list(list()) -> stream().
from_list([])     -> ?STREAM();
from_list([H| T]) -> ?STREAM(H, from_list(T)).

%%-----------------------------------------------------------------------------
%% @doc
%% Converts the given stream into a list.
%%
%% This function is computed monolithically.
%% @end
%%-----------------------------------------------------------------------------
-spec to_list(stream()) -> list().
to_list(S0) ->
    case S0() of
        {}     -> [];
        {X, S} -> [X| to_list(S)]
    end.    


%% ======================================================================
%% EUnit
%% ======================================================================

-ifdef(TEST).

creational_test_() ->
    [?_assertEqual(0, length(?STREAM())),
     ?_assertEqual(1, length(?STREAM(1))),
     ?_assertEqual(1, length(?STREAM(1, ?STREAM()))),
     ?_assertEqual(2, length(?STREAM(1, ?STREAM(2, ?STREAM())))),
     ?_assertEqual(5, length(from_list([1,2,3,4,5]))),
     ?_assertMatch([1,2,3], to_list(seq(1, 3))),
     ?_assertMatch([3,2,1], to_list(seq(3, 1))),
     ?_assertMatch([1, 3], to_list(seq(1, 3, 2))),
     ?_assertMatch([1], to_list(seq(1, 3, 3))),
     ?_assertMatch([3, 1], to_list(seq(3, 1, -2))),
     ?_assertMatch([3], to_list(seq(3, 1, -3)))].

emptyness_test_() ->
    [?_assert(is_empty(?STREAM())),
     ?_assertNot(is_empty(?STREAM(1))),
     ?_assertNot(is_empty(?STREAM(1, ?STREAM()))),
     ?_assert(is_empty(from_list([]))),
     ?_assertNot(is_empty(from_list([1]))),
     ?_assert(is_empty(append(?STREAM(), ?STREAM()))),
     ?_assertNot(is_empty(append(?STREAM(), ?STREAM(1)))),
     ?_assertNot(is_empty(append(?STREAM(1), ?STREAM())))].

equality_test_() ->
    [?_assert(equal(?STREAM(), ?STREAM())),
     ?_assertNot(equal(?STREAM(), ?STREAM(1))),
     ?_assertNot(equal(?STREAM(1), ?STREAM())),
     ?_assert(equal(?STREAM(1, ?STREAM(2, ?STREAM(3))), 
                    from_list([1,2,3]))),
     ?_assertNot(equal(?STREAM(3, ?STREAM(2, ?STREAM(1))), 
                       from_list([1,2,3])))].

take_test_() ->
    [?_assertMatch([1,2,3,4], to_list(take(4, from_list([1,2,3,4,5])))),
     ?_assert(is_empty(take(0, from_list([1,2,3,4,5])))),
     ?_assertMatch([1,2,3,4,5], to_list(take(10, from_list([1,2,3,4,5]))))].

drop_test_() ->
    [?_assertMatch([5], to_list(drop(4, from_list([1,2,3,4,5])))),
     ?_assert(is_empty(drop(10, from_list([1,2,3,4,5])))),
     ?_assertMatch([1,2,3,4,5], to_list(drop(0, from_list([1,2,3,4,5]))))].

append_test_() ->
    [?_assertMatch([1,2,3], to_list(append(from_list([1]), from_list([2, 3])))),
     ?_assertMatch([1,2,3], to_list(append(?STREAM(), from_list([1,2,3])))),
     ?_assertMatch([1,2,3], to_list(append(from_list([1,2,3]), ?STREAM())))].

reverse_test() ->
    [?_assert(is_empty(reverse(?STREAM()))),
     ?_assertMatch([1,2,3], to_list(reverse(from_list([3,2,1]))))].

hof_test_() ->
    [?_assert(is_empty(map(fun(X) -> X * X end, ?STREAM()))),
     ?_assertEqual(0, reduce(fun(X, Acc0) -> Acc0 + X end, 
                             0, 
                             ?STREAM())),
     ?_assertMatch([1,4,9], to_list(map(fun(X) -> X * X end, 
                                        from_list([1,2,3])))),
     ?_assertMatch([4], to_list(filtermap(fun(X) -> 
                                              case X rem 2 =:= 0 of
                                                  true ->
                                                      {true, X * X};
                                                  false -> 
                                                      false
                                              end
                                          end,
                                          from_list([1,2,3])))),
     ?_assertMatch([2], to_list(filter(fun(X) -> X rem 2 =:= 0 end,
                                       from_list([1,2,3]))))].

-endif.
