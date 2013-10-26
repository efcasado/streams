streams
=======

Streams implementation in Erlang

[![Build Status](https://secure.travis-ci.org/efcasado/streams.png?branch=master)](http://travis-ci.org/efcasado/streams)

The following is an example of how streams can be used to define the infinite 
Fibonacci sequence.

```erlang
fibonacci() ->
    fibonacci_helper(0, 1).

fibonacci_helper(M, N) ->
    ?STREAM(M, fibonacci_helper(N, M + N)).
```

