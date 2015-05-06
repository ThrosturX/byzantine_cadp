# byzantine_cadp

## exchange

Example:

```
c(ex).
ex:init().
ex:create(5).
[H|T] = ex:get_list().
H ! exchange.
lists:last(T) ! {exchange, 5}.
```

This will make the first and last process exchange the values 0 and 5.
