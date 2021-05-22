-module(aula1).

-export([add/3,
         client1/2,
         consult/1,
         count/1,
         create_server1/0,
         delete/2,
         deposit/2,
         factorial/1,
         fib/1,
         lookup/2,
         loop1/0,
         member/2,
         open_account/2,
         reverse/1,
         server/1,
         square/1,
         start_bank/0,
         temp_convert/1,
         withdraw/2]).

%Basic problems
%1a
square(X) -> X * X.

%1b
temp_convert(F) -> 5 * (F - 32) / 9.

%2a
factorial(0) -> 1;
factorial(N) -> N * factorial(N - 1).

%2b
fib(X) when X < 2 -> 1;
fib(X) -> fib(X - 1) + fib(X - 2).

%3a
count([]) -> 0;
count([_ | L]) -> 1 + count(L).

%3b
member([], _) -> false;
member([H | _], H) -> true;
member([_ | T], X) -> member(T, X).

%3c
delete([], _) -> [];
delete([H | T], H) -> T;
delete([H | T], X) -> [H | delete(T, X)].

%3d
reverse([]) -> [];
reverse([H | T]) -> reverse(T) ++ [H].

%Process

loop1() ->
    receive
        {From, Msg} ->
            R = "The message is and the pid " ++ Msg,
            From ! {self(), R},
            loop1()
    end.

client1(Pid, Msg) ->
    Pid ! {self(), Msg},
    receive {Pid, R} -> R end.

create_server1() -> spawn(fun loop1/0).

%3
server(Accounts) ->
    receive
        {new_acc, ClientName, InitialAmount, From} ->
            case lookup(ClientName, Accounts) of
                not_existent ->
                    From ! {ok, 'Account created.'},
                    server([{ClientName, InitialAmount}, Accounts]);
                _ ->
                    From ! {error, 'Account already exists.'},
                    server(Accounts)
            end;
        {consult, ClientName, From} ->
            case lookup(ClientName, Accounts) of
                not_existent ->
                    From ! {error, 'Client does not exist.'},
                    server(Accounts);
                Balance ->
                    From ! {saldo, Balance},
                    server(Accounts)
            end;
        {deposit, ClientName, Amount, From} ->
            case lookup(ClientName, Accounts) of
                not_existent ->
                    From ! {error, 'Client does not exist.'},
                    server(Accounts);
                _ ->
                    From ! {ok, 'saldo depositado'},
                    server(add(ClientName, Amount, Accounts))
            end;
        {withdraw, ClientName, Amount, From} ->
            case lookup(ClientName, Accounts) of
                not_existent ->
                    From ! {error, 'Client does not exist.'};
                Balance ->
                    if Amount > Balance ->
                           From ! {error, 'You dont have enough cash money'};
                       true ->
                           From ! {ok, 'saldo retirado!'},
                           server(add(ClientName, -Amount, Accounts))
                    end
            end
    end.

add(Name, Amount, [{Name, Balance} | T]) ->
    [{Name, Balance + Amount} | T];
add(Name, Amount, [H | T]) ->
    [H | add(Name, Amount, T)].

open_account(ClientName, InitialAmount) ->
    bank ! {new_acc, ClientName, InitialAmount, self()},
    receive Msg -> Msg end.

consult(ClientName) ->
    bank ! {consult, ClientName, self()},
    receive Msg -> Msg end.

deposit(ClientName, Amount) ->
    bank ! {deposit, ClientName, Amount, self()},
    receive Msg -> Msg end.

withdraw(ClientName, Amount) ->
    bank ! {withdraw, ClientName, Amount, self()},
    receive Msg -> Msg end.

start_bank() ->
    register(bank, spawn(aula1, server, [[]])).

lookup(_, []) -> not_existent;
lookup(ClientName, [{ClientName, Balance} | _]) ->
    Balance;
lookup(ClientName, [_, T]) -> lookup(ClientName, T).
