-module(mobilePhone2).

-export([allocateFrequency/2,
         client/1,
         delete_Freq/2,
         disconnectedFrequency/2,
         find/2,
         findMobile/2,
         mobile_phone/0,
         start/1,
         tower/1]).

start(FrequencyList) ->
    register(tower,
             spawn(mobilePhone2, tower, [{FrequencyList, []}])).

tower(Frequencies) ->
    %fazer o trap de erros
    process_flag(trap_exit, true),
    receive
        {make_call, From} ->
            {NewFrequencies, Reply} = allocateFrequency(Frequencies,
                                                        From),
            From ! {reply, Reply},
            tower(NewFrequencies);
        {disconnect_call, From} ->
            unlink(From),
            Freq = find(From, Frequencies),
            NewFrequencies = disconnectedFrequency(Frequencies,
                                                   Freq),
            From ! {reply, ok},
            tower(NewFrequencies);
        {'EXIT', _, normal} -> tower(Frequencies);
        {'EXIT', From, _} ->
            Freq = find(From, Frequencies),
            NewFrequencies = disconnectedFrequency(Frequencies,
                                                   Freq),
            tower(NewFrequencies)
    end.

allocateFrequency({[], Allocated}, _) ->
    {{[], Allocated}, {error, no_frequency_available}};
allocateFrequency({[Freq | T], Allocated}, From) ->
    link(From),
    {{T, [{Freq, From} | Allocated]}, {ok, Freq}}.

disconnectedFrequency({Availables, Allocated}, Freq) ->
    {[Freq | Availables], delete_Freq(Allocated, Freq)}.

delete_Freq([], _) -> [];
delete_Freq([Freq | T], Freq) -> T;
delete_Freq([H | T], Freq) ->
    [H | delete_Freq(T, Freq)].

find(From, {_, Allocated}) ->
    findMobile(From, Allocated).

findMobile(From, [{Freq, From} | _]) -> Freq;
findMobile(From, [_ | T]) -> findMobile(From, T).

client(Id) ->
    io:format("Starting client ~w~n", [Id]),
    spawn(fun mobile_phone/0).

mobile_phone() ->
    tower ! {make_call, self()},
    receive
        {reply, {error, no_frequency_available}} ->
            io:format("Cannot make call!~n");
        {reply, {ok, F}} ->
            io:format("Making call with frequency: ~w~n", [F]),
            timer:sleep(10000),
            io:format("Ok, ended call, releasing frequency "
                      "~w~n.",
                      [F]),
            tower ! {disconnect_call, self()},
            receive
                {reply, ok} -> io:format("Client closed...~n")
            end
    end.
