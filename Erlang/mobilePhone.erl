-module(mobilePhone).

-export([checkFrequency/1,
         lockFrequency/3,
         makeCall/0,
         start/1,
         tower/1]).

start(FrequencyList) ->
    register(tower,
             spawn(mobilePhone, tower, [FrequencyList])).

tower(FrequencyList) ->
    %fazer o trap de erros
    process_flag(trap_exit, true),
    receive
        {make_call, From} ->
            case checkFrequency(FrequencyList) of
                not_available ->
                    From ! {error, 'there are no frequencies available'},
                    tower(FrequencyList);
                Freq ->
                    link(From),
                    io:format("The following frequence ~w was attributed "
                              "to the Pid ~w~n",
                              [Freq, From]),
                    From ! {ok, 'frequency locked'},
                    tower(lockFrequency(Freq, FrequencyList, From))
            end
    end.

checkFrequency([]) -> not_available;
checkFrequency([{Freq, available, _} | _]) -> Freq;
checkFrequency([H | T]) -> [H | checkFrequency(T)].

lockFrequency(Freq, [{Freq, _, _} | T], From) ->
    [{Freq, locked, From} | T];
lockFrequency(Freq, [H | T], From) ->
    [H | lockFrequency(Freq, T, From)].

makeCall() ->
    tower ! {make_call, self()},
    receive Msg -> Msg end.
