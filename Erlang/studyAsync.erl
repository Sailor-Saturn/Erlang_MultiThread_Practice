-module(studyAsync).

-export([changeStatusOfResource/4,
         communication/2,
         create_processes/0,
         freeResource/1,
         last_process/0,
         requestResource/1,
         seeResourceAvailable/2,
         sendMsg/1,
         server/1,
         start/1,
         getAllResourcesFromPid/3]).

create_processes() ->
    P4 = spawn(studyAsync, last_process, []),
    P3 = spawn(studyAsync, communication, [P4, 3]),
    P2 = spawn(studyAsync, communication, [P3, 2]),
    P1 = spawn(studyAsync, communication, [P2, 1]),

    register(first, P1).

last_process() ->
    receive
        stop ->
            io:format("The process 4 will stop~n"),
            exit(finish);
        Msg ->
            io:format("The message of process 4 is ~w~n", [Msg]),
            last_process()
    end.

communication(NextProcess, N) ->
    link(NextProcess),
    receive
        Msg ->
            io:format("The message of this process ~w is ~w~n",
                      [N, Msg]),
            NextProcess ! Msg,
            communication(NextProcess, N)
    end.

sendMsg(Msg) ->
    first ! Msg,
    ok.

%2

start(ResourceList) ->
    register(server,
             spawn(studyAsync, server, [ResourceList])).

server(Resources) ->
    process_flag(trap_exit,true),
    receive
        {request_resource, From, Resource} ->
            case seeResourceAvailable(Resource, Resources) of
                not_available ->
                    From ! {error, 'this resource is not available'},
                    server(Resources);
                does_not_exist ->
                    From ! {error, 'this resource does not exist'},
                    server(Resources);
                _ ->
                    link(From),
                    From ! {ok, 'this resource is allocated'},
                    server(changeStatusOfResource(Resource,
                                                  not_available,
                                                  From,
                                                  Resources))
            end;
        {free_resource, From, Resource} ->
            case seeResourceAvailable(Resource, Resources) of
                not_available ->
                    unlink(From),
                    From ! {ok, 'this resource is free now'},
                    server(changeStatusOfResource(Resource,
                                                  available,
                                                  '',
                                                  Resources));
                does_not_exist ->
                    From ! {error, 'this resource does not exist'},
                    server(Resources);
                _ ->
                    From ! {error, 'this resource was already available'},
                    server(Resources)
            end;
        {peek, From, Resource} ->
            case seeResourceAvailable(Resource, Resources) of
                not_available ->
                    From ! {ok, 'this resource is being used'},
                    server(Resources);
                does_not_exist ->
                    From ! {error, 'this resource does not exist'},
                    server(Resources);
                _ ->
                    From ! {error, 'this resource is available'},
                    server(Resources)
            end;
        {'EXIT', From, Why} ->
            io:format("Processo ~w terminou com o erro: ~w~n",[From,Why]),
            case getAllResourcesFromPid(Resources,From,[]) of
                [] ->
                    io:format("Este Processo ~w não tinha recurso alocado~n",[From]),
                    server(Resources);
                _ -> 
                    io:format("Este Processo ~w não tinha recurso alocado~n",[From]),
                    server(Resources)
            end
    end.

changeStatusOfResource(Resource, Value, Pid, [{Resource, _, _} | T]) ->
    [{Resource, Value,Pid} | T];
changeStatusOfResource(Resource, Value,Pid, [H | T]) ->
    [H | changeStatusOfResource(Resource, Value,Pid, T)].

seeResourceAvailable(_, []) -> does_not_exist;
seeResourceAvailable(Resource,
                     [{Resource, available,_} | _]) ->
    available;
seeResourceAvailable(Resource,
                     [{Resource, not_available,_} | _]) ->
    not_available;
seeResourceAvailable(Resource, [_ | T]) ->
    seeResourceAvailable(Resource, T).

requestResource(Resource) ->
    server ! {request_resource, self(), Resource},
    receive Msg -> Msg end.

freeResource(Resource) ->
    server ! {free_resource, self(), Resource},
    receive Msg -> Msg end.

getAllResourcesFromPid([],_,List) -> List;
getAllResourcesFromPid([{Resource,_,Pid}|T],Pid, List) -> [{Resource,available,''}|getAllResourcesFromPid(T,Pid,[Resource|List])];
getAllResourcesFromPid([H|T],Pid,List) ->  [H|getAllResourcesFromPid(T,Pid,List)].
