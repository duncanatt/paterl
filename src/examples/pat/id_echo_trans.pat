# Two servers that to show that in Pat, mailboxes can be localised but that it
# cannot be done in Erlang. The downside is that the main function making the
# calls to the two different servers must combine their interfaces. Moreover,
# the first call must account for all the possible messages expected in the
# interface, the second call must account for one less message, etc. Depending
# on how many requests are made to different services with different interfaces,
# the interface used by main() and its corresponding regexes will grow.

interface IdServer { Get(Main!) }

interface EchoServer { Echo(Main!, String) }

interface Main { Id(Int), Reply(String)} # Combines IdClient and EchoClient
#interface Main { Id(Int) } # Combines IdClient and EchoClient

# -spec id_server(integer()) -> no_return().
# id_server(Next) ->
#   ?mb_state_free("get*"),
#   receive
#     {get, Client} ->
#       Client ! {id, Next},
#       id_server(Next + 1)
#   end.
def id_server(self: IdServer?, next: Int): Unit {
    guard self: *Get {
        free -> ()
        receive Get(client) from self ->
            client ! Id(next);
            id_server(self, next + 1)
    }
}

# -spec echo_server() -> no_return().
# echo_server() ->
#   ?mb_state_free("echo*"),
#   receive
#     {echo, Client, Msg} ->
#       Client ! {reply, Msg},
#       echo_server()
#   end.
def echo_server(self: EchoServer?): Unit {
    guard self: *Echo {
        free -> ()
        receive Echo(client, msg) from self ->
            client ! Reply(msg);
            echo_server(self)
    }
}

# -spec id_client(id_server()) -> integer().
# id_client(Server) ->
#   Server ! {get, self()},
#   ?mb_state("id"),
#   receive
#     {id, Id} ->
#       Id
#   end.
def id_client(mainMb: Main?, serverMb: IdServer!): (Main? * Int) {
    serverMb ! Get(mainMb);
    #guard mainMb: (Id . Reply) { # Would have thought this would be the correct pattern. It is incorrect however because the calls are synchronous, so the main mailbox cannot contain reply until the request is made. The correct regex should consist of just Id.
    guard mainMb: Id {
        receive Id(id) from mainMb ->
            (mainMb, id)
    }
}

# -spec echo_client(echo_server(), string()) -> string().
# echo_client(Server, Msg) ->
#   Server ! {echo, self(), Msg},
#   ?mb_state("reply"),
#   receive
#     {reply, Echoed} ->
#       Echoed
#   end.
def echo_client(mainMb: Main?, serverMb: EchoServer!, msg: String): (Main? * String) {
    serverMb ! Echo(mainMb, msg);
    guard mainMb: Reply {
        receive Reply(echoed) from mainMb ->
            (mainMb, echoed)
    }
}



# main() ->
#   IdServer = spawn(?MODULE, id_server, [0]),
#   EchoServer = spawn(?MODULE, echo_server, []),
#   format("~p~n", [id_client(IdServer)]),
#   format("~s~n", [echo_client(EchoServer, "Hello")]).
def main(): Unit {

    let mainMb = new [Main] in

    let idServerMb = new [IdServer] in
    spawn {id_server(idServerMb, 0)};

    let echoServerMb = new [EchoServer] in
    spawn {echo_server(echoServerMb)};

    let (mainMb0, id) = id_client(mainMb, idServerMb) in
    print(intToString(id));

    let (mainMb1, echoed) = echo_client(mainMb0, echoServerMb, "Hello") in
    print(echoed);
    free(mainMb1)
}

main()