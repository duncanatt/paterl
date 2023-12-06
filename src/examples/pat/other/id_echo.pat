# Two servers that to show that in Pat, mailboxes can be localised but that it
# cannot be done in Erlang. The downside is that the main function making the
# calls to the two different servers must combine their interfaces. Moreover,
# the first call must account for all the possible messages expected in the
# interface, the second call must account for one less message, etc. Depending
# on how many requests are made to different services with different interfaces,
# the interface used by main() and its corresponding regexes will grow.

interface IdServer { Get(IdClient!) }
interface IdClient { Id(Int) }

interface EchoServer { Echo(EchoClient!, String) }
interface EchoClient { Reply(String) }

def id_server(self: IdServer?, next: Int): Unit {
    guard self: *Get {
        free -> ()
        receive Get(client) from self ->
            client ! Id(next);
            id_server(self, next + 1)
    }
}

def echo_server(self: EchoServer?): Unit {
    guard self: *Echo {
        free -> ()
        receive Echo(client, msg) from self ->
            client ! Reply(msg);
            echo_server(self)
    }
}

def id_client(serverMb: IdServer!): Int {
    let clientMb = new [IdClient] in
    serverMb ! Get(clientMb);
    guard clientMb: Id {
        receive Id(id) from clientMb ->
            free(clientMb);
            id
    }
}

def echo_client(serverMb: EchoServer!, msg: String): String {
    let clientMb = new [EchoClient] in
    serverMb ! Echo(clientMb, msg);
    guard clientMb: Reply {
        receive Reply(echoed) from clientMb ->
            free(clientMb);
            echoed
    }
}

def main(): Unit {

    let idServerMb = new [IdServer] in
    spawn {id_server(idServerMb, 0)};

    let echoServerMb = new [EchoServer] in
    spawn {echo_server(echoServerMb)};

    print(intToString(id_client(idServerMb)));
    print(echo_client(echoServerMb, "Hello"))
}

main()