### Interface definitions.

interface IdServer { Init(IdClient!, Int), Get(IdClient!) }
interface EchoServer { Echo(EchoClient!, String) }

interface IdClient { Ready(), Id(Int) }
interface EchoClient { Ok(String) }

### Server definitions.

## ID sever.
def id_server(self: IdServer?): Unit {
    guard self: Init.*Get {
        receive Init(client, start) from self ->
            client ! Ready();
            id_server_loop(self, start)
    }
}

def id_server_loop(self: IdServer?, next: Int): Unit {
    guard self: *Get {
        free ->
            ()
        receive Get(client) from self ->
            client ! Id(next);
            id_server_loop(self, next + 1)
    }
}

## Echo server.
def echo_server(self: EchoServer?): Unit {
    guard self: *Echo {
        free ->
            ()
        receive Echo(client, msg) from self ->
            client ! Ok(msg);
            echo_server(self)
    }
}

### Client definitions.

## ID server. Local mailbox.
def init(server: IdServer!, start: Int): Unit {
    let self = new [IdClient] in
    server ! Init(self, start);
    guard self: Ready {
        receive Ready() from self ->
            free(self)
    }
}

# Local mailbox. @local @interface IdClient
def id_asy(server: IdServer!): IdClient? {
    let self = new [IdClient] in
    server ! Get(self);
    self
}

def id_get(self: IdClient?): Int {
    guard self: Id {
        receive Id(id) from self ->
            free(self);
            id
    }
}

## Echo server. Local mailbox.
def echo(server: EchoServer!, msg: String): String {
    let self = new [EchoClient] in
    server ! Echo(self, msg);
    guard self: Ok {
        receive Ok(msg) from self ->
            free(self);
            msg
    }
}

### Main.
def main(): Unit {

    # Spawn ID server.
    let idServer = new [IdServer] in
    spawn { id_server(idServer) };

    # Spawn Echo server.
    let echoServer = new [EchoServer] in
    spawn { echo_server(echoServer) };

    # Initialize ID server with offset 10.
    init(idServer, 10);

    # Issue asynchronous request to ID server.
    let idClient = id_asy(idServer) in

    # Issue synchronous request to Echo server.
    let echo = echo(echoServer, "hello") in

    # Fulfil ID request.
    let id = id_get(idClient) in

    # Print.
    print(intToString(id));
    print(echo)
}