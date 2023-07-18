### Interface definitions.

interface IdServer { Init(Main!, Int), Get(Main!) }
interface EchoServer { Echo(Main!, String) }

interface Main { Ready(), Id(Int), Ok(String) }

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

## ID server.
def init(self: Main?, server: IdServer!, start: Int): Main? {
    server ! Init(self, start);
    guard self: Ready {
        receive Ready() from self ->
            self
    }
}

def id_asy(self: Main?, server: IdServer!): Main? {
    server ! Get(self);
    self
}

def id_get(self: Main?): (Main? * Int) {
    guard self: Id {
        receive Id(id) from self ->
            (self, id)
    }
}

## Echo server.
def echo(self: Main?, server: EchoServer!, msg: String): (Main? * String) {
    server ! Echo(self, msg);
    guard self: Ok {
        receive Ok(msg) from self ->
            (self, msg)
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

    # Main (combined) mailbox.
    let main0 = new [Main] in

    # Initialize ID server with offset 10.
    let main1 = init(main0, idServer, 10) in

    # Issue synchronous request to ID server.
    let main2 = id_asy(main1, idServer) in
    let (main3, id) = id_get(main2) in

    # Issue synchronous request to Echo server.
    let (main4, echo) = echo(main3, echoServer, "hello") in

    # Free main mailbox.
    free(main4);

    # Print.
    print(intToString(id));
    print(echo)
}