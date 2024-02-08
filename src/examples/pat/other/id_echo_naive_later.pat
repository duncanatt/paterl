interface IdServer { Init(Main!, Int), Get(Main!), Stop(Main!) }
interface EchoServer { Echo(Main!, String) }


interface Main { Ready(), Id(Int), Ok(String), Done() }

def id_server_init(self: IdServer?): Unit {
    guard self: Init.(*Get).Stop {
        receive Init(client, start) from self ->
            client ! Ready();
            id_server(self, start + 1)
    }
}

def id_server(self: IdServer?, next: Int): Unit {
    guard self: (*Get).Stop {
        #free -> ()
        receive Get(client) from self ->
            client ! Id(next);
            id_server(self, next + 1)
        receive Stop(client) from self ->
            client ! Done();
            id_server_exit(self)
    }
}

def id_server_exit(self: IdServer?): Unit {
    guard self: *Get {
        free -> ()
        receive Get(client) from self ->
            id_server_exit(self)
    }
}


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

def id_get(self0: Main?): (Main? * Int) {
    guard self0: Id + 1 {
        free -> (self0, 0) # Issue here!
        receive Id(id) from self ->
            (self, id)
    }
}

def stop(self: Main?, server: IdServer!): Main? {
    server ! Stop(self);
    guard self: Done {
        receive Done() from self ->
            self
    }
}


def echo_server(self: EchoServer?): Unit {
    guard self: *Echo {
        free -> ()
        receive Echo(client, msg) from self ->
            client ! Ok(msg);
            echo_server(self)
    }
}

def echo(self: Main?, server: EchoServer!, msg: String): (Main? * String) {
    server ! Echo(self, msg);
    guard self: Ok . (Id ) { # + 1 Added because of asy. Id too.
        #free ->
        #    (self, msg) # Added because of asy
        receive Ok(msg) from self ->
            (self, msg)
    }
}



def main(): Unit {
    let main = new [Main] in

    let idServer = new [IdServer] in
    spawn { id_server_init(idServer) };

    let echoServer = new [EchoServer] in
    spawn { echo_server(echoServer) };

    let main0 = init(main, idServer, 10) in

    let main1 = id_asy(main0, idServer) in

    let (main2, msg) = echo(main1, echoServer, "hello") in

    let (main3, id) = id_get(main2) in


    let main4 = stop(main3, idServer) in


    print(intToString(id));
    print(msg);




    free(main4)
}