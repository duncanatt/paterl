interface IdServer { Seed(Main!, Int), Get(Main!) }
interface EchoServer { Echo(Main!, String) }


interface Main { Ready(), Id(Int), Ok(String) }

def id_server_init(self: IdServer?): Unit {
    guard self: Seed.*Get {
        receive Seed(client, start) from self ->
            client ! Ready();
            id_server(self, start + 1)
    }
}

def id_server(self: IdServer?, next: Int): Unit {
    guard self: *Get {
        free -> ()
        receive Get(client) from self ->
            client ! Id(next);
            id_server(self, next + 1)
    }
}


def seed(self: Main?, server: IdServer!, start: Int): Main? {
    server ! Seed(self, start);
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
    guard self: Ok {
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

    let main0 = seed(main, idServer, 10) in

    let main1 = id_asy(main0, idServer) in
    let (main2, id) = id_get(main1) in

    let (main3, msg) = echo(main2, echoServer, "hello") in



    print(intToString(id));
    print(msg);



    free(main3)
}