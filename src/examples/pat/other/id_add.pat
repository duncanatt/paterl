
interface IdServer { Get(IdClient!) }
interface AddServer { Add(AddClient!, Int, Int) }

interface IdClient { Id(Int) }
interface AddClient { Ok(Int) }

def id_server(self: IdServer?, next: Int): Unit {
    guard self: *Get {
        free -> ()
        receive Get(client) from self ->
            client ! Id(next);
            id_server(self, next + 1)
    }
}

def add_server(self: AddServer?): Unit {
    guard self: *Add {
        free -> ()
        receive Add(client, a, b) from self ->
            client ! Ok(a + b);
            add_server(self)
    }
}

#def id(server: IdServer!): Int {
#    let self = new [IdClient] in
#    server ! Get(self);
#    guard self: Id {
#        receive Id(id) from self ->
#            free(self);
#            id
#    }
#}

def add(server: AddServer!, a: Int, b: Int): Int {
    let self = new [AddClient] in
    server ! Add(self, a, b);
    guard self: Ok {
        receive Ok(ans) from self ->
            free(self);
            ans
    }
}

#def add_asy(server: AddServer!, a: Int, b: Int): AddClient? {
#    let self = new [AddClient] in
#    server ! Add(self, a, b);
#    self
#}

#def add_get(self: AddClient?) {
#    guard
#}

# def add_rpc()

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

def id_rpc(server: IdServer!): Int {
    let self = id_asy(server) in
    id_get(self)
}

def main(): Unit {
    let idServerMb = new [IdServer] in
    spawn { id_server(idServerMb, 0) };

    let addServerMb = new [AddServer] in
    spawn { add_server(addServerMb) };

    let idClientMb0 = id_asy(idServerMb) in
    let id0 = id_get(idClientMb0) in

    print(intToString(id_rpc(idServerMb)));
    print(intToString(add(addServerMb, 10, 16)))
}