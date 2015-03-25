-record(hackney_socket, {transport,
                         sock}).

-define(SOCKETS_SERVER, hackney_sockets_server).

-define(CHUNK_SIZE, 65536000). %% 64 MB is the default
