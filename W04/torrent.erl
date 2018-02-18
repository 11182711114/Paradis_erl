-module(torrent).
-behaviour(gen_server).
-compile(export_all).



%%%* Torrent does the following;
%%%     Keeps track of all things surrounding a torrent, e.g. Filename, Size, Parts md5s, Content 
%%%     Announces to the tracker, i.e. gathers peers   
%%%     Asks peers what parts they have    
%%%     Requests parts from peers


%%% State = #{
%%%             filename=>  string()    | Files name
%%%             md5     =>  string()    | Entire files md5
%%%             size    =>  integer()   | The size of the file(Number of parts)
%%%             parts   =>  [string()]  | the md5s of each part of the file
%%%             content =>  [<<X>>]     | The content of a file, list of binaries
%%%             tracker =>  pid()       | The location of the tracker
%%%         }
init(InitialState) ->
    {ok, InitialState}.

-spec announce(State :: map()) -> list().
announce(State) ->
    Tracker = maps:get(tracker, State),
    Md5 = maps:get(md5, State),
    gen_server:call(Tracker, {who_is_interested, Md5}).

handle_call({get_name}, _From, State) ->
    {reply, maps:get(name, State), State};
handle_call({announce}, _From, State) ->
    NewPeers = announce(State),
    {reply, ok, State#{peers => NewPeers}}.

handle_cast(_Query, State) ->
    {noreply, State}.