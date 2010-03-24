-module(mimir_web).
-export([start/0, stop/0]).

-include_lib("xmerl/include/xmerl.hrl").

-define(USER_AGENT, "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10_5_8; en-us) AppleWebKit/532.8+ (KHTML, like Gecko) Version/4.0.4 Safari/531.21.10").
-define(HOST, "localhost").
-define(PORT, 8080).
-define(DOCROOT, "./htdocs").

start() ->
    application:start(inets),
    ok = erltl:compile("templates/index.html"),
    {ok, FeedList} = file:consult("app.cfg"),
    mochiweb_http:start([{name, ?MODULE}, {port, ?PORT},
                         {loop, fun(R) -> loop(R, ?DOCROOT, FeedList) end}]).

stop() ->
    application:stop(inets),
    mochiweb_http:stop(?MODULE).

loop(Request, DocRoot, FeedList) ->
    case Request:get(method) of
        'GET' ->
            case Request:get(path) of
                "/" ->
                    Feeds = feeds(FeedList),
                    Body = index:render(Feeds),
                    Request:ok({"text/html; charset=utf-8", Body});
                "/" ++ Path ->
                    Request:serve_file(Path, DocRoot)
            end;
        _ ->
            Request:respond({501, [], []})
    end.

feeds(Feeds) -> feeds(Feeds, []).

feeds([], Feeds) -> lists:reverse(Feeds);
feeds([H|T], Feeds) ->
    {Name, SiteLink, FeedLink, TTL} = H,
    Items = cache:get(Name, TTL, fun() -> items(FeedLink) end),
    feeds(T, [{feed, {name, Name}, {link, SiteLink}, {items, Items}}|Feeds]).

items(Url) ->
    case fetch(Url) of
        {ok, Body} ->
            {Doc, _Misc} = xmerl_scan:string(xmerl_ucs:to_utf8(Body)),
            Items = xmerl_xpath:string("//item", Doc),
            Fun = fun(I) ->
                Title = extract_text(I, "title"),
                Link = extract_text(I, "link"),
                Description = extract_text(I, "description"),
                {item, {title, Title}, {link, Link}, {description, Description}}
            end,
            lists:map(Fun, Items);
        error ->
            []
    end.

extract_text(Item, ElementName) ->
    Nodes = xmerl_xpath:string("//" ++ ElementName ++ "/text()", Item),
    lists:flatten([X#xmlText.value || X <- Nodes]).

fetch(Url) ->
    case http:request(get, {Url, [{"User-Agent", ?USER_AGENT}]}, [], []) of
        {ok, {_Status, _Headers, Body}} ->
            {ok, Body};
        {error, _} ->
            error
    end.
