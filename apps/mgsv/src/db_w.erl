-module(db_w).

-include("payapp.hrl").

-export([ open_file/2, foldl/3, match/2, lookup/2, insert/2, delete/2, match_delete/2
        , close/1, traverse/2 ]).

open_file(Path, Conf) ->
    dets:open_file(Path, Conf).

close(Db)->
    dets:close(Db).

traverse(Db, Fun) ->
    dets:traverse(Db, Fun).

foldl(Fun, Acc, Db) ->
    dets:foldl(Fun, Acc, Db).

match(Db, Match) ->
    dets:match(Db, Match).

match_delete(Db, Match) ->
    dets:match_delete(Db, Match).

lookup(Db, Id) ->
    dets:lookup(Db, Id).

insert(Db, Thing) ->
    dets:insert(Db, Thing).

delete(Db, Key) ->
    dets:delete(Db, Key).
