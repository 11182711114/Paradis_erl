-module(list_op).

-compile(export_all).


% Helpers
splitList(L, NrParts) ->
    ParLen = round(math:ceil(length(L)/NrParts)), %TODO: split better
    splitListParLen(L, ParLen).

splitListParLen([], _ParLen) -> [];
splitListParLen(L, ParLen) ->
    Sub = lists:sublist(L, ParLen),
    [Sub|splitListParLen(L -- Sub, ParLen)].