-module(list_op).

-compile(export_all).


% splits a list L into NrParts, 
% note that it will never split a list into lists of less items than 2 unless length(L) is specified as the NrParts
%   e.g. length(L) = 20, NrParts = 19 -> list is split into lists of 2 items
%   length(L) = 20, NrParts = 20 -> list is split into lists of 1 item
% the last list in the resulting lists will carry fewer than the preceding if the list is not evenly dividable
%   e.g. list_op:splitList(lists:seq(1,5),3). -> [[1,2],[3,4],[5]]
splitList(L, NrParts) ->
    ParLen = round(math:ceil(length(L)/NrParts)), %TODO: split better
    splitListParLen(L, ParLen).

splitListParLen([], _ParLen) -> [];
splitListParLen(L, ParLen) ->
    Sub = lists:sublist(L, ParLen),
    [Sub|splitListParLen(L -- Sub, ParLen)].