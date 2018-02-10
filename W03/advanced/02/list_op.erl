-module(list_op).

-compile(export_all).


% splits a list L into NrParts, 
% The L rem NrParts is added to the first list in the sublists,
%   e.g. L = 10, NrParts = 4 -> 4,2,2,2
split_list_n_parts(L, NrParts) ->
    Length = length(L),
    Rem = Length rem NrParts,
    FirstLen = trunc(((Length-Rem) / NrParts) + Rem),   % Length of the first list, trunc for float -> int conversion
    ParLen = trunc((Length-FirstLen) / (NrParts-1)),    % Length of the rest of the lists
    {FirstList, Rest} = lists:split(FirstLen, L),
    [FirstList | split_list_n_parts_recur(Rest, ParLen)].

split_list_n_parts_recur([], _ParLen) -> [];
split_list_n_parts_recur(L, ParLen) ->
    {Sub, Rest} = lists:split(ParLen, L),
    [Sub | split_list_n_parts_recur(Rest, ParLen)].
