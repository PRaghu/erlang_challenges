-module(gr_cipher_test).
%% API
-export([scramble_test/4]).

-spec scramble_test(list(), integer(), integer(), integer()) -> string().
scramble_test(List, A, B, C) ->
	convert_to_string(length(List), List, {A, B, C}, [], 0).

%% @private
convert_to_string(0, [], {_A, _B, _C}, ACC, _I) ->
	lists:reverse(ACC);
convert_to_string(ListLength, List, {A, B, C}, ACC, I) ->
if
	I rem 3 == 0 ->
		convert_to_string(ListLength -1, tl(List), {A,B,C}, append_to_acc(hd(List) - A, ACC), I + 1);
	I rem 3 == 1 ->
		convert_to_string(ListLength -1, tl(List), {A,B,C}, append_to_acc(hd(List) - B, ACC), I + 1);
	I rem 3 == 2 ->
		convert_to_string(ListLength -1, tl(List), {A,B,C}, append_to_acc(hd(List) - C, ACC), I + 1)
end.

%% @private
append_to_acc(Element, ACC) -> [Element | ACC].