-module(datetime_shift).

-export([
  shift_days/1,
  shift_months/1,
  shift_hours/1,
  shift_mins/1
]).

-define(SECONDS_IN_A_DAY, 86400).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Moves the received datetime number of days to the future or to the past
-spec shift_days(integer()) -> calendar:datetime().
shift_days(NumberOfDays) ->
  Datetime = calendar:universal_time(),
  Shift = ?SECONDS_IN_A_DAY * NumberOfDays,
  Secs = calendar:datetime_to_gregorian_seconds(Datetime),
  calendar:gregorian_seconds_to_datetime(Secs + Shift).

%% @doc Moves the received date number of months to the future or to the past
-spec shift_months(integer()) -> calendar:datetime().
shift_months(NumberOfMonths) ->
  {{Y, M, D}, Time} = calendar:universal_time(),

  %% in order for the modular arithmetic to work, months in this function range
  %% from 0 to 11 (January to December)
  TotalMonths = 12*Y + M-1 + NumberOfMonths,

  case TotalMonths >= 0 of
    true ->
      Month = TotalMonths rem 12,
      Year = (TotalMonths - Month) div 12,

      %% add one back to the month to fix our tricky mod 12
      {find_valid_date({Year, Month+1, D}), Time};
    false ->
      error(out_of_bounds)
  end.

%% @doc Moves the received date number of hours to the future or to the past.
-spec shift_hours(integer()) -> calender:datetime().
shift_hours(NumberOfHours) ->
  calendar:datetime_to_gregorian_seconds(calendar:universal_time()) + NumberOfHours * 60  * 60.

%% @doc Moves the received date number of mins to the future or to the past.
-spec shift_mins(integer()) -> calender:datetime().
shift_mins(NumberOfMins) ->
  calendar:datetime_to_gregorian_seconds(calendar:universal_time()) + NumberOfMins * 60.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Returns `Date' if valid. Otherwise, returns `Date' replacing `Day` with the last day of the month.
find_valid_date(Date) ->
  case calendar:valid_date(Date) of
    true ->
      Date;
    false ->
      {Y, M, _} = Date,
      {Y, M, calendar:last_day_of_the_month(Y, M)}
  end.