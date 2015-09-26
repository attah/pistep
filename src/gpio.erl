-module(gpio).
-export([init/2, write/2, write_set/2, read/1, release/1]).

init(Pin, Direction) ->
	release(Pin),

  {ok, FdExport} = file:open("/sys/class/gpio/export", [write]),
  file:write(FdExport, integer_to_list(Pin)),
  file:close(FdExport),

  {ok, FdPinDir} = file:open("/sys/class/gpio/gpio" ++ integer_to_list(Pin) ++ "/direction", [write]),
  case Direction of
    in  -> file:write(FdPinDir, "in");
    out -> file:write(FdPinDir, "out")
  end,
  file:close(FdPinDir),

  {ok, FdPinVal} = file:open("/sys/class/gpio/gpio" ++ integer_to_list(Pin) ++ "/value", [read, write]),
  FdPinVal.

write(Fd, Val) ->
	file:position(Fd, 0),
	file:write(Fd, integer_to_list(Val)).

write_set(ListPins, ListValues) ->
	lists:foreach( fun({Pin, Val}) -> gpio:write(Pin, Val) end, lists:zip(ListPins, ListValues) ).

read(Fd) ->
	file:position(Fd, 0),
	{ok, Val} = file:read(Fd, 1),
	Val.

release(Pin) ->
  {ok, FdUnexport} = file:open("/sys/class/gpio/unexport", [write]),
  file:write(FdUnexport, integer_to_list(Pin)),
  file:close(FdUnexport).

