program Greeter;

var
    Count: Integer;

procedure GreetTimes(N: Integer);
var
    I: Integer;
begin
    for I := 1 to N do
        WriteLn('Hello, World!');
end;

begin
    Count := 5;
    GreetTimes(Count);
end.

