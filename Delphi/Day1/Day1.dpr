program Day1;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, Classes;

procedure Validate(Result: String; Expected: String);
begin
  if Result = Expected then
    WriteLn('Result is ', Result, ' as expected')
  else
    WriteLn('Expected ', Expected, ', got ', Result);
end;

function Day1_1(Input: TStringList): String;
begin
  var Count := 0;
  var Prev := Input[0];
  for var Current in Input do
  begin
    if Current.ToInt64 > Prev.ToInt64 then
      Inc(Count);
    Prev := Current;
  end;
  Result := Count.ToString;
end;

var
  Input: TStringList;
  Result: String;
begin
  Input := TStringList.Create;
  Input.LoadFromFile('Day1.1.test.txt');
  Result := Day1_1(Input);
  Validate(Result, '7');
  Input.LoadFromFile('Day1.1.input.txt');
  Result := Day1_1(Input);
  Validate(Result, '1602');

  WriteLn('Hit it');
  ReadLn;
end.
