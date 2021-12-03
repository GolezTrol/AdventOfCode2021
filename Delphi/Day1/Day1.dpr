program Day1;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Lib in '..\Lib.pas',
  Vcl.Graphics,   Classes, SysUtils;

function Day1_1(Input: TIntArray): String;
begin
  var Count := 0;
  var Prev := Input[0];
  for var Current in Input do
  begin
    if Current > Prev then
      Inc(Count);
    Prev := Current;
  end;
  Result := Count.ToString;
end;

function Day1_2(Input: TIntArray): String;
begin
  var Prev := 0;
  var Count := 0;
  for var i := Low(Input) to High(Input) - 2 do
  begin
    var Sum := 0;
    for var j := 0 to 2 do
      Inc(Sum, Input[i+j]);
    if (i > 0) and (Sum > Prev) then
      Inc(Count);
    Prev := Sum;
  end;
  Result := Count.ToString;
end;

var
  Result: String;
begin
  WriteLn('Tests');
  var Input := Load('Day1.1.test.txt');
  Result := Day1_1(Input);
  Validate(Result, '7');
  Result := Day1_2(Input);
  Validate(Result, '5');

  WriteLn(#10'Final');
  Input := Load('Day1.1.input.txt');
  Result := Day1_1(Input);
  Validate(Result, '1602');
  Result := Day1_2(Input);
  Validate(Result, '');

  WriteLn(#10'Hit it');
  ReadLn;
end.
