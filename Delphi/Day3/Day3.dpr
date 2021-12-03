program Day3;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Lib in '..\Lib.pas',
  Vcl.Graphics,
  Classes,
  SysUtils;

function Day3_1(Raw: TStringArray): String;
begin
  // Determine how many bit per number in the input values, assuming all the same
  var Bits := Length(Raw[0]);

  // Convert strings to numbers
  var Input: TIntArray;
  SetLength(Input, Length(Raw));
  for var i := Low(Input) to High(Input) do
    for var c := 1 to Bits do
      if Raw[i][c] = '1' then
        Input[i] := Input[i] or (1 shl (Bits - c));

  // For each bit position, count how many times it is 1.
  var Counters: TIntArray;
  SetLength(Counters, Bits);
  for var n in Input do
    for var b := 0 to Bits - 1 do
      if (n and (1 shl b)) > 0 then
        Inc(Counters[b]);

  // If a counter is more than half the total number, that bit is 1 in the gamma rate
  var Half := Length(Input) div 2;
  var Rate := 0;
  for var b := 0 to Bits - 1 do
    if Counters[b] > Half then
      Rate := Rate or (1 shl b);

  // The Epsilon rate is just the inverse of the gamma rate, taking the number of
  // relevant bits (input line length) into account
  var Mask := (1 shl Bits) - 1;
  Result := (Rate * (not Rate and Mask)).ToString;
end;

function Day3_2(Input: TStringArray): String;
begin
end;

var
  Result: String;
begin
  WriteLn('Tests');
  var Input := LoadStrings('Day3.test.txt');
  Result := Day3_1(Input);
  Validate(Result, '198');
  Result := Day3_2(Input);
  Validate(Result, '');

  WriteLn(#10'Final');
  Input := LoadStrings('Day3.input.txt');
  Result := Day3_1(Input);
  Validate(Result, '2967914');
  Result := Day3_2(Input);
  Validate(Result, '');

  WriteLn(#10'Hit it');
  ReadLn;
end.
