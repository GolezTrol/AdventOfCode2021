program Day3;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Lib in '..\Lib.pas',
  Vcl.Graphics,
  Classes,
  SysUtils;

function BinStringsToNumbers(Strings: TStringArray; out Bits: Integer): TIntArray;
begin
  // Determine how many bit per number in the input values, assuming all the same
  Bits := Length(Strings[0]);

  // Convert strings to numbers
  SetLength(Result, Length(Strings));
  for var i := Low(Result) to High(Result) do
  begin
    for var c := 1 to Bits do
      if Strings[i][c] = '1' then
        Result[i] := Result[i] or (1 shl (Bits - c));
  end;
end;

function GetCounters(Input: TIntArray; Bits: Integer): TIntArray;
begin
  // For each bit position, count how many times it is 1.
  SetLength(Result, Bits);
  for var n in Input do
    for var b := 0 to Bits - 1 do
      if (n and (1 shl b)) > 0 then
        Inc(Result[b]);
end;

function Day3_1(Raw: TStringArray): String;
begin
  var Bits: Integer;
  var Input := BinStringsToNumbers(Raw, Bits);

  var Counters := GetCounters(Input, Bits);

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

function Filter(Input: TIntArray; Bits: Integer; Oxygen: Boolean): Integer;
begin
  for var b := Bits - 1 downto 0 do
  begin
    // Count occurrances for this bit
    var Mask := 1 shl b;
    var Count := 0;

    // Count 1s
    for var i := Low(Input) to High(Input) do
      if (Input[i] and Mask) = Mask then
        Inc(Count);

    var Keep: TIntArray;
    var Filter := 0;
    var Half := Length(Input) div 2;

    var Tie := (Count = Half) and (Length(Input) and 1 = 0);

    // For Oxygen we keep the 1s, if they are at least half
    if Oxygen and ((Count > Half) or Tie) then
      Filter := 1
    // For CO2, we keep the 1s if they are less than half, not tied.
    // <=, not <, because Half is rounded down. If it's an actual tie, Tie settles it.
    else if (not Oxygen) and (Count <= Half) and not Tie then
      Filter := 1;

    if Filter = 1 then
      SetLength(Keep, Count)
    else
      SetLength(Keep, Length(Input) - Count);
    Filter := Filter shl b;

    var Index := 0;
    for var i := Low(Input) to High(Input) do
      if (Input[i] and Mask) = Filter then
      begin
        Keep[Index] := Input[i];
        Inc(Index);
      end;

    Assert(Index = Length(Keep), 'Length of Keep');

    Input := Keep;

    if Length(Input) = 1 then
      Break;
  end;

  Assert(Length(Input) = 1, 'Length of Input');

  Result := Input[0];
end;

function Day3_2(Raw: TStringArray): String;
begin
  var Bits: Integer;
  var Input := BinStringsToNumbers(Raw, Bits);

  var Oxygen := Filter(Input, Bits, True);
  var CO2 := Filter(Input, Bits, False);

  WriteLn(Oxygen);
  WriteLn(CO2);

  Result := (Oxygen * CO2).ToString;
end;

var
  Result: String;
begin
  WriteLn('Tests');
  var Input := LoadStrings('Day3.test.txt');
  Result := Day3_1(Input);
  Validate(Result, '198');
  Result := Day3_2(Input);
  Validate(Result, '230');

  WriteLn(#10'Final');
  Input := LoadStrings('Day3.input.txt');
  Result := Day3_1(Input);
  Validate(Result, '2967914');
  Result := Day3_2(Input);
  Validate(Result, '7041258');

  WriteLn(#10'Hit it');
  ReadLn;
end.
