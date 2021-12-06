program Day6;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Lib in '..\Lib.pas',
  Windows,
  System.Types,
  Vcl.Graphics,
  Classes,
  SysUtils,
  Math,
  System.Diagnostics,
  System.Generics.Collections;


function Simulate(Raw: TStringArray; Days: Integer): String;
var
  FishStates: array[0..9] of Int64;
  Sum: Int64;
begin
  Sum := 0;
  // Initialize an array of 'timers'/states, with how many fish are in that state
  ZeroMemory(@FishStates[0], (Length(FishStates) - 1) * SizeOf(FishStates[0]));

  var Len := Length(Raw[0]);
  var P := 1;
  while P <= Len do
  begin
    Inc(FishStates[Ord(Raw[0][P]) - Ord('0')]);
    Inc(P, 2);
  end;

  // Iterate for the number of days.
  var Day0 := 0;
  var Day7 := 7;
  var Day9 := 9;
  for var Day := 1 to Days do
  begin
    FishStates[(Day9)] := FishStates[Day0]; // Spawn new fishes in a 'spawn buffer'
    Inc(FishStates[(Day7)], FishStates[Day0]); // Reset timer for fished in 0, by adding them to 7

    // Instead of shifting the array of states (Day 1 becomes Day 0), increment
    // a day pointer instead. Keep separate pointers for days needed, and keep them
    // in 0 to 9 using an if, which beats mod 10 by _a lot_
    Inc(Day0); Inc(Day7); Inc(Day9);
    if Day0 = 10 then Day0 := 0;
    if Day7 = 10 then Day7 := 0;
    if Day9 = 10 then Day9 := 0;
  end;

  // Sum the fish per state, excluding the ones in [9], which is the spawn buffer.
  for var i := 0 to 9 do
    if i <> Day9 then
      Sum := Sum + FishStates[i];
  Result := Sum.ToString;
end;

function Day6_1(Raw: TStringArray): String;
begin
  Result := Simulate(Raw, 80);
end;

function Day6_2(Raw: TStringArray): String;
begin
  Result := Simulate(Raw, 256);
end;

var
  Result: String;
begin
  WriteLn('Tests');
  Sleep(100);
  var Input := LoadStrings('Day6.test.txt');
  Result := Day6_1(Input);
  Validate(Result, '5934');
  Result := Day6_2(Input);
  Validate(Result, '26984457539');

  WriteLn(#10'Final');
  Input := LoadStrings('Day6.input.txt');
  Result := Day6_1(Input);
  Validate(Result, '380758');

  var s := TStopwatch.StartNew;
  const Iterations = 1000000;
  for var i := 1 to Iterations do
    Result := Day6_2(Input);
  WriteLn(((s.ElapsedTicks * 1000000000) div Iterations) div s.Frequency, ' ns per simulation');
  WriteLn(Iterations, ' iterations in ', s.ElapsedMilliseconds, ' ms');
  Validate(Result, '1710623015163');

  WriteLn(#10'Hit it');
  ReadLn;
end.
