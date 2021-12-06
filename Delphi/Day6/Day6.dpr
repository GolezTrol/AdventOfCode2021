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
const
  Days7: array[0..9] of Integer = (7, 8, 9, 0, 1, 2, 3, 4, 5, 6);
  Days9: array[0..9] of Integer = (9, 0, 1, 2, 3, 4, 5, 6, 7, 8);

function Simulate(const Input: String; Days: Integer): Int64;
var
  FishStates: array[0..9] of Int64;
  Sum: Int64;
begin
  // Initialize an array of 'timers'/states, with how many fish are in that state
  ZeroMemory(@FishStates[0], (Length(FishStates) - 1) * SizeOf(FishStates[0]));

  var Len := Length(Input);
  var P := 1;
  repeat
    Inc(FishStates[Ord(Input[P]) - Ord('0')]);
    Inc(P, 2);
  until P > Len;

  // Iterate for the number of days.
  var Day0 := 0;
  for var Day := 1 to Days do
  begin
    FishStates[(Days9[Day0])] := FishStates[Day0]; // Spawn new fishes in a 'spawn buffer'
    Inc(FishStates[(Days7[Day0])], FishStates[Day0]); // Reset timer for fishes in 0, by adding them to 7

    // Instead of shifting the array of states (Day 1 becomes Day 0), increment
    // a day pointer instead. Keep separate pointers for days needed, and keep them
    // in 0 to 9 using an if, which beats mod 10 by _a lot_
    Inc(Day0);
    if Day0 = 10 then Day0 := 0;
  end;

  // Sum the fish per state, excluding the ones in the spawn buffer
  Sum := - FishStates[Days9[Day0]];
  for var i := 0 to 9 do
    Sum := Sum + FishStates[i];
  Result := Sum;
end;

var
  Input: String;
  Result: Int64;
begin
  WriteLn('Tests');
  Sleep(100);
  Input := LoadStrings('Day6.test.txt')[0];
  Result := Simulate(Input, 80);
  Validate(Result.ToString, '5934');
  Result := Simulate(Input, 256);
  Validate(Result.ToString, '26984457539');

  WriteLn(#10'Final');
  Input := LoadStrings('Day6.input.txt')[0];
  Result := Simulate(Input, 80);
  Validate(Result.ToString, '380758');

  var s := TStopwatch.StartNew;
  const Iterations = 1000000;
  for var i := 1 to Iterations do
    Result := Simulate(Input, 256);
  WriteLn(((s.ElapsedTicks * 1000000000) div Iterations) div s.Frequency, ' ns per simulation');
  WriteLn(Iterations, ' iterations in ', s.ElapsedMilliseconds, ' ms');
  Validate(Result.ToString, '1710623015163');

  WriteLn(#10'Hit it');
  ReadLn;
end.
