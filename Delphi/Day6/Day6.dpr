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
  ZeroMemory(@FishStates[0], Length(FishStates) * SizeOf(FishStates[0]));

  var States := Raw[0].Split([',']);
  for var State in States do
    Inc(FishStates[State.ToInteger]);

  // Iterate for the number of days.
  for var Day := 1 to Days do
  begin
    FishStates[9] := FishStates[0]; // Spawn new fishes in a 'spawn buffer'
    Inc(FishStates[7], FishStates[0]); // Reset timer for fished in 0, by adding them to 7
    // Move the whole array. A 'day 0' pointer would be more efficient, but with
    // just 256 iterations, this is fine.
    CopyMemory(@FishStates[0], @FishStates[1], SizeOf(FishStates[0]) * 9);
  end;

  // Sum the fish per state, excluding the ones in [9], which is the spawn buffer.
  for var i := 0 to 8 do
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
  for var i := 1 to 1000 do
    Result := Day6_2(Input);
  WriteLn((s.ElapsedTicks * 1000) div s.Frequency);
  WriteLn(s.ElapsedMilliseconds);
  Validate(Result, '1710623015163');

  WriteLn(#10'Hit it');
  ReadLn;
end.