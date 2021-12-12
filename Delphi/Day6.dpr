program Day6;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Lib in '..\Lib.pas',
  Windows,
  //SysUtils,
  System.Diagnostics;

type
  BigInt = Int64;
  BestInt = NativeInt;

const
  Days7: array[0..8] of BestInt = (7, 8, 0, 1, 2, 3, 4, 5, 6);

function Simulate(const Input: String; Days: BestInt): BigInt;
var
  FishStates: array[0..8] of BigInt;
  Sum: BigInt;
begin
  // Initialize an array of 'timers'/states, with how many fish are in that state
  ZeroMemory(@FishStates[0], (Length(FishStates)) * SizeOf(FishStates[0]));

  var P := Length(Input);
  repeat
    Inc(FishStates[Ord(Input[P]) - Ord('0')]);
    Dec(P, 2)
  until P < 0;

  // Iterate for the number of days.

  var Day0 := 0;
  for var Day := 1 to Days do
  begin
    Inc(FishStates[(Days7[Day0])], FishStates[Day0]);
    Inc(Day0);
    if Day0 = 9 then Day0 := 0;
  end;

  // Sum the fish per state, excluding the ones in the spawn buffer
  Sum := 0;
  for var i := 0 to 8 do
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
  ValidateNr(Result, 5934);
  Result := Simulate(Input, 256);
  ValidateNr(Result, 26984457539);

  WriteLn(#10'Final');
  Input := LoadStrings('Day6.input.txt')[0];
  Result := Simulate(Input, 80);
  ValidateNr(Result, 380758);

  var s := TStopwatch.StartNew;
  const Iterations = 1000000;
  for var i := 1 to Iterations do
    Result := Simulate(Input, 256);
  WriteLn(((s.ElapsedTicks * 1000000000) div Iterations) div s.Frequency, ' ns per simulation');
  WriteLn(Iterations, ' iterations in ', s.ElapsedMilliseconds, ' ms');
  ValidateNr(Result, 1710623015163);

  WriteLn(#10'Hit it');
  ReadLn;
end.
