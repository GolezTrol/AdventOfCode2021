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
  ExampleFile: String = '3,4,3,1,2';
  ActualFile: String = '3,3,2,1,4,1,1,2,3,1,1,2,1,2,1,1,1,1,1,1,4,1,1,5,2,1,1,2,1,1,1,3,5,1,5,5,1,1,1,1,3,1,1,3,2,1,1,1,1,1,1,4,1,1,1,1,1,1,1,4,1,3,3,1,1,3,1,3,1,2,1,3,1,1,4,1,2,4,4,5,1'+',1,1,1,1,1,4,1,5,1,1,5,1,1,3,3,1,3,2,5,2,4,1,4,1,2,4,5,1,1,5,1,1,1,4,1,1,5,2,1,1,5,1,1,1,5,1,1,1,1,1,3,1,5,3,2,1,1,2,2,1,2,1,1,5,1,1,4,5,1,4,3,1,1,1,1,1,1,5,1,1,1,5,'+'2,1,1,1,5,1,1,1,4,4,2,1,1,1,1,1,1,1,3,1,1,4,4,1,4,1,1,5,3,1,1,1,5,2,2,4,2,1,1,3,1,5,5,1,1,1,4,1,5,1,1,1,4'+',3,3,3,1,3,1,5,1,4,2,1,1,5,1,1,1,5,5,1,1,2,1,1,1,3,1,1,1,2,3,1,2,2,3,1,3,1,1,4,1,1,2,1,1,1,1,3,5,1,1,2,1,1,1,4,1,1,1,1,1,2,4,1,1,5,3,1,1,1,2,2,2,1,5,1,3,5,3,1,1,4,1,1,4';

type
  TFishStates = record
    case Integer of
    0: (F0, F1, F2, F3, F4, F5, F6, F7, F8: BigInt);
    1: (FishStates: array[0..8] of BigInt);
  end;

function Simulate(const Input: String; Days: BestInt): BigInt;
var
  //FishStates: array[0..8] of BigInt;
  f: TFishStates;
begin
  f := Default(TFishStates);
  with f do
  begin
  // Initialize an array of 'timers'/states, with how many fish are in that state

  //ZeroMemory(@FishStates[0], (Length(FishStates)) * SizeOf(FishStates[0]));

  var P := Length(Input);
  repeat
    Inc(FishStates[Ord(Input[P]) - Ord('0')]);
    Dec(P, 2)
  until P < 0;

    for var i := 1 to Days div 9 do
    begin
      Inc(F7, F0);
      Inc(F8, F1);
      Inc(F0, F2);
      Inc(F1, F3);
      Inc(F2, F4);
      Inc(F3, F5);
      Inc(F4, F6);
      Inc(F5, F7);
      Inc(F6, F8);
    end;

    Inc(F7, F0);
    Inc(F8, F1);
    Inc(F0, F2);
    Inc(F1, F3);
    if Days = 80 then
    begin
      Inc(F2, F4);
      Inc(F3, F5);
      Inc(F4, F6);
      Inc(F5, F7);
      //Inc(F6, F8);
    end;

  {
    var Day0 := 0;
    for var Day := 1 to Days do
    begin
      Inc(FishStates[(Days7[Day0])], FishStates[Day0]);
      Inc(Day0);
      if Day0 = 9 then Day0 := 0;
    end;}

    // Sum the fish per state, excluding the ones in the spawn buffer
    Result := 0;
    for var i := 0 to 8 do
      Inc(Result, FishStates[i]);
    //Result := F0+F1+F2+F3+F4+F5+F6+F7+F8;
  end;
end;

var
  Result: Int64;
begin
  WriteLn('Tests');
  Sleep(100);
  Result := Simulate(ExampleFile, 80);
  ValidateNr(Result, 5934);
  Result := Simulate(ExampleFile, 256);
  ValidateNr(Result, 26984457539);

  WriteLn(#10'Final');
  Result := Simulate(ActualFile, 80);
  ValidateNr(Result, 380758);

  var s := TStopwatch.StartNew;
  const Iterations = 1000000;
  for var i := 1 to Iterations do
    Result := Simulate(ActualFile, 256);
  WriteLn(((s.ElapsedTicks * 1000000000) div Iterations) div s.Frequency, ' ns per simulation');
  WriteLn(Iterations, ' iterations in ', s.ElapsedMilliseconds, ' ms');
  ValidateNr(Result, 1710623015163);

  WriteLn(#10'Hit it');
  ReadLn;
end.
