program Day9;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Lib in '..\Lib.pas',
  Windows,
  SysUtils,
  System.Generics.Defaults,
  System.Generics.Collections,
  System.Diagnostics;

type
  BigInt = Int64;
  BestInt = NativeInt;

function Solve1(const Inputs: TStringArray): BigInt;
begin
  Result := 0;

  var Len := Length(Inputs[0]);
  for var y := Low(Inputs) to High(Inputs) do
  begin
    for var x := 1 to Len do
    begin
      var C := Inputs[y][x];
      if
        ((x = 1) or (Inputs[y][x-1] > C)) and
        ((x = Len) or (Inputs[y][x+1] > C)) and
        ((y = Low(Inputs)) or (Inputs[y-1][x] > C)) and
        ((y = High(Inputs)) or (Inputs[y+1][x] > C))
      then
        Inc(Result, Ord(C) - Ord('0') + 1);
    end;
  end;
end;

function Solve2(const Inputs: TStringArray): BigInt;
begin
end;

var
  Input: TStringArray;
  Result: Int64;
begin
  WriteLn('Tests');
  Sleep(100);
  Input := LoadStrings('Day9.test.txt');
  Result := Solve1(Input);
  ValidateNr(Result, 15);
  Result := Solve2(Input);
  ValidateNr(Result, 0);

  WriteLn(#10'Final');
  Input := LoadStrings('Day9.input.txt');
  Result := Solve1(Input);
  ValidateNr(Result, 478);

  var s := TStopwatch.StartNew;
  const Iterations = 0;
  // for var i := 1 to Iterations do
  Result := Solve2(Input);
  //WriteLn(((s.ElapsedTicks * 1000000000) div Iterations) div s.Frequency, ' ns per simulation');
  WriteLn(Iterations, ' iterations in ', s.ElapsedMilliseconds, ' ms');
  ValidateNr(Result, 0);

  WriteLn(#10'Hit it');
  ReadLn;
end.
