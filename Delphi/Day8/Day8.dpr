program Day8;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Lib in '..\Lib.pas',
  Windows,
  SysUtils,
  System.Diagnostics;

type
  BigInt = Int64;
  BestInt = NativeInt;

function Solve1(const Inputs: TStringArray): BigInt;
begin
  Result := 0;
  for var Input in Inputs do
  begin
    WriteLn('---');
    var Raw := Input.Split([' | ']);
    WriteLn(Raw[1]);
    var Displays := Raw[1].Split([' ']);
    for var Display in Displays do
      if Length(Display) in [2,3,4,7] then
        Inc(Result);
  end;
end;

var
  Input: TStringArray;
  Result: Int64;
begin
  WriteLn('Tests');
  Sleep(100);
  Input := LoadStrings('Day8.test.txt');
  Result := Solve1(Input);
  ValidateNr(Result, 26);
  //Result := Solve1(Input);
  ValidateNr(Result, 0);

  WriteLn(#10'Final');
  Input := LoadStrings('Day8.input.txt');
  Result := Solve1(Input);
  ValidateNr(Result, 548);

  var s := TStopwatch.StartNew;
  const Iterations = 0;
 // for var i := 1 to Iterations do
 //   Result := Solve1(Input);
  //WriteLn(((s.ElapsedTicks * 1000000000) div Iterations) div s.Frequency, ' ns per simulation');
  //WriteLn(Iterations, ' iterations in ', s.ElapsedMilliseconds, ' ms');
  ValidateNr(Result, 1710623015163);

  WriteLn(#10'Hit it');
  ReadLn;
end.
