program Day10;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Lib in '..\Lib.pas',
  Windows,
  System.Types,
  SysUtils,
  Vcl.Graphics,
  System.Generics.Defaults,
  System.Generics.Collections,
  System.Diagnostics;

type
  BigInt = Int64;
  BestInt = NativeInt;

const
  Open = '([{<';
  Close = ')]}>';
  Scores: TArray<Integer> = [0, 3, 57, 1197, 25137];

type
  TState = (Incomplete, Corrupted);

function CheckState(Stack: String; out Score: Integer): TState;
begin
  var p := 0;

  while True do
  begin
    Inc(p);
    if p > Length(Stack) then
      Exit(Incomplete);

    var o := Pos(Stack[p], Open);
    var c := Pos(Stack[p], Close);

    if o > 0 then
    begin
      Score := Scores[o];
      Continue;
    end;

    if c > 0 then
    begin
      Score := Scores[c];
      if p = 1 then
        Exit(Corrupted);

      if Stack[p-1] <> Open[c] then
        Exit(Corrupted);

      Delete(Stack, p-1, 2);
      p := p - 2;
    end;
  end;
end;

function Solve1(const Inputs: TStringArray): BigInt;
begin
  Result := 0;
  for var Line in Inputs do
  begin
    var Score: Integer;
    if CheckState(Line, Score) = Corrupted then
      Inc(Result, Score)
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
  Input := LoadStrings('Day10.test.txt');
  Result := Solve1(Input);
  ValidateNr(Result, 26397);
  Result := Solve2(Input);
  ValidateNr(Result, 0);

  WriteLn(#10'Final');
  Input := LoadStrings('Day10.input.txt');
  Result := Solve1(Input);
  ValidateNr(Result, 321237);

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
