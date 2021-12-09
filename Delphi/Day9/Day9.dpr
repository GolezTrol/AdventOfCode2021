program Day9;

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

function FindLows(const Inputs: TStringArray): TArray<TPoint>;
begin
  var Lows := TList<TPoint>.Create;
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
        Lows.Add(Point(X, Y));
        //Inc(Result, Ord(C) - Ord('0') + 1);
    end;
  end;

  Result := Lows.ToArray;
end;

function Solve1(const Inputs: TStringArray): BigInt;
begin
  Result := 0;
  for var L in FindLows(Inputs) do
    Result := Result + 1 + Ord(Inputs[L.Y][L.X]) - Ord('0');
end;

function GetArea(var Inputs: TStringArray; const Low: TPoint): Integer;
var
  Stack: TList<TPoint>;
begin
  Result := 0;
  Stack := TList<TPoint>.Create;
  Stack.Add(Low);
  while Stack.Count > 0 do
  begin
    var Current := Stack.Last;
    Stack.Delete(Stack.Count-1);

    if (Current.X < 1) or (Current.Y < 0) or (Current.X > Length(Inputs[0])) or (Current.Y > High(Inputs)) then
      Continue;

    if CharInSet(Inputs[Current.Y][Current.X], ['X', '9']) then
      Continue;

    Inputs[Current.Y][Current.X] := 'X';

    Inc(Result);

    Stack.Add(Point(Current.X-1, Current.Y));
    Stack.Add(Point(Current.X+1, Current.Y));
    Stack.Add(Point(Current.X, Current.Y-1));
    Stack.Add(Point(Current.X, Current.Y+1));
  end;
end;


function Solve2(const Inputs: TStringArray): BigInt;
begin
  var Lows := FindLows(Inputs);

  var Areas := TList<Integer>.Create;

  var WorkBuffer := Inputs;
  for var Low in Lows do
    Areas.Add(GetArea(WorkBuffer, Low));

  Areas.Sort(
    TDelegatedComparer<Integer>.Create(
      function(const A, B: Integer): Integer
      begin
        Exit(B-A);
      end
    ));

  Result := 1;

  for var i := 0 to 2 do
    Result := Result * Areas[i];
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
  ValidateNr(Result, 1134);

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
  ValidateNr(Result, 1327014);

  WriteLn(#10'Hit it');
  ReadLn;
end.
