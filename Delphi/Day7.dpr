program Day7;

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

const
  Example: string = '16,1,2,0,4,2,7,1,2,14';

function Solve(const Input: String; Incremental: Boolean): BigInt;
begin
  var Items := Input.Split([',']);
  var Positions: TArray<Integer>;
  SetLength(Positions, Length(Items));



  var Min := MAXINT;
  var Max := -1;
  for var i := Low(Items) to High(Items) do
  begin
    Positions[i] := Items[i].ToInteger();
    if Positions[i] > Max then
      Max := Positions[i];
    if Positions[i] < Min then
      Min := Positions[i];
  end;

  // Precalculate fuel cost per distance.
  var Cost: TArray<Int64>;
  if Incremental then
  begin
    SetLength(Cost, Max-Min+1);
    for var c := Low(Cost) + 1 to High(Cost) do
      Cost[c] := Cost[c-1]+c;
  end;

  var BestPos := 0;
  Result := Int64(MaxInt)*MaxInt;
  for var i := Min to Max do
  begin
    var Fuel: BigInt := 0;
    for var p := Low(Positions) to High(Positions) do
      if Incremental then
        Inc(Fuel, Cost[Abs(Positions[p]-i)])
      else
        Inc(Fuel, Abs(Positions[p]-i));

    if Fuel < Result then
    begin
      Result := Fuel;
      BestPos := i;
    end;
  end;

  writeLn('Best pos: ', BestPos);
end;

var
  Input: String;
  Result: Int64;
begin
  WriteLn('Tests');
  Sleep(100);
  Input := Example;
  Result := Solve(Example, False);
  ValidateNr(Result, 37);
  Result := Solve(Example, True);
  ValidateNr(Result, 168);

  WriteLn(#10'Final');
  Input := LoadStrings('Day7.input.txt')[0];
  Result := Solve(Input, False);
  ValidateNr(Result, 352331);
  var s := TStopwatch.StartNew;
  const Iterations = 1;
  for var i := 1 to Iterations do
    Result := Solve(Input, True);
  WriteLn(((s.ElapsedTicks * 1000000000) div Iterations) div s.Frequency, ' ns per simulation');
  WriteLn(Iterations, ' iterations in ', s.ElapsedMilliseconds, ' ms');
  ValidateNr(Result, 99266250);

  WriteLn(#10'Hit it');
  ReadLn;
end.
