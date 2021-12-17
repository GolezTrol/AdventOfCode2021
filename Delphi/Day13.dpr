program Day13;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Lib in 'Lib.pas',
  Windows,
  Math,
  SysUtils,
  Vcl.Graphics,
  System.Generics.Defaults,
  System.Generics.Collections,
  System.Diagnostics;

type
  BigInt = Int64;
  BestInt = NativeInt;

  TFold = TPoint;
  TPuzzle = record
    Dots: TArray<TPoint>;
    Folds: TArray<TFold>;
  end;

function Solve(const Inputs: TStringArray; FoldCount: Integer; Offset: Integer): BigInt;
var Puzzle: TPuzzle;
begin
  with Puzzle do
  begin
    // Read dots
    var Line := 0;

    SetLength(Dots, Length(Inputs));
    for var i := 0 to High(Inputs) do
    begin
      Line := i;
      var l := Inputs[i];
      if l = '' then
        Break;
      var p := Pos(',', l);
      Dots[Line].X := Copy(l, 1, p - 1).ToInteger;
      Dots[Line].Y := Copy(l, p + 1, Length(l)).ToInteger;
    end;
    SetLength(Dots, Line);

    //Read folds
    SetLength(Folds, High(Inputs) - Line);
    for var i := 0 to High(Folds) do
    begin
      var l := Inputs[Line + i + 1];
      var v := Copy(l, 14, Length(l)).ToInteger;
      if l[12] = 'x' then
        Folds[i].X := v
      else
        Folds[i].Y := v;
    end;

    // Fold
    for var f := 0 to Min(FoldCount - 1, High(Folds)) do
    begin

      if Folds[f].X > 0 then
      begin
        for var d := Low(Dots) to High(Dots) do
          if Dots[d].X > Folds[f].X then
            Dots[d].X := Folds[f].X - (Dots[d].X - Folds[f].X);
      end
      else
      begin
        for var d := Low(Dots) to High(Dots) do
          if Dots[d].Y > Folds[f].Y then
            Dots[d].Y := Folds[f].Y - (Dots[d].Y - Folds[f].Y);
      end;
    end;

    var Deduplicator := TDictionary<TPoint, Boolean>.Create;
    for var Dot in Dots do
      Deduplicator.AddOrSetValue(Dot, True);
    Dots := Deduplicator.Keys.ToArray;
    Result := Length(Dots);

    for var Dot in Dots do
    begin
      CursorPos(Dot.X, Dot.Y + Offset);
      Write('#');
    end;
    CursorPos(0, 15);
  end;
end;

function Solve1(const Inputs: TStringArray; Offset: Integer): BigInt;
begin
  Result := Solve(Inputs, 1, Offset);
end;

function Solve2(const Inputs: TStringArray; Offset: Integer): BigInt;
begin
  Result := Solve(Inputs, MaxInt, Offset);
end;

var
  Input: TStringArray;
  Result: Int64;
begin

  WriteLn('Tests');
  Sleep(100);
  Input := LoadStrings('Day13.test.txt');
  Result := Solve1(Input, 0);

  ValidateNr(Result, 17);
  Result := Solve2(Input, 8);
  ValidateNr(Result, 0);

  WriteLn(#10'Final');
  Input := LoadStrings('Day13.input.txt');
  Result := Solve1(Input, 16);
  ValidateNr(Result, 0);

  var s := TStopwatch.StartNew;
  const Iterations = 1;
  for var i := 1 to Iterations do
    Result := Solve2(Input, 24);
  WriteLn(((s.ElapsedTicks * 1000000000) div Iterations) div s.Frequency, ' ns per simulation');
  WriteLn(Iterations, ' iterations in ', s.ElapsedMilliseconds, ' ms');
  ValidateNr(Result, 0);

  WriteLn(#10'Hit it');
  ReadLn;
end.
