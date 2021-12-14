program Day14;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Lib in 'Lib.pas',
  Windows,
  SysUtils,
  Vcl.Graphics,
  System.Generics.Defaults,
  System.Generics.Collections,
  System.Diagnostics;

type
  BigInt = Int64;
  BestInt = NativeInt;

type
  TPolyPair = class
    Key: String;
    Insertion: String;
    Count: BigInt;
    NewCount: BigInt;
  end;

function Solve(const Inputs: TStringArray; const Steps: Integer): BigInt;
begin
  var Pairs := TDictionary<String, TPolyPair>.Create;
  var Template := Inputs[0];

  // Build a dictionary of pairs
  for var i := 2 to High(Inputs) do
  begin
    var Pair := TPolyPair.Create;
    Pair.Insertion := Inputs[i][7];
    Pair.Key := Copy(Inputs[i], 1, 2);
    Pairs.Add(Pair.Key, Pair);
  end;

  // Initialize pair count
  for var i := 1 to Length(Template) - 1 do
  begin
    var Pair: TPolyPair;
    Assert(Pairs.TryGetValue(Copy(Template, i, 2), Pair), 'bork');
    Pair.Count := 1;
  end;

  // Do the steps
  for var i := 1 to Steps do
  begin
    // Each insertion results in two new pairs. Increase the count of those pairs.
    for var Pair in Pairs.Values do
    begin
      var Insertion := Pairs[Pair.Key].Insertion;

      Inc(Pairs[Pair.Key[1] + Insertion].NewCount, Pair.Count);
      Inc(Pairs[Insertion + Pair.Key[2]].NewCount, Pair.Count);
    end;

    // To finalize the step, set the current count and reset the temporary count
    for var Pair in Pairs.Values do
    begin
      Pair.Count := Pair.NewCount;
      Pair.NewCount := 0;
    end;
  end;

  // Count each element, by counting just the first characters of each pair.
  var Elements := TDictionary<String, BigInt>.Create;
  for var Pair in Pairs.Values do
  begin
    var Count: BigInt;
    if not Elements.TryGetValue(Pair.Key[1], Count) then
      Count := 0;
    Inc(Count, Pair.Count);
    Elements.AddOrSetValue(Pair.Key[1], Count);
  end;

  // Add the last element, which is not part of a
  var C := Template[Length(Template)];
  Elements[C] := Elements[C] + 1;

  // Find min and max
  var Min := Elements[C];
  var Max := Elements[C];
  for var Count in Elements.Values do
  begin
    if Min > Count then
      Min := Count;
    if Max < Count then
      Max := Count;
  end;

  // Calculate the result
  Result := Max - Min;
end;

function Solve1(const Inputs: TStringArray): BigInt;
begin
  Result := Solve(Inputs, 10);
end;

function Solve2(const Inputs: TStringArray): BigInt;
begin
  Result := Solve(Inputs, 40);
end;

var
  Input: TStringArray;
  Result: Int64;
begin

  WriteLn('Tests');
  Sleep(100);
  Input := LoadStrings('Day14.test.txt');
  Result := Solve1(Input);

  ValidateNr(Result, 1588);
  Result := Solve2(Input);
  ValidateNr(Result, 2188189693529);

  WriteLn(#10'Final');
  Input := LoadStrings('Day14.input.txt');
  Result := Solve1(Input);
  ValidateNr(Result, 2899);

  var s := TStopwatch.StartNew;
  const Iterations = 10;
  for var i := 1 to Iterations do
    Result := Solve2(Input);
  WriteLn(((s.ElapsedTicks * 1000000000) div Iterations) div s.Frequency, ' ns per simulation');
  WriteLn(Iterations, ' iterations in ', s.ElapsedMilliseconds, ' ms');
  ValidateNr(Result, 3528317079545);

  WriteLn(#10'Hit it');
  ReadLn;
end.
