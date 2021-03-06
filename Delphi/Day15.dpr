program Day15;

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

  TDirection = (Left, Right, Top, Bottom);
  TCell = record
    From: TDirection;
    Height: Integer;
    Risk: BigInt;
  end;
  PCell = ^TCell;
  TGrid = array of array of TCell;
  TPuzzle = record
    Dimensions: TPoint;
    Grid: TGrid;
  end;

function Load(Inputs: TStringArray): TPuzzle;
begin
  var Grid: TGrid;
  SetLength(Grid, Length(Inputs));
  for var l := Low(Inputs) to High(Inputs) do
  begin
    var Line := Inputs[l];
    SetLength(Grid[l], Length(Line));
    for var i := Low(Grid[l]) to High(Grid[l]) do
    begin
      Grid[l][i].Height := Line.Substring(i, 1).ToInteger;
      Grid[l][i].Risk := MaxInt;
    end;
  end;
  Result.Dimensions.X := Length(Grid[0]);
  Result.Dimensions.Y := Length(Grid);
  Result.Grid := Grid;
end;

procedure Wrap(var Puzzle: TPuzzle; const Multi: Integer);
begin
  if Multi = 1 then Exit;

  var Grid := Puzzle.Grid;
  var g: TGrid;
  var pd := Puzzle.Dimensions;
  // Set the new dimensions
  SetLength(g, pd.Y * Multi);
  for var y := Low(g) to High(g) do
    SetLength(g[y], pd.X * Multi);

  for var y := 0 to pd.Y-1 do
    for var x := 0 to pd.X-1 do
      for var my := 0 to Multi - 1 do
        for var mx := 0 to Multi - 1 do
        begin
          // The height is incremented by the 'manhattan' distance (x + y)
          var Height := Grid[y][x].Height + mx + my;
          // Wrapping. -1, mod 9, + 1?
          while Height > 9 do Dec(Height, 9);

          g[y+pd.Y*my][x+pd.X*mx] := Grid[y][x]; // Copy record
          g[y+pd.Y*my][x+pd.X*mx].Height := Height; // Override height
        end;

  Puzzle.Dimensions := TPoint.Create(pd.X * Multi, pd.Y * Multi);
  Puzzle.Grid := g;
end;

function Solve(const Inputs: TStringArray; const Multi: Integer): BigInt;
var
  Puzzle: TPuzzle;
  Buffer: TList<TPoint>;

  procedure Explore(const Pos: TPoint);
  const
    // Reverse, because we're going backwards
    Offsets: array[TDirection] of TPoint = ((X: +1; Y: 0), (X: -1; Y: 0), (X: 0; Y: +1), (X: 0; Y: -1));
  begin
    // Find the current cell and iterate over all its neighbors
    var Cur := PCell(@Puzzle.Grid[Pos.Y][Pos.X]);
    for var Dir := Low(TDirection) to High(TDirection) do
    begin
      // Find all Neighbors n at neighbor positions np, and check if they need to
      // be (re-)evaluated.
      var Offset := Offsets[Dir];
      var np := Pos.Add(Offset);
      if // in grid boundaries
        (np.X >= 0) and (np.Y >= 0) and
        (np.X < Puzzle.Dimensions.X) and
        (np.Y < Puzzle.Dimensions.Y)
      then
      begin
        var n: PCell := @Puzzle.Grid[np.Y][np.X];
        // If the currently evaluated route is more efficient than any previous route
        // through the neighbor, point the neighbor to the current cell,
        // and add it to the list for further exploration
        if n.Risk > Cur.Risk + n.Height then
        begin
          n.Risk := Cur.Risk + n.Height;
          n.From := Dir;
          Buffer.Add(np);
        end;
      end;
    end;
  end;

  function GetRoute: TArray<TPoint>;
  const
    Offsets: array[TDirection] of TPoint = ((X: -1; Y: 0), (X: +1; Y: 0), (X: 0; Y: -1), (X: 0; Y: +1));
  begin
    SetLength(Result, 0);
    var Target := Puzzle.Dimensions.Add(TPoint.Create(-1,-1));
    var Point := TPoint.Create(0,0);
    repeat
      SetLength(Result, Length(Result)+1); // Not very efficient
      Result[High(Result)] := Point;
      Point := Point.Add(Offsets[Puzzle.Grid[Point.Y][Point.X].From]);
    until Point = Target;
  end;

begin
  Puzzle := Load(Inputs);

  Wrap(Puzzle, Multi);

  var Goal := TPoint.Create(Puzzle.Dimensions.X-1, Puzzle.Dimensions.Y-1);
  Puzzle.Grid[Goal.Y][Goal.X].Risk := Puzzle.Grid[Goal.Y][Goal.X].Height;

  Buffer := TList<TPoint>.Create;

  // Find the least risky direction from each cell, back to front.
  // This is sort of A*, but without evaluating the most efficient places first.
  // Instead, starting from the last cell, for every cell the least risky
  // neighbor is evaluated, and only after that, the route is established.

  // Start by 1 cell, the final destination, to be explored.
  Buffer.Add(Goal);
  repeat
    // Explore each cell in the buffer.
    var Work := Buffer.Count;
    for var i := 0 to Work - 1 do
      Explore(Buffer[i]);
    // Any new cells will have been appended to the list. Clear the part we just did.
    Buffer.DeleteRange(0, Work);
  until Buffer.Count = 0;

  // Turn it into a route, front to back
  var Route := GetRoute;
  // In hindsight, we don't need the entire route. The first neighbor of the
  // starting point will contain the answer we're interested in.
  Result := Puzzle.Grid[Route[1].Y][Route[1].X].Risk;
end;

function Solve1(const Inputs: TStringArray): BigInt;
begin
  Result := Solve(Inputs, 1);
end;

function Solve2(const Inputs: TStringArray): BigInt;
begin
  Result := Solve(Inputs, 5);
end;

var
  Input: TStringArray;
  Result: Int64;
begin

  WriteLn('Tests');
  Sleep(100);
  Input := LoadStrings('Day15.test.txt');
  Result := Solve1(Input);

  ValidateNr(Result, 40);
  Result := Solve2(Input);
  ValidateNr(Result, 315);

  WriteLn(#10'Final');
  Input := LoadStrings('Day15.input.txt');
  Result := Solve1(Input);
  ValidateNr(Result, 609);

  var s := TStopwatch.StartNew;
  const Iterations = 1;
  for var i := 1 to Iterations do
    Result := Solve2(Input);
  WriteLn(((s.ElapsedTicks * 1000000000) div Iterations) div s.Frequency, ' ns per simulation');
  WriteLn(Iterations, ' iterations in ', s.ElapsedMilliseconds, ' ms');
  ValidateNr(Result, 2925);

  WriteLn(#10'Hit it');
  ReadLn;
end.
