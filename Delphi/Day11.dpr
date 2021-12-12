program Day11;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Lib in '..\Lib.pas',
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
  TGrid = TIntArray;

function TryFlash(var Grid: TGrid; const Oct: Integer): Boolean;
begin
  // 'Greater than 9', check for 10 exactly. It flashes at 10.
  Result := Grid[Oct] = 10;
  if not Result then
    Exit;

  Grid[Oct] := 11; // Set state to 11 to indicate it flashed already

  var OctX := Oct mod 10;
  var OctY := Oct div 10;
  for var x := OctX - 1 to OctX + 1 do
    for var y := OctY - 1 to OctY + 1 do
      if (x >= 0) and (x <= 9) and (y >= 0) and (y <= 9) then
      begin
        var n := y*10+x;
        if Grid[n] < 10 then // Increase only up to 10 when neighbors flash
          Inc(Grid[n]);
      end;
end;

procedure PrintGrid(var Grid: TGrid);
begin
  Sleep(3);
  for var p := Low(Grid) to High(Grid) do
  begin
    CursorPos(30 + (p mod 10) * 2, p div 10);
    FgColor(clDefault);
    if Grid[p] = 10 then
      FgColor(clRed) // Armed
    else if Grid[p] = 11 then
      FgColor(clGreen) // Just flashed
    else if Grid[p] = 12 then
      FgColor(clWhite); // Reset

    if Grid[p] > 10 then Write('0') else
    if Grid[p] > 9 then Write('9') else
      Write(Grid[p]);
    Write(' ');
    if p mod 10 = 9 then
      WriteLn;
  end;
  WriteLn; // }

  // Modify to remember it was drawn as flashed already
  for var p := Low(Grid) to High(Grid) do
    if Grid[p] = 11 then Grid[p] := 12;
end;

procedure Step(var Grid: TGrid; var Flashes: BigInt);
begin
  // Normal increase of energy level
  for var p := Low(Grid) to High(Grid) do
    Inc(Grid[p]);

  // Let them flash
  var Flashed: Boolean;
  repeat
    Flashed := False;
    // Room for optimization by not checked if, but which
    for var p := Low(Grid) to High(Grid) do
    begin
      if TryFlash(Grid, p) then
      begin
        Flashed := True;
        Inc(Flashes);
      end;
    end;
    PrintGrid(Grid);
  until not Flashed;

  PrintGrid(Grid);

  // Reset energy level of flashed ones
  for var p := Low(Grid) to High(Grid) do
    if Grid[p] > 9 then
    begin
      Assert(Grid[p] in [11, 12], 'Unexpected state ' + Grid[p].ToString + ' for octopus ' + p.ToString);
      Grid[p] := 0;
    end;

  FgColor(clDefault);
end;

function Grid(Inputs: TStringArray): TGrid;
begin
  SetLength(Result, 100);
  Assert(Length(Inputs) = 10, 'Prepped for 10x10 grid alone');
  for var i := Low(Inputs) to High(Inputs) do
    for var c := 1 to Length(Inputs[i]) do
      Result[c-1+i*10] := Ord(Inputs[i][c]) - Ord('0');
end;

function Solve1(const Inputs: TStringArray): BigInt;
begin
  Result := 0;
  var Grid := Grid(Inputs);
  for var i := 1 to 100 do
  begin
    CursorPos(0,0);
    WriteLn('Step: ', i, '    ');
    Step(Grid, Result);
  end;
  CursorPos(0,3);
end;

function Solve2(const Inputs: TStringArray): BigInt;
begin
  Result := 0;
  var Grid := Grid(Inputs);
  var Flashes: BigInt;
  repeat
    CursorPos(0,0);
    Inc(Result);
    WriteLn('Step: ', Result, '    ');
    Flashes := 0;
    Step(Grid, Flashes);
    Sleep(10);
  until Flashes = 100;
  CursorPos(0,4);
end;

var
  Input: TStringArray;
  Result: Int64;
begin

  WriteLn('Tests');
  Sleep(100);
  Input := LoadStrings('Day11.test.txt');
  Result := Solve1(Input);

  ValidateNr(Result, 1656);
  Result := Solve2(Input);
  ValidateNr(Result, 195);

  WriteLn(#10'Final');
  Input := LoadStrings('Day11.input.txt');
  Result := Solve1(Input);
  ValidateNr(Result, 1617);

  var s := TStopwatch.StartNew;
  const Iterations = 1;
  // for var i := 1 to Iterations do
  Result := Solve2(Input);
  //WriteLn(((s.ElapsedTicks * 1000000000) div Iterations) div s.Frequency, ' ns per simulation');
  WriteLn(Iterations, ' iterations in ', s.ElapsedMilliseconds, ' ms');
  ValidateNr(Result, 258);

  WriteLn(#10'Hit it');
  ReadLn;
end.
