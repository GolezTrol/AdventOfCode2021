program Day5;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Lib in '..\Lib.pas',
  Windows, System.Types,
  Vcl.Graphics,
  Classes,
  SysUtils,
  System.Generics.Collections;


type
  TCell = class
    X, Y: Integer;
    Count: Integer;
  end;
  TGrid = record
    Cells: TObjectDictionary<TPoint, TCell>;
    function GetCell(Point: TPoint): TCell;
    procedure IncCellCount(Point: TPoint);
  end;

procedure Plot(const Grid: TGrid);
begin
  var Cell: TCell;
  for var Y := 0 to 10 do
  begin
    for var X := 0 to 10 do
      if Grid.Cells.TryGetValue(Point(X, Y), Cell) then
        Write(Cell.Count.ToString)
      else
        Write('.');
    WriteLn;
  end;
end;

function Day5_1(Raw: TStringArray): String;
var
  X1, Y1, X2, Y2: Integer;
  Grid: TGrid;
begin
  Grid.Cells := TObjectDictionary<TPoint, TCell>.Create;

  var Parser := TStringList.Create;
  for var Line in Raw do
  begin
    Parser.CommaText := Line.Replace(' -> ', ',');
    WriteLn(Line);
    X1 := Parser[0].ToInteger;
    Y1 := Parser[1].ToInteger;
    X2 := Parser[2].ToInteger;
    Y2 := Parser[3].ToInteger;
    if X1 > X2 then begin var X := X1; X1 := X2; X2 := X; end;
    if Y1 > Y2 then begin var Y := Y1; Y1 := Y2; Y2 := Y; end;


    if (X1 = X2) or (Y1 = Y2) then
      for var X := X1 to X2 do
        for var Y := Y1 to Y2 do
          Grid.IncCellCount(Point(X, Y));
  end;

  Plot(Grid);

  var Count := 0;

  for var Cell in Grid.Cells.Values do
    if Cell.Count >= 2 then
      Inc(Count);

  Result := Count.ToString;
end;

function Day5_2(Raw: TStringArray): String;
begin
end;

{ TGrid }

function TGrid.GetCell(Point: TPoint): TCell;
begin
  if not Cells.TryGetValue(Point, Result) then
  begin
    Result := TCell.Create;
    Result.X := Point.X;
    Result.Y := Point.Y;
    Cells.Add(Point, Result);
  end;
end;

procedure TGrid.IncCellCount(Point: TPoint);
begin
  Inc(GetCell(Point).Count);
end;

var
  Result: String;
begin
  WriteLn('Tests');
  var Input := LoadStrings('Day5.test.txt');
  Result := Day5_1(Input);
  Validate(Result, '5');
  Result := Day5_2(Input);
  Validate(Result, '');

  WriteLn(#10'Final');
  Input := LoadStrings('Day5.input.txt');
  Result := Day5_1(Input);
  Validate(Result, '5632');
  Result := Day5_2(Input);
  Validate(Result, '');

  WriteLn(#10'Hit it');
  ReadLn;
end.
