program Day5;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Lib in '..\Lib.pas',
  Windows, System.Types,
  Vcl.Graphics,
  Classes,
  SysUtils,
  Math,
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
        Write(Format('%2d', [Cell.Count]))
      else
        Write(' .');
    WriteLn;
  end;
end;

function Solve(Raw: TStringArray; IncludeDiagonal: Boolean): String;
var
  X1, Y1, X2, Y2: Integer;
  Grid: TGrid;
begin
  Grid.Cells := TObjectDictionary<TPoint, TCell>.Create;

  var Parser := TStringList.Create;
  for var Line in Raw do
  begin
    Parser.CommaText := Line.Replace(' -> ', ',');
    X1 := Parser[0].ToInteger;
    Y1 := Parser[1].ToInteger;
    X2 := Parser[2].ToInteger;
    Y2 := Parser[3].ToInteger;

    if (X1 = X2) or (Y1 = Y2) or IncludeDiagonal then
    begin
      var Dist := Max(Abs(X2-X1), Abs(Y2-Y1));

      var XDelta := Sign(X2-X1);
      var YDelta := Sign(Y2-Y1);

      for var l := 0 to Dist do
        Grid.IncCellCount(Point(X1+XDelta*l, Y1+YDelta*l));
    end;
  end;

  Plot(Grid);

  var Count := 0;

  for var Cell in Grid.Cells.Values do
    if Cell.Count >= 2 then
      Inc(Count);

  Result := Count.ToString;
end;

function Day5_1(Raw: TStringArray): String;
begin
  Result := Solve(Raw, False);
end;

function Day5_2(Raw: TStringArray): String;
begin
  Result := Solve(Raw, True);
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
  Validate(Result, '12');

  WriteLn(#10'Final');
  Input := LoadStrings('Day5.input.txt');
  Result := Day5_1(Input);
  Validate(Result, '5632');
  Result := Day5_2(Input);
  Validate(Result, '22213');

  WriteLn(#10'Hit it');
  ReadLn;
end.
