program Day4;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Lib in '..\Lib.pas',
  Vcl.Graphics,
  Classes,
  SysUtils;


type
  TCell = record
    Number: Byte;
    Called: Boolean;
  end;
  TCard = record
    Cells: array[0..4, 0..4] of TCell;
    CalledRows: array[0..4] of Boolean;
    CalledCols: array[0..4] of Boolean;
  end;
  PCard = ^TCard;
  TBingo = record
    Index: Integer;
    Numbers: TIntArray;
    Cards: TArray<TCard>;
    Winners: TIntArray;
  end;

function LoadBingo(Input: TStringArray): TBingo;
begin
  Result.Index := -1;

  var s := TStringList.Create;
  s.CommaText := Input[0];
  Result.Numbers := ToIntArray(s);

  SetLength(Result.Cards, Length(Input) div 6);
  for var Card := Low(Result.Cards) to High(Result.Cards) do
  begin
    for var Row := 0 to 4 do
      for var Col := 0 to 4 do
        Result.Cards[Card].Cells[Row][Col].Number := Copy(Input[2+Card*6+Row], Col*3+1, 2).Trim.ToInteger;
  end;
end;

procedure Draw(var Bingo: TBingo);
begin
  Inc(Bingo.Index);
  var Number := Bingo.Numbers[Bingo.Index];

  for var c := Low(Bingo.Cards) to High(Bingo.Cards) do
  begin
    var Card: PCard := @Bingo.Cards[c];
    for var Row := 0 to 4 do
      for var Col := 0 to 4 do
        if Card.Cells[Row][Col].Number = Number then
        begin
          Card.Cells[Row][Col].Called := True;
          Assert(Card.CalledRows[Row] = False, 'Row not called yet');
          Assert(Card.CalledCols[Col] = False, 'Col not called yet');

          var RowCheck := 0;
          var ColCheck := 0;
          for var i := 0 to 4 do
            begin
              Inc(RowCheck, Card.Cells[i][Col].Called.ToInteger);
              Inc(ColCheck, Card.Cells[Row][i].Called.ToInteger);
            end;
          if RowCheck = 5 then
            Card.CalledRows[Row] := True;
          if ColCheck = 5 then
            Card.CalledCols[Col] := True;

          if Card.CalledRows[Row] or Card.CalledCols[Col] then
          begin
            SetLength(Bingo.Winners, Length(Bingo.Winners) + 1);
            Bingo.Winners[High(Bingo.Winners)] := c;

            Break;
          end;
        end;
  end;
end;

function SumUnmarked(const Card: TCard): Integer;
begin
  Result := 0;
  for var Row := 0 to 4 do
    for var Col := 0 to 4 do
      if not Card.Cells[Row][Col].Called then
        Inc(Result, Card.Cells[Row][Col].Number);
end;

function Day4_1(Raw: TStringArray): String;
begin
  var Bingo := LoadBingo(Raw);

  repeat
    Draw(Bingo);
  until Length(Bingo.Winners) > 0;

  Assert(Length(Bingo.Winners) = 1, 'Exactly 1 winner');

  var Number := Bingo.Numbers[Bingo.Index];
  Result := (Number * SumUnmarked(Bingo.Cards[Bingo.Winners[0]])).ToString;
end;

function Day4_2(Raw: TStringArray): String;
begin
end;

var
  Result: String;
begin
  WriteLn('Tests');
  var Input := LoadStrings('Day4.test.txt');
  Result := Day4_1(Input);
  Validate(Result, '4512');
  Result := Day4_2(Input);
  Validate(Result, '');

  WriteLn(#10'Final');
  Input := LoadStrings('Day4.input.txt');
  Result := Day4_1(Input);
  Validate(Result, '44736');
  Result := Day4_2(Input);
  Validate(Result, '');

  WriteLn(#10'Hit it');
  ReadLn;
end.
