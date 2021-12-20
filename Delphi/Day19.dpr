program Day19;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Lib in 'Lib.pas',
  Windows,
  System.Math.Vectors,
  Math,
  SysUtils,
  Vcl.Graphics,
  System.Generics.Defaults,
  System.Generics.Collections,
  System.Diagnostics;

type
  BigInt = Int64;
  BestInt = NativeInt;
  Int = BestInt;

  TBeacon = record
    Pos: TPoint3D;
  end;
  TBeaconArray = TArray<TBeacon>;
  TScanner = record
    Nr: Integer;
    Pos: TPoint3D;
    Beacons: TBeaconArray;
  end;
  TScannerArray = TArray<TScanner>;

procedure WritePoint(const p: TPoint3D);
begin
  Write('(X:',Trunc(p.X),',Y:',Trunc(p.Y),',Z:',Trunc(p.Z),')');
end;

function DoesOverlap(const A, B: TBeaconArray; const MinCount: BigInt; out Offset: TPoint3d): Boolean;
begin
  for var ai := Low(A) to High(A) do
    for var bi := Low(B) to High(B) do
    begin
      Offset := B[bi].Pos - A[ai].Pos;

      WritePoint(Offset); WriteLn;

      var OverlapCount := 0;

      for var at := Low(A) to High(A) do
        for var bt := Low(B) to High(B) do
        begin
          Write('Comparing A ');
          WritePoint(A[at].Pos);
          Write(' offset to ');
          WritePoint(A[at].Pos + Offset);
          Write(' to B ');
          WritePoint(B[bt].Pos);
          if (A[at].Pos + Offset) = B[bt].Pos then
          begin
            Inc(OverlapCount);
            Write(' MATCH ');
          end;
          WriteLn;
        end;

      if OverlapCount >= MinCount then
        Exit(True);
    end;

  Exit(False);
end;

function ReadScanners(const Inputs: TStringArray): TScannerArray;
begin
  var Beacon: TBeacon;
  SetLength(Result, 0);
  var Scanner: TScanner;
  Scanner.Nr := 0;
  for var Input in Inputs do
  begin
    if Input.StartsWith('---') then
    else if Input = '' then
    begin
      SetLength(Result, Length(Result)+1);
      Result[High(Result)] := Scanner;
      SetLength(Scanner.Beacons, 0);
      Inc(Scanner.Nr);
    end
    else
    begin
      var nrs := Input.Split([',']);
      Beacon.Pos := TPoint3D.Zero;
      Beacon.Pos.X := nrs[0].ToInteger;
      Beacon.Pos.Y := nrs[1].ToInteger;
      if Length(nrs) > 2 then
        Beacon.Pos.Z := nrs[2].ToInteger;
      SetLength(Scanner.Beacons, Length(Scanner.Beacons)+1);
      Scanner.Beacons[High(Scanner.Beacons)] := Beacon;
    end;
  end;
  SetLength(Result, Length(Result)+1);
  Result[High(Result)] := Scanner;
end;

function GetBeaconCount(Inputs: TStringArray): BigInt;
begin
  //
end;

procedure TestOverlap2d;
begin
  var Scanners := ReadScanners([
    '--- scanner 0 ---',
    '0,2',
    '4,1',
    '3,3',
    '',
    '--- scanner 1 ---',
    '-1,-1',
    '-5,0',
    '-2,1']);

  WriteLn('sc ', Length(Scanners));
  WriteLn('bc ', Length(Scanners[0].Beacons));

  var Offset: TPoint3D;
  if DoesOverlap(Scanners[0].Beacons, Scanners[1].Beacons, 3, Offset) then
  begin
    WritePoint(Offset); WriteLn;
  end
  else
    WriteLn('No overlap');
end;

procedure FindChain(const Input: TStringArray);
begin

end;

begin
  WriteLn('Test');

  TestOverlap2D;

  var TestInput := Lib.LoadStrings('Day19.Test.txt');

  WriteLn('Test 1');
  ValidateNr(GetBeaconCount(TestInput), 79);

  WriteLn(#10'Hit it');
  ReadLn;
end.
