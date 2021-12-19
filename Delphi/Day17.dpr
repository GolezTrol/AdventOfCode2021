program Day17;

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

  TTarget = TRect;

function ReadTarget(const Input: String): TRect;
begin
  var Numbers := Input
    .Replace('target area: x=', '')
    .Replace(', y=', '..')
    .Split(['..']);
  Result := TRect.Create(
    Numbers[0].ToInteger, // Left
    Numbers[3].ToInteger, // Top
    Numbers[1].ToInteger, // Right
    Numbers[2].ToInteger); // Bottom
end;

type
  TExit = (
    No, // No exit, keep plotting trajectory
    Hit, // Direct hit, success
    Missed, // The shot missed, undetermined if this is final
    Stalled, // The shot stalled, X velocity became 0.
    Undershot, // Definitely under (not really relevant, maybe?)
    Overshot // Definitely over. No use incrementing StartY for this StartX.
  );
function Highest(const Input: String): BigInt;
begin
  var Target := ReadTarget(Input);

  WriteLn('Target: X=', Target.Left, ' to ', Target.Right, ', Y=', Target.Top, ' to ', Target.Bottom);
  var MaxXVelocity := Target.Right+1;
  var MinYVelocity := Target.Bottom-1;
  var MaxYVelocity := 0 - Target.Bottom + 2;

  var Exit: TExit;
  var TotalMaxY: BigInt := 0;

  for var StartXv := 1 to MaxXVelocity do
  begin
    for var StartYv := MinYVelocity to MaxYVelocity do
    begin
      var Step := 0;
      var X := 0;
      var Y: BigInt := 0;
      var Xv := StartXv;
      var Yv: BigInt := StartYv;
      var MaxY: BigInt := 0;
      Exit := TExit.No;
      repeat // Step
        Inc(X, Xv);
        Inc(Y, Yv);
        Dec(Xv, Sign(Xv)); // Drag towards 0
        Dec(Yv, 1); // Gravity
        MaxY := Max(Y, MaxY);

        Inc(Step);

        // Determine exit conditions
        if (X > Target.Right) and (Y > Target.Top) then // Passed over
          Exit := TExit.Overshot
        else if (Xv <= 0) and (X < Target.Left) and (Yv >= 0) then // Will fall before
          Exit := TExit.Stalled
        else if (X < Target.Left) and (Y < Target.Bottom) then // Passed under
          Exit := TExit.Undershot
        else if (Y < Target.Bottom) then // It is under. Undetermined how
          Exit := TExit.Missed
        else if (X > Target.Right) then // It is beyond. Undetermined how
          Exit := TExit.Missed
        else if (X >= Target.Left) and (X <= Target.Right) and (Y >= Target.Bottom) and (Y <= Target.Top) then
          Exit := TExit.Hit;

      until Exit <> TExit.No;

      if Exit in [Overshot, Stalled] then
      begin
        Write(
          'Stopped Xv ', StartXv, ' after Yv ', StartYv,
          '(',X,',',Y,') ');
      end;

      if Exit = TExit.Overshot then // Too hard
      begin
        WriteLn('Too hard');
        Break;
      end;
      if Exit = TExit.Stalled then // Too steep
      begin
        WriteLn('Too steep');
        Break;
      end;

      if Exit = TExit.Undershot then // Too soft
        ; // Ignore this Yv, but continue evaluating this Xv.
      if Exit = TExit.Missed then
        ; // Ignore this Yv, but continue evaluating this Xv.

      if Exit = TExit.Hit then
      begin
        TotalMaxY := Max(TotalMaxY, MaxY);
        WriteLn('Hit! Xv:', StartXv, ', Yv:', StartYv, '. MaxY:', MaxY, '. Steps:', Step);
      end;
    end;
  end;
  Result := TotalMaxY;
end;

begin
  WriteLn('Part1, test');
  ValidateNr(Highest('target area: x=20..30, y=-10..-5'), 45);
  WriteLn('Part1, final');
  ValidateNr(Highest('target area: x=269..292, y=-68..-44'), 0);

  //WriteLn('Part2, test');
  //ValidateNr(Solve2('target area: x=20..30, y=-10..-5'), 0);
  //WriteLn('Part, final');
  //ValidateNr(Solve2('target area: x=269..292, y=-68..-44'), 0);

  WriteLn(#10'Hit it');
  ReadLn;
end.
