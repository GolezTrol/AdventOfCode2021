program Day18;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Lib in 'Lib.pas',
  Windows,
  Math,
  SysUtils,
  Vcl.Graphics,
  System.Character,
  System.Generics.Defaults,
  System.Generics.Collections,
  System.Diagnostics,
  Snailfish.Number in 'Snailfish.Number.pas';


procedure TestRead(const Expected: String);
begin
  var n := ReadNumber(Expected);
  Validate(n.ToString, Expected);
end;

procedure TestMagnitude(const s: String; const Expected: BigInt);
begin
  var n := ReadNumber(s);
  ValidateNr(n.Magnitude, Expected);
end;

procedure TestExplode(const s, Expected: String);
begin
  var n := ReadNumber(s);
  Validate(n.ToString, Expected);
end;

begin
  WriteLn('Test reading');

  TestRead('[1,2]');
  TestRead('[[1,2],3]');
  TestRead('[9,[8,7]]');
  TestRead('[[1,9],[8,5]]');
  TestRead('[[[[1,2],[3,4]],[[5,6],[7,8]]],9]');
  TestRead('[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]');
  TestRead('[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]');

  TestMagnitude('[[1,2],[[3,4],5]]', 143);
  TestMagnitude('[[[[0,7],4],[[7,8],[6,0]]],[8,1]]', 1384);
  TestMagnitude('[[[[1,1],[2,2]],[3,3]],[4,4]]', 445);
  TestMagnitude('[[[[3,0],[5,3]],[4,4]],[5,5]]', 791);
  TestMagnitude('[[[[5,0],[7,4]],[5,5]],[6,6]]', 1137);
  TestMagnitude('[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]', 3488);

  TestExplode('[[[[[9,8],1],2],3],4]', '[[[[0,9],2],3],4]');
  TestExplode('[7,[6,[5,[4,[3,2]]]]]', '[7,[6,[5,[7,0]]]]');
  TestExplode('[[6,[5,[4,[3,2]]]],1]', '[[6,[5,[7,0]]],3]');
  TestExplode('[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]', '[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]');
  TestExplode('[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]', '[[3,[2,[8,0]]],[9,[5,[7,0]]]]');

  WriteLn(#10'Hit it');
  ReadLn;
end.
