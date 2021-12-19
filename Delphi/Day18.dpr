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
  Snailfish.Number.Intf in 'Snailfish.Number.Intf.pas',
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

procedure TestExplodeOnce(const s, Expected: String);
begin
  var r := '';
  var t := TTokenUtils.Tokenize(s);
  if TTokenUtils.ExplodeOnce(t) then
    r := TTokenUtils.Serialize(t);
  Validate(r, Expected);
end;

procedure TestReduce(const s, Expected: String);
begin
  var r := '';
  var t := TTokenUtils.Tokenize(s);
  while TTokenUtils.ReduceOnce(t) do
    ;
  r := TTokenUtils.Serialize(t);
  Validate(r, Expected);
end;

procedure TestSplitOnce(const s, Expected: String);
begin
  var r := '';
  var t := TTokenUtils.Tokenize(s);
  if TTokenUtils.SplitOnce(t) then
    r := TTokenUtils.Serialize(t);
  Validate(r, Expected);
end;

procedure TestAdd(const A, B, Expected: String; const Reductions: TArray<String>);
begin
  var At := TTokenUtils.Tokenize(A);
  var Bt := TTokenUtils.Tokenize(B);
  var E := TTokenUtils.Add(At, Bt);
  Validate(TTokenUtils.Serialize(E), Expected);
  for var Reduction in Reductions do
  begin
    TTokenUtils.ReduceOnce(E);
    Validate(TTokenUtils.Serialize(E), Reduction);
  end;
end;

procedure TestSum(const List: TArray<String>; const Expected: String);
begin
  var Number := ReadNumber(List[0]);
  for var i := 1 to High(List) do
    Number.Add(ReadNumber(List[i]));
  Validate(Number.ToString, Expected);
end;

function CheckSum(const List: TArray<String>): BigInt;
begin
  var Number := ReadNumber(List[0]);
  for var i := 1 to High(List) do
    Number.Add(ReadNumber(List[i]));

  Result := Number.Magnitude;
end;

begin
  WriteLn('Test reading');

  WriteLn('Reading');
  TestRead('[1,2]');
  TestRead('[[1,2],3]');
  TestRead('[9,[8,7]]');
  TestRead('[[1,9],[8,5]]');
  TestRead('[[[[1,2],[3,4]],[[5,6],[7,8]]],9]');
  TestRead('[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]');
  TestRead('[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]');

  WriteLn('Magnitude');
  TestMagnitude('[[1,2],[[3,4],5]]', 143);
  TestMagnitude('[[[[0,7],4],[[7,8],[6,0]]],[8,1]]', 1384);
  TestMagnitude('[[[[1,1],[2,2]],[3,3]],[4,4]]', 445);
  TestMagnitude('[[[[3,0],[5,3]],[4,4]],[5,5]]', 791);
  TestMagnitude('[[[[5,0],[7,4]],[5,5]],[6,6]]', 1137);
  TestMagnitude('[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]', 3488);

  WriteLn('Explode (single step)');
  TestExplodeOnce('[[[[0,9],2],3],4]', ''); // Test boolean response
  TestExplodeOnce('[[[[[9,8],1],2],3],4]', '[[[[0,9],2],3],4]');
  TestExplodeOnce('[7,[6,[5,[4,[3,2]]]]]', '[7,[6,[5,[7,0]]]]');
  TestExplodeOnce('[[6,[5,[4,[3,2]]]],1]', '[[6,[5,[7,0]]],3]');
  TestExplodeOnce('[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]', '[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]');
  TestExplodeOnce('[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]', '[[3,[2,[8,0]]],[9,[5,[7,0]]]]');

  TestSplitOnce('[0,1]', ''); // Test boolean response
  TestSplitOnce('[0,10]', '[0,[5,5]]');
  TestSplitOnce('[11,12]', '[[5,6],12]');

  WriteLn('Reducing');
  TestReduce('[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]', '[[3,[2,[8,0]]],[9,[5,[7,0]]]]');

  WriteLn('Adding');
  TestAdd(
    '[[[[4,3],4],4],[7,[[8,4],9]]]','[1,1]', // A = B
    '[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]', // = E
    [ '[[[[0,7],4],[7,[[8,4],9]]],[1,1]]', // Reductions
      '[[[[0,7],4],[15,[0,13]]],[1,1]]',
      '[[[[0,7],4],[[7,8],[0,13]]],[1,1]]',
      '[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]',
      '[[[[0,7],4],[[7,8],[6,0]]],[8,1]]' ]);


  WriteLn('Summing lists');
  TestSum(['[1,1]', '[2,2]', '[3,3]', '[4,4]'], '[[[[1,1],[2,2]],[3,3]],[4,4]]');
  TestSum(['[1,1]', '[2,2]', '[3,3]', '[4,4]', '[5,5]'], '[[[[3,0],[5,3]],[4,4]],[5,5]]');
  TestSum(['[1,1]', '[2,2]', '[3,3]', '[4,4]', '[5,5]', '[6,6]'], '[[[[5,0],[7,4]],[5,5]],[6,6]]');
  TestSum([ '[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]',
            '[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]',
            '[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]',
            '[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]',
            '[7,[5,[[3,8],[1,4]]]]',
            '[[2,[2,2]],[8,[8,1]]]',
            '[2,9]',
            '[1,[[[9,3],9],[[9,0],[0,7]]]]',
            '[[[5,[7,4]],7],1]',
            '[[[[4,2],2],6],[8,7]]'],
          '[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]');


  WriteLn;
  WriteLn('Part 1 test');
  ValidateNr(CheckSum(Lib.LoadStrings('Day18.Test.txt')), 4140);
  WriteLn('Part 1 answer');
  ValidateNr(CheckSum(Lib.LoadStrings('Day18.Input.txt')), 4176);


  WriteLn(#10'Hit it');
  ReadLn;
end.
