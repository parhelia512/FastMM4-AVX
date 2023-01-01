program project1;

{$mode objfpc}{$H+}
{$APPTYPE CONSOLE}

uses
  cthreads,
  FastMM4,
  Classes, SysUtils;
  { you can add units after this }

procedure RunBenchmark;
const
  Prime = 10513;
var
  sz, i, LPointerNumber, FIterCount, NumPointers, BlockSizeDelta, MaxBlockSize, MinBlockSize: Integer;
  CurValue: Int64;
  P: PAnsiChar;

type
  PReallocMemBenchmarkBlockSizesArray = ^TReallocMemBenchmarkBlockSizesArray;
  PReallocMemBenchmarkPointerArray    = ^TReallocMemBenchmarkPointerArray;

  TReallocMemBenchmarkPointerArray          =  packed array[0..MaxInt div SizeOf(Pointer)-1] of Pointer;
  TReallocMemBenchmarkBlockSizesArray       =  packed array[0..MaxInt div SizeOf(Pointer)-1] of Integer;

var
    BlockSizes: PReallocMemBenchmarkBlockSizesArray;
    Pointers: PReallocMemBenchmarkPointerArray;

begin
  MaxBlockSize := 0;
  MinBlockSize := MaxInt;
  CurValue := Prime;
  NumPointers := 20011;
  BlockSizeDelta := 257;

  GetMem(Pointers, NumPointers*SizeOf(Pointer));
  {Clear all the pointers}
  FillChar(Pointers^, NumPointers*SizeOf(Pointer), 0);

  GetMem(BlockSizes, NumPointers*SizeOf(Integer));
  {Clear all the block sizes}
  FillChar(BlockSizes^, NumPointers*SizeOf(Integer), 0);


  {Do the benchmark}
  FIterCount := 50000*1000;
  for i := 1 to FIterCount do
  begin
    {Get an arbitarry pointer number}
    LPointerNumber := CurValue mod NumPointers;
    Inc(CurValue, Prime);
    {Adjust the current block size up or down by up to BlockSizeDelta}
    BlockSizes^[LPointerNumber] := abs(BlockSizes^[LPointerNumber] + (CurValue mod BlockSizeDelta) - (BlockSizeDelta shr 1) - ((i and 7)mod BlockSizeDelta));
    Inc(CurValue, Prime);
    {Reallocate the pointer}
    if MaxBlockSize < BlockSizes^[LPointerNumber] then MaxBlockSize := BlockSizes^[LPointerNumber];
    if (BlockSizes^[LPointerNumber] > 0) and (MinBlockSize > BlockSizes^[LPointerNumber]) then MinBlockSize := BlockSizes^[LPointerNumber];

    System.ReallocMem(Pointers^[LPointerNumber], BlockSizes^[LPointerNumber]);
    {Touch the memory}
    sz := BlockSizes^[LPointerNumber];
    if sz > 0 then
    begin
      P := Pointers^[LPointerNumber];
      P[0] := #1;
      if sz > 1 then
      begin
        P[sz-1] := #2;
      end;
    end;
  end;
  {What we end with should be close to the peak usage}
end;


type
  TBenchmarkThread = class(TThread)
    procedure Execute; override;
  end;

procedure TBenchmarkThread.Execute;
begin
  RunBenchmark;
end;

const
  CNumThreads = 12;
var
  A: array[0..1000] of Pointer;
  i: Integer;
  w: Word;
  LThreads: array[0..CNumThreads-1] of TBenchmarkThread;
begin
  w := GetFastMMCpuFeatures;
  for i := Low(A) to High(A) do
    GetMem(A[i], i+1);
  for i := Low(A) to High(A) do
    FreeMem(A[i], i+1);
//  WriteLn(w);
  if (W and $100) <> 0 then
  begin
    WriteLn('Wait PKG is supported');
    WriteLn('Smallest monitor line size', FastMMCpuSmallestMonitorLineSize);
    WriteLn('Largest monitor line size', FastMMCpuLargestMonitorLineSize);

  end else
  begin
    WriteLn('Wait PKG is not supported')
  end;
  for i := Low(LThreads) to High(LThreads) do
  begin
    LThreads[i] := TBenchmarkThread.Create(True);
  end;
  for i := Low(LThreads) to High(LThreads) do
  begin
    LThreads[i].Resume;
  end;
  for i := Low(LThreads) to High(LThreads) do
  begin
    LThreads[i].WaitFor;
  end;
  for i := Low(LThreads) to High(LThreads) do
  begin
    LThreads[i].Destroy;
    LThreads[i] := nil;
  end;
end.

