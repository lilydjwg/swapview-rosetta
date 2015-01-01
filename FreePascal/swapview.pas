Program swapview;
{$mode objfpc}
{I+}
Uses sysutils;

Type
	SwapInfo = Record
		pid  : Integer;
		size : Real;
		comm : AnsiString
		end;
	ArraySwapInfo = array of SwapInfo;
	ArrayString = array of AnsiString;


Function filesize(size : Real): AnsiString;
Var units : AnsiString;
	left : Real;
	u : Integer;
Begin
	units := 'KMGT';
	left := abs(size);
	u := 0;
	While (left > 1100) And (u < 4) Do Begin
		left := left / 1024;
		Inc(u);
	End;
	If u = 0 Then
		filesize := Format('%DB', [round(size)])
	Else Begin
		If size < 0 Then
			left := - left;
		filesize := Format('%.1F%SiB', [left, units[u]]);
	End;
End;


Function ReadAll(path: AnsiString): AnsiString;
Var c: char;
	ret: AnsiString;
	f: Text;
Begin
	ret := '';
	Assign(f, path);
	Reset(f);
	While Not Eof(f) do
	Begin
		Read(f, c);
		ret := ret + c;
	End;
	ReadAll := ret
End;

Function ReadLines(path: AnsiString): ArrayString;
Var c: AnsiString;
	f: Text;
	ret: ArrayString = NIL;
Begin
	Assign(f, path);
	Reset(f);
	While Not Eof(f) do
	Begin
		ReadLn(f, c);
		SetLength(ret, Length(ret) + 1);
		ret[Length(ret)-1] := c;
	End;
	ReadLines := ret
End;

Function NoisyStrToInt(str : AnsiString): Integer;
Var ret: Integer = 0;
	i : Integer;
Begin
	For i:= 1 To Length(str) Do
		If (str[i] >= '0') And (str[i] <= '9') Then 
		Begin
			ret := ret * 10 ;
			ret := ret + ord(str[i]) - ord('0')
		End;
	NoisyStrToInt := ret
End;


Function getSwapFor(pid : Integer): SwapInfo;
Var comm : AnsiString;
	s : Real;
	lines : ArrayString;
	i : Integer;
Begin
	Try
		comm := ReadAll(Format('/proc/%D/cmdline', [pid]));
		For i := 1 To Length(comm) Do
			If comm[i] = #0 Then comm[i] := #32;
		If Length(comm) > 0 Then
			comm := LeftStr(comm, Length(comm) - 1);
		lines := ReadLines(Format('/proc/%D/smaps', [pid]));
		s := 0 ;
		For i:= 0 to Length(lines) - 1 Do
			if CompareStr(LeftStr(Lines[i], 5), 'Swap:') = 0 Then
				s := s + NoisyStrToInt(Lines[i]);
		getSwapFor.pid := pid;
		getSwapFor.comm := comm;
		getSwapFor.size := s*1024;
	Except
		On E:EInOutError do
		Begin
			getSwapFor.pid := pid;
			getSwapFor.comm := '';
			getSwapFor.size := 0;
		End
	End
End;

Function readdir(path : String): ArrayString;
Var info : TSearchRec;
	ret : ArrayString = NIL;
Begin
If FindFirst (path+'/*', faDirectory, info)=0 then
begin
	Repeat
		With Info do
		begin
			SetLength(ret, Length(ret) + 1);
			ret[Length(ret)-1] := name;
		end;
	Until FindNext(info)<>0;
end;
FindClose(Info);
readdir := ret
End;

Procedure bubblesort(var items : ArraySwapInfo);
Var i,j,l : Integer;
	t : SwapInfo;
Begin
	l := Length(items) - 1;
	for i := 0 to l do
		for j := i+1 to l do
			If items[i].size > items[j].size Then
			Begin
				t := items[i];
				items[i] := items[j];
				items[j] := t
			End
End;

Function getSwap: ArraySwapInfo;
Var dirs : ArrayString ;
	i, pid : Integer;
	ret : ArraySwapInfo = NIL;
	swap : SwapInfo;
Begin
	dirs := readdir('/proc');
	For i:=0 To Length(dirs) - 1 Do 
	Begin
		pid := NoisyStrToInt(dirs[i]);
		If pid > 0 Then 
		Begin
			swap := getSwapFor(pid);
			if swap.size > 0.0 Then
			Begin
				SetLength(ret, Length(ret)+1);
				ret[Length(ret)-1] := swap
			End
		End
	End;
	bubblesort(ret);
	getSwap := ret
End;


Var
	results : ArraySwapInfo;
	i : Integer;
	t : Real = 0;
Begin
	results := getSwap();
	WriteLn(Format('%5S %9S %S', ['PID', 'SWAP', 'COMMAND']));
	For i := 0 To Length(results) - 1 Do
	Begin
		t := t + results[i].size;
		WriteLn(Format('%5D %9S %S', [results[i].pid, filesize(results[i].size), results[i].comm]));
	End;
	WriteLn(Format('Total: %8S', [filesize(t)]));
End.
